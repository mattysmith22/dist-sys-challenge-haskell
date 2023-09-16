{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Maelstrom
    ( Node(..)
    , MessageId(..)
    , MaelstromState(..)
    , Message(..)
    -- * Message types
    , InitRequest(..)
    , InitOkResponse(..)
    , ErrorResponse(..)

    , ensureMessageType

    , Handler
    , makeEndpoint
    , sendMessage
    , sendMessage'
    , maelstromMain
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Bimap as Bimap
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Control.Monad.Catch (SomeException, catch)
import Control.Monad.State
import Control.Applicative
import System.IO

newtype Node = Node { unNode :: Text }
    deriving (Show, Eq, Ord)

instance FromJSON Node where
    parseJSON = fmap Node <$> parseJSON
instance ToJSON Node where
    toJSON = toJSON . unNode

data MessageId = MessageId { unMessageId :: Integer }
    deriving (Show, Eq, Ord)

instance FromJSON MessageId where
    parseJSON = fmap MessageId <$> parseJSON
instance ToJSON MessageId where
    toJSON = toJSON . unMessageId

data MaelstromState a = MaelstromState
    { maelHostId :: Node
    , maelNodeIds :: [Node]
    , maelExtra :: a
    , maelNextMsgId :: Integer
    }

data Message = Message
    { msgSource :: Node
    , msgDest :: Node
    , msgBody :: Value
    }
    deriving (Show, Eq)

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> do
        msgSource <- o .: "src"
        msgDest <- o .: "dest"
        msgBody <- o .: "body"
        return Message{..}

instance ToJSON Message where
    toJSON Message{..} = Object
        ( "src" .= msgSource
        <> "dest" .= msgDest
        <> "body" .= msgBody
        )

data InitRequest = InitRequest
    { initMsgId :: MessageId
    , initNodeId :: Node
    , initNodeIds :: [Node]
    }
    deriving (Show, Eq)

instance FromJSON InitRequest where
    parseJSON = withObject "InitRequest" $ \o -> do
        ensureMessageType "init" o
        initMsgId <- o .: "msg_id"
        initNodeId <- o .: "node_id"
        initNodeIds <- o .: "node_ids"
        return InitRequest{..}

newtype InitOkResponse = InitOkResponse
    { initOkReplyTo :: MessageId
    }
    deriving (Show, Eq)

instance ToJSON InitOkResponse where
    toJSON InitOkResponse{..} = Object
        ( "type" .= ("init_ok"::Text)
        <> "in_reply_to" .= initOkReplyTo)

data ErrorCode
    = ErrorTimeout
    | ErrorNodeNotFound
    | ErrorNotSupported
    | ErrorTemporarilyUnavailable
    | ErrorMalformedRequest
    | ErrorCrash
    | ErrorAbort
    | ErrorKeyDoesNotExist
    | ErrorKeyAlreadyExists
    | ErrorPreconditionFailed
    | ErrorTxnConflict
    deriving (Show, Eq, Ord)

errorCodeMapping :: Bimap.Bimap ErrorCode Int
errorCodeMapping = Bimap.fromList
    [ (ErrorTimeout, 0)
    , (ErrorNodeNotFound, 1)
    , (ErrorNotSupported, 10)
    , (ErrorTemporarilyUnavailable, 11)
    , (ErrorMalformedRequest, 12)
    , (ErrorCrash, 13)
    , (ErrorAbort, 14)
    , (ErrorKeyDoesNotExist, 20)
    , (ErrorKeyAlreadyExists, 21)
    , (ErrorPreconditionFailed, 22)
    , (ErrorTxnConflict, 30)
    ]

instance FromJSON ErrorCode where
    parseJSON x = parseJSON x >>= (maybe (fail "Error code not found") return . flip Bimap.lookupR errorCodeMapping)
instance ToJSON ErrorCode where
    toJSON = toJSON . fromMaybe 13 . flip Bimap.lookup errorCodeMapping

data ErrorResponse = ErrorResponse
    { errorReplyTo :: MessageId
    , errorCode :: ErrorCode
    , errorMessage :: Text
    }
    deriving (Show, Eq)
    
instance ToJSON ErrorResponse where
    toJSON ErrorResponse{..} = Object
        ( "type" .= ("error"::Text)
        <> "in_reply_to" .= errorReplyTo
        <> "code" .= errorCode
        <> "text" .= errorMessage)

ensureMessageType :: Text -> Object -> Parser ()
ensureMessageType expectedType o = do
    messageType <- o .: "type"
    unless (messageType == expectedType) $ fail $ T.unpack ("Expected type " <> expectedType)

type Handler state a
    = StateT (MaelstromState state) IO a

type Endpoint state
    = Value -> Parser (Node -> Handler state ())

repeatedly
    :: Monad m 
    => (a -> m a)
    -> a
    -> m b
repeatedly f x = f x >>= repeatedly f

readMessage :: IO Message
readMessage = (either error id . eitherDecode . BS.fromStrict) <$> BS.getLine

newMessageId :: Handler state MessageId
newMessageId = do
    i <- gets (MessageId . maelNextMsgId)
    modify (\x -> x {maelNextMsgId = succ $ maelNextMsgId x})
    return i
    
sendMessage'' :: Message -> IO ()
sendMessage'' m
    = BS.putStrLn (BS.toStrict $ encode m)
    >> hFlush stdout

sendMessage' :: ToJSON a => Node -> a -> Handler state ()
sendMessage' msgDest body = do
    msgSource <- gets maelHostId
    let msgBody = toJSON body
    lift $ sendMessage'' Message{..}

sendMessage :: ToJSON a => Node -> (MessageId -> a) -> Handler state ()
sendMessage n f = do
    msgId <- newMessageId
    sendMessage' n (f msgId)

data MessageIdOnly = MessageIdOnly
    { msgId :: MessageId }
    deriving (Show, Eq)

instance FromJSON MessageIdOnly where
    parseJSON = fmap (fmap MessageIdOnly) $ withObject "MessageIdOnly"
        $ \o -> o .: "msg_id"

makeEndpoint :: FromJSON message
    => (message -> Node -> Handler state b)
    -> Value
    -> Parser (Node -> Handler state b)
makeEndpoint handler = fmap handler <$> parseJSON

maelstromMain
    :: IO state -- ^ Function to init
    -> [Endpoint state]-- ^ Endpoint implementations
    -> IO ()
maelstromMain serverInit endpoints = repeatedly
    (\case
        Nothing -> do
            msg <- readMessage 
            case parseMaybe parseJSON (msgBody msg) of
                (Just InitRequest{..}) -> do
                    let initServer maelExtra = 
                            let maelHostId = initNodeId
                                maelNodeIds = initNodeIds
                                maelNextMsgId = 0
                                
                                s = MaelstromState{..}
                            in fmap Just $ execStateT
                                (sendMessage' (msgSource msg) InitOkResponse
                                    { initOkReplyTo = initMsgId }) s
                    let exceptionHandler e = do
                            sendMessage'' Message
                                { msgBody = toJSON ErrorResponse
                                    { errorReplyTo = initMsgId
                                    , errorCode = ErrorCrash
                                    , errorMessage = T.pack $ show (e::SomeException)
                                    }
                                , msgSource = msgDest msg
                                , msgDest = msgSource msg
                                }
                            return Nothing
                    catch (serverInit >>= initServer) exceptionHandler
                Nothing -> return Nothing
        (Just maelState) -> do
            let handleUnknownMessage = \MessageIdOnly{..} sender -> sendMessage' sender ErrorResponse
                    { errorReplyTo = msgId
                    , errorCode = ErrorNotSupported
                    , errorMessage = "Could not find a handler which matched message type"
                    }
            
            msg <- readMessage
            case parseMaybe (\x -> asum (fmap ($ x) endpoints) <|> makeEndpoint handleUnknownMessage x) (msgBody msg) of
                (Just x) -> Just <$> execStateT (x $ msgSource msg) maelState
                Nothing -> pure $ Just maelState)
    Nothing