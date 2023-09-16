{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Maelstrom
import Data.Aeson
import Data.Text (Text)

data EchoRequest = EchoRequest
    { echoMsgId :: MessageId
    , echo :: Text
    }
    deriving (Eq, Show)

instance FromJSON EchoRequest where
    parseJSON = withObject "EchoRequest" $ \o -> do
        ensureMessageType "echo" o
        echoMsgId <- o .: "msg_id"
        echo <- o .: "echo"
        return EchoRequest{..}

data EchoOkResponse = EchoOkResponse
    { echoRespMsgId :: MessageId
    , echoRespInReply :: MessageId
    , echoResp :: Text
    }
    deriving (Eq, Show)

instance ToJSON EchoOkResponse where
    toJSON EchoOkResponse{..} = Object
        ( "type" .= ("echo_ok" :: Text)
        <> "msg_id" .= echoRespMsgId
        <> "in_reply_to" .= echoRespInReply
        <> "echo" .= echoResp
        )

handleEcho :: EchoRequest -> Node -> Handler () ()
handleEcho EchoRequest{..} sender = let
        echoRespInReply = echoMsgId
        echoResp = echo
    in sendMessage sender $ \echoRespMsgId -> EchoOkResponse{..}

main :: IO ()
main = maelstromMain
    (return ())
    (makeHandler handleEcho)
