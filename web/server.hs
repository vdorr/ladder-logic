{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns -fno-warn-tabs #-}
{-# LANGUAGE CPP, OverloadedStrings, DeriveGeneric, RecordWildCards, TupleSections,
	BangPatterns, ScopedTypeVariables, LambdaCase, FlexibleContexts, TypeSynonymInstances #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

import Network.Wai --(responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types -- (status200)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as B
import Data.Text.Encoding as E

import Compile

--------------------------------------------------------------------------------

eeek :: B.ByteString -> IO ResponseData
eeek s = do
	putStrLn $ T.unpack $ E.decodeUtf8 s
	undefined

--------------------------------------------------------------------------------

headers =
	[ (hContentType, "text/plain")
	, ("Access-Control-Allow-Origin", "*")
	]

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req f = do

	print ("hello", remoteHost req, pathInfo req) -- , queryString req)

	case (pathInfo req, queryString req) of
		(["compile"], [("src", Just src)]) -> do
-- 			print (here, src)
			r <- eeek src
			f $ responseLBS status200 headers $ encode r 
		_ -> do
--404 actually
			f $ responseLBS status404 headers "" --"console.log(\"Hello world!\");"

--	print (here, encode $ ResponseData Nothing ":-)")

data Tag = Tag
	{ tag :: T.Text
	, value :: T.Text
	, tag_kind :: T.Text --state(/in/out/mem)/node
	}
	deriving (Show, Generic)

instance ToJSON Tag

data ResponseData = ResponseData
	{ failure :: !(Maybe T.Text)
	, tags :: ![Tag]
	, artifact :: !T.Text
	}
	deriving (Show, Generic)

instance ToJSON ResponseData
--instance FromJSON ResponseData


--------------------------------------------------------------------------------

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

