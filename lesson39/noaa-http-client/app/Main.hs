{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (
    Request,
    defaultRequest,
    getResponseBody,
    getResponseStatus,
    httpLBS,
    setRequestHeader,
    setRequestHost,
    setRequestMethod,
    setRequestPath,
    setRequestPort,
    setRequestSecure,
 )
import Network.HTTP.Types (Status (statusCode, statusMessage))

import System.Environment.Blank (getEnv)

data Method = GET deriving (Show)

data Endpoint = DataSets

instance Show Endpoint where
    show DataSets = "/cdo-web/api/v2/datasets"

newtype Token
    = Token BC.ByteString
    deriving (Show)

buildReq :: Token -> Method -> Endpoint -> Request
buildReq (Token token) meth endpoint =
    defaultRequest
        & setRequestMethod (BC.pack $ show meth)
        & setRequestHost "www.ncei.noaa.gov"
        & setRequestHeader "token" [token]
        & setRequestPath (BC.pack $ show endpoint)
        & setRequestSecure True
        & setRequestPort 443

-- Q39.1
buildReqNoSSL :: Token -> Method -> Endpoint -> Request
buildReqNoSSL a b c =
    setRequestSecure False $ buildReq a b c

buildReqNoSSL' :: Token -> Method -> Endpoint -> Request
buildReqNoSSL' a b =
    setRequestSecure False . buildReq a b

-- Just for fun
-- https://hackage.haskell.org/package/composition-prelude-3.0.0.2/docs/src/Control.Composition.html#.%2A%2A
(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) f g a b c = f (g a b c)

buildReqNoSSL'' :: Token -> Method -> Endpoint -> Request
buildReqNoSSL'' = setRequestSecure False .** buildReq

{-
getToken :: IO (Maybe Token)
getToken = do
    mToken <- getEnv "TOKEN"
    return $ Token . BC.pack <$> mToken
 -}

getToken :: IO (Maybe Token)
getToken =
    fmap
        (fmap (Token . BC.pack))
        (getEnv "TOKEN")

getTokenExn :: IO Token
getTokenExn =
    fmap
        (fromMaybe $ error "Token is missing, check the README")
        getToken

main :: IO ()
main = do
    token <- getTokenExn
    response <- httpLBS $ buildReq token GET DataSets
    let sc = getResponseStatus response
    let (statusCode', statusMessage') = (statusCode sc, statusMessage sc)
    if statusCode' /= 200
        then do
            -- Q39.2
            putStrLn "Sorry, the request failed"
            putStrLn $
                mconcat
                    [ "The status was: "
                    , show statusCode'
                    , " (" <> show statusMessage' <> ")"
                    ]
        else do
            putStrLn "Saving response to /tmp/tmp.json"
            let jsonBody = getResponseBody response
            L.writeFile "/tmp/tmp.json" jsonBody
            putStrLn $
                mconcat
                    [ "Now you can observe the data with: "
                    , show ("cat /tmp/tmp.json | jq" :: String)
                    ]
