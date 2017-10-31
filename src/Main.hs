module Main where

import Servant

import Stopwatch.Pages.Main
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Data.IORef

import qualified Network.Wai as Wai
import qualified Data.Map as Map
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

httpsPort :: Warp.Port
httpsPort = 443

parseEnvPort :: Maybe String -> Int
parseEnvPort mStr = fromMaybe httpsPort $ do
  str <- mStr
  readMaybe str

runServant :: Warp.Port -> IO ()
runServant port = do
  putStrLn ("Servant is running on port " ++ show port)
  myIO <- newIORef Map.empty
  let app = serve itemApi (server myIO)
  if port /= httpsPort then Warp.run port app else do
    let certPath = "/etc/letsencrypt/live/int-index.com/"
    Warp.runTLS
      (Warp.tlsSettingsChain
        (certPath ++ "cert.pem")
        [certPath ++ "chain.pem"]
        (certPath ++ "privkey.pem"))
      (Warp.setPort port Warp.defaultSettings)
      app

main :: IO ()
main = do
  port <- parseEnvPort <$> lookupEnv "PORT"
  runServant port
