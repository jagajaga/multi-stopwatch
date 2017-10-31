module Stopwatch.Pages.Main where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Data.UUID
import           Data.Time.Clock
import           Data.Map
import           Data.IORef
import           Control.Monad.Reader

-- * api

data InitResponse = InitResponse
     {
        uUID :: UUID
     } deriving (Generic)
data InitRequest = InitRequest
     {
       uTCTime :: UTCTime
     } deriving (Generic)

type StopRequest = InitRequest -- TODO @pva701
stopRequest = InitRequest

instance ToJSON InitResponse
instance FromJSON InitRequest

type ItemApi =
  "item" :> Capture "itemUuid" UUID :> Get '[JSON] Item :<|>
  "init" :> ReqBody '[JSON] InitRequest :> Post '[JSON] InitResponse :<|>
  "stop" :> Capture "itemUuid" UUID :> ReqBody '[JSON] StopRequest :> Patch '[JSON] ()

itemApi :: Proxy ItemApi
itemApi = Proxy

type Env = IORef (Map UUID Item)

type M = ReaderT Env Handler

server :: Env -> Server ItemApi
server x = enter runM $
  getItem :<|>
  initItem :<|>
  stopItem -- vegani lohi
  where
    runM :: M :~> Handler
    runM = NT $ \m ->
        runReaderT m x

getItem :: UUID -> M Item
getItem _ = throwError err500

initItem :: InitRequest -> M InitResponse
initItem _ = throwError err500

stopItem :: UUID -> StopRequest -> M ()
stopItem _ _ = throwError err500

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
