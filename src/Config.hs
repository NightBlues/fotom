{-# LANGUAGE DeriveGeneric #-}
module Config (Config(..), load, save) where

import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM)
import Data.Aeson
import System.Directory


config_path = getXdgDirectory XdgConfig "fotom"


data Config = Config
  {
    store_path :: String
  } deriving (Show, Generic)


instance ToJSON Config
instance FromJSON Config

load :: IO (Maybe Config)
load = do
  config_path <- config_path
  e <- doesFileExist config_path
  if e then
    liftM decode (B.readFile config_path)
  else
    return Nothing

save :: Config -> IO ()
save conf =
  config_path >>= (\c -> save' c conf)
  where
    save' config_path = B.writeFile config_path . encode
