module Photo where
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Time.Clock
import qualified Data.ByteString as BS


data Photo = Photo {
  id_ :: Maybe Integer,
  name :: String,
  date :: Maybe UTCTime,
  hash :: BS.ByteString
  } deriving Show

instance FromRow Photo where
  fromRow = do
    id_ <- field
    name <- field
    date <- field
    hash <- field
    return Photo {id_=Just id_, name=name, date=date, hash=hash}

instance ToRow Photo where
  toRow p =
    [toField $ id_ p, toField $ name p, toField $ date p, toField $ hash p]
