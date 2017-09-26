{-# LANGUAGE OverloadedStrings #-}
module Photo where
import Graphics.HsExif
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Crypto.Hash.SHA256 as SHA256

dbpath = "db.sqlite"

data Photo = Photo {
  id_ :: Maybe Integer,
  name :: String,
  date :: String,
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


flt_func (Just x) = True
flt_func Nothing = False


make_photo filename = do
  exif <- parseFileExif filename
  hash <- fmap SHA256.hash $ (BS.readFile filename)
  hash_unique <- check_hash hash
  case (hash_unique, exif) of
    (True, Right tags) ->
      let ExifText date =
            findWithDefault
            (findWithDefault (ExifText "unknown") dateTimeOriginal tags)
            dateTime tags in
        return $ Right $ Photo {id_=Nothing, name=filename, date=date,
                                hash=hash}
    (True, Left err) -> return $ Left err
    (False, _) -> return $ Left "Photo with such hash already exists."

show_all = do
  conn <- open dbpath
  r <- query_ conn "SELECT * FROM photo" :: IO [Photo]
  mapM_ print r
  close conn

insert p = do
  conn <- open dbpath
  execute conn "INSERT INTO photo VALUES (?, ?, ?, ?)" p
  rowId <- lastInsertRowId conn
  close conn
  return rowId

check_hash :: BS.ByteString -> IO Bool
check_hash hash = do
  conn <- open dbpath
  [[res]] <- queryNamed conn "SELECT COUNT(*) FROM photo WHERE hash=:hash"
           [":hash" := hash]
  close conn
  return (res == (0 :: Integer))

init_ = do
  conn <- open dbpath
  execute_ conn "CREATE TABLE IF NOT EXISTS photo (id INTEGER PRIMARY KEY, name TEXT, date TEXT, hash TEXT)"
  close conn
