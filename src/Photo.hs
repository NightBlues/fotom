{-# LANGUAGE OverloadedStrings #-}
module Photo where
import Graphics.HsExif
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Map


dbpath = "db.sqlite"

data Photo = Photo {
  id_ :: Maybe Integer,
  name :: String,
  date :: String
  } deriving Show


instance FromRow Photo where
  fromRow = do
    id_ <- field
    name <- field
    date <- field
    return Photo {id_=Just id_, name=name, date=date}

instance ToRow Photo where
  toRow p =
    [toField $ id_ p, toField $ name p, toField $ date p]


flt_func (Just x) = True
flt_func Nothing = False


make_photo filename = do
  exif <- parseFileExif filename
  case exif of
    Left err -> return Nothing
    Right tags ->
      let ExifText date = tags ! dateTime in
        return $ Just $ Photo {id_=Nothing, name=filename, date=date}

show_all = do
  conn <- open dbpath
  r <- query_ conn "SELECT * FROM photo" :: IO [Photo]
  mapM_ print r
  close conn

insert p = do
  conn <- open dbpath
  execute conn "INSERT INTO photo VALUES (?, ?, ?)" p
  rowId <- lastInsertRowId conn
  close conn
  return rowId
