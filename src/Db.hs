{-# LANGUAGE OverloadedStrings #-}
module Db where
import Database.SQLite.Simple
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Photo
import Config


dbpath = "db.sqlite"

get_db_path store_path = store_path </> dbpath

open_conn (Config store_path) = open $ get_db_path store_path

close_conn = close

map_photos conn func = do
  r <- query_ conn "SELECT * FROM photo" :: IO [Photo]
  res <- mapM func r
  return res

insert conn p = do
  execute conn "INSERT INTO photo VALUES (?, ?, ?, ?)" p
  rowId <- lastInsertRowId conn
  return rowId

save conn p = do
  -- execute conn "UPDATE photo SET ?" p
  executeNamed conn
    "UPDATE photo SET name=:name, date=:date, hash=:hash WHERE id=:id_"
    [":name" := name p, ":date" := date p, ":hash" := hash p, ":id_" := id_ p]

check_hash :: Connection -> BS.ByteString -> IO Bool
check_hash conn hash = do
  [[res]] <- queryNamed conn "SELECT COUNT(*) FROM photo WHERE hash=:hash"
           [":hash" := hash]
  return (res == (0 :: Integer))

init_ conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS photo (id INTEGER PRIMARY KEY, name TEXT, date TEXT, hash TEXT)"
