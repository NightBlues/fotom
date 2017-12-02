{-# LANGUAGE OverloadedStrings #-}
module Action where
import Control.Monad
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.HsExif
import Text.Printf
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import System.Directory
import System.FilePath

import Photo
import Db
import Config


find_date tags =
  let datetime =
        case Map.lookup dateTimeOriginal tags of
          Nothing -> Map.lookup dateTime tags
          Just datetime -> Just datetime
  in
    localTimeToUTC utc <$!>
    (datetime >>=
     \(ExifText datetime) -> readExifDateTime datetime)


make_photo (Config store_path) conn filename = do
  abspath <- makeAbsolute filename
  let filepath = makeRelative store_path abspath
  exif <- parseFileExif filename
  hash <- fmap SHA256.hash $ (BS.readFile filename)
  hash_unique <- check_hash conn hash
  case (hash_unique, exif) of
    (True, Right tags) ->
        return $ Right $ Photo {id_=Nothing, name=filepath, date=find_date tags,
                                hash=hash}
    (True, Left err) -> return $ Left err
    (False, _) -> return $ Left "Photo with such hash already exists."



calculate_path :: Photo -> String
calculate_path (Photo id_ name date hash) =
  let (year, _, _) =
        case date of
          Just (UTCTime {utctDay=day, utctDayTime=_}) -> toGregorian day
          Nothing -> (-1, -1, -1)
      left = printf "%d" year
      filename = takeFileName name
      right = case id_ of
                Just id_ ->
                  if isPrefixOf (printf "%d_" id_) filename then
                    filename
                  else
                    printf "%d_%s" id_ filename
                Nothing -> filename
  in
    case year of
      -1 -> right
      _ -> left </> right


move_photo (Config store_path) from to rmflag =
  withCurrentDirectory store_path domove
  where
    domove = do
      createDirectoryIfMissing True $ takeDirectory to
      if from /= to then do
        move_func from to
        return True
      else
        return False
    move_func = if rmflag then renamePath else copyFile
