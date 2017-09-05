module Main where
import System.Environment
import Graphics.Exif

data Photo = Photo { name :: String, date :: String } deriving Show


make_photo filename = do
  exif <- fromFile filename
  -- tags <- allTags exif
  -- return tags
  Just date <- getTag exif "DateTimeOriginal"
  return $ Photo {name=filename, date=date}

main = do
  [filename] <- getArgs
  photo <- make_photo filename
  print photo
