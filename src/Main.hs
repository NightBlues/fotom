module Main where
import System.Environment
import Photo (make_photo, show_all, insert)

main = do
  args <- getArgs
  case args of
    [filename] -> do
      Just photo <- make_photo filename
      insert photo
      print photo
    _ -> show_all
