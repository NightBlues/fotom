module Main where
import Photo (make_photo, show_all, insert, init_)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)


main :: IO ()
main =
  join $ execParser (info (helper <*> parseCli) idm)


parseCli :: Parser (IO ())
parseCli =
  let add_parser = info (helper <*> add_opt) desc
        where
          add_opt = add_cmd <$> argument str (
            metavar "FILENAME" <> help "file to add")
          desc = progDesc "Add file to your collection."
      init_parser = info (helper <*> pure init_cmd) desc
        where
          desc = progDesc "Init collection database."
      list_parser = info (helper <*> pure list_cmd) desc
        where
          desc = progDesc "Show photo list."
  in
    subparser (
      command "init" init_parser
      <>
      command "add" add_parser
      <>
      command "list" list_parser
    )

init_cmd = init_

add_cmd filename =  do
  Just photo <- make_photo filename
  insert photo
  print photo

list_cmd = show_all
