module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)
import System.Directory
import Text.Printf
import qualified Action
import qualified Db
import qualified Config
import qualified Photo

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
      move_parser = info (helper <*> pure move_cmd) desc
        where
          desc = progDesc "Move photos on their places in directory tree."
  in
    subparser (
      command "init" init_parser
      <>
      command "add" add_parser
      <>
      command "list" list_parser
      <>
      command "move" move_parser
    )


init_cmd = do
  cd <- getCurrentDirectory
  conf_ <- Config.load
  let conf =
        case conf_ of
          Nothing -> Config.Config {Config.store_path = ""}
          Just c -> c
  putStrLn (printf "Set directory '%s' as store path (previous was '%s')."
            cd (Config.store_path conf))
  Config.save (Config.Config cd)
  conn <- Db.open_conn conf
  Db.init_ conn
  Db.close_conn conn

with_db func = do
  conf_ <- Config.load
  case conf_ of
    Nothing -> putStrLn "Error: Could not load config"
    Just conf -> do
      conn <- Db.open_conn conf
      func conf conn
      Db.close_conn conn

add_cmd filename = with_db action
  where
    action _ conn = do
      photo <- Action.make_photo conn filename
      case photo of
        Left err -> putStrLn $ "Error: " ++ err
        Right photo -> do
          Db.insert conn photo
          print photo

list_cmd = with_db action
  where
    action _ conn = do
      Db.map_photos conn print >> return ()
  
move_cmd = with_db action
  where
    action conf conn = do
      let move_func photo = do
            let src = Photo.name photo
                dst = Action.calculate_path photo
            putStrLn (printf "%s -> %s" src dst)
            res <- Action.move_photo conf src dst
            case res of
              True -> Db.save conn (photo {Photo.name=dst})
              False -> return ()
      Db.map_photos conn move_func
      return ()
