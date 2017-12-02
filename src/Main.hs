module Main where
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join, foldM, (<$!>))
import System.Directory
import Text.Printf
import qualified System.Directory as D
import qualified Action
import qualified Db
import qualified Config
import qualified Photo
import System.FilePath
import Debug.Trace

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
      move_parser = info (helper <*> move_opts) desc
        where
          move_opts = move_cmd <$> switch (
            long "rm" <> help
            "remove old files if given, keep otherwise (default: keep old files)")
          desc = progDesc "Move photos on their places in directory tree."
      check_parser = info (helper <*> pure check_cmd) desc
        where
          desc = progDesc "Check that photos are on their places and hash match."
  in
    subparser (
      command "init" init_parser
      <>
      command "add" add_parser
      <>
      command "list" list_parser
      <>
      command "move" move_parser
      <>
      command "check" check_parser
    )


init_cmd = do
  cd <- getCurrentDirectory
  conf_ <- Config.load
  let conf =
        case conf_ of
          Nothing -> Config.Config {Config.store_path = ""}
          Just c -> c
  putStrLn (printf "Setting directory '%s' as store path (previous was '%s')."
            cd (Config.store_path conf))
  let conf = Config.Config cd
  Config.save conf
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

bfs_directory dir =
  let group_func (files, dirs) f = do
        let f_ = dir </> f
        isfile <- D.doesFileExist f_
        isdir <- D.doesDirectoryExist f_
        let res =
              case (isfile, isdir) of
                (True, _) -> (f_:files, dirs)
                (_, True) -> (files, f_:dirs)
                _ -> (files, dirs)
        return res
      extend_dir files dir =
        (files ++) <$!> bfs_directory dir
  in
    D.listDirectory dir >>=
    (foldM group_func ([], [])) >>=
    -- (\(files, dirs) -> return $ files ++ dirs)
    (\(files, dirs) -> foldM extend_dir files dirs)


add_cmd filename = with_db action
  where
    action conf conn = do
      isdir <- D.doesDirectoryExist filename
      if isdir then do
        files <- bfs_directory filename
        mapM_ (process conf conn) files
      else
        process conf conn filename
    process conf conn file = do
      photo <- Action.make_photo conf conn file
      case photo of
        Left err -> putStrLn $ "Error: " ++ err
        Right photo -> do
          Db.insert conn photo
          print photo

list_cmd = with_db action
  where
    action _ conn = do
      Db.map_photos conn print >> return ()

move_cmd rmflag = with_db action
  where
    action conf conn = do
      let move_func photo = do
            let src = Photo.name photo
                dst = Action.calculate_path photo
            res <- Action.move_photo conf src dst rmflag
            if res then do
              putStrLn (printf "%s -> %s" src dst)
              Db.save conn (photo {Photo.name=dst})
            else return ()
      Db.map_photos conn move_func
      return ()

check_cmd = putStrLn "Not implemented yet"
