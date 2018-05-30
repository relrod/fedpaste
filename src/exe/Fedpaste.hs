{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (find, isSuffixOf)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Web.Modernpaste.Client
import Web.Modernpaste.Types
import System.Directory (doesFileExist)
import System.Exit
import System.IO

import Language
import Shorten
import Sysinfo

data Args = Args GlobalOpts Command
  deriving (Eq, Show)

data Command =
    Paste { pasteOptions :: PasteOptions }
  | Sysinfo
  deriving (Eq, Show)

data PasteOptions =
    PasteOptions { files :: [String]
                 , title :: Maybe T.Text
                 , language :: Maybe T.Text
                 , password :: Maybe T.Text
                 } deriving (Eq, Show)

data GlobalOpts =
  GlobalOpts { server :: String
             , noconfirm :: Bool }
  deriving (Eq, Show)

globalOpts :: Parser GlobalOpts
globalOpts = GlobalOpts
  <$> strOption
      ( short 'U'
     <> long "URL"
     <> long "url"
     <> help "The modern-paste server URL"
     <> value "https://paste.fedoraproject.org/"
      )
  <*> switch ( long "no-confirm"
            <> help "Do NOT require confirmation")


pasteOptionsParser :: Parser PasteOptions
pasteOptionsParser =
  PasteOptions
    <$> many (strArgument (metavar "FILES..."))
    <*> optional (strOption
                  ( long "title"
                 <> short 't'
                 <> help "Paste title"
                 <> metavar "TITLE" ))
    <*> optional (strOption
                  ( long "language"
                 <> short 'l'
                 <> help "Syntax highlighting language"
                 <> metavar "LANGUAGE" ))
    <*> optional (strOption
                  ( long "password"
                 <> short 'p'
                 <> short 'd'
                 <> help "Optional password"
                 <> metavar "PASSWORD" ))

parser :: Parser Args
parser =
  Args
    <$> globalOpts
    <*> (subparser
          ( command "sysinfo"
            (info (pure Sysinfo)
              (progDesc "Paste system information")))
          <|> (Paste <$> pasteOptionsParser))


confirm :: IO Bool
confirm = do
  putStr "Do you wish to continue with pasting? [y/N] "
  answer <- getLine
  return $ case fmap toLower answer of
    "y" -> True
    "yes" -> True
    _ -> False

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \x -> if x then t else f

run :: Args -> IO ()
run (Args global Sysinfo) = do
  putStrLn "Obtaining system information. Please stand by."
  info <- runInfoList
  T.putStrLn info
  confirmation <- if noconfirm global then return True else confirm
  if confirmation
    then paste global (createReq (PasteOptions
                                  []
                                  (Just "System information")
                                  Nothing
                                  Nothing) info)
    else putStrLn "Aborting."
run (Args global (Paste pasteOptions@(PasteOptions [] _ _ _))) = do
  input <- T.getContents
  let req = createReq pasteOptions input
  paste global req
run (Args global (Paste pasteOptions@(PasteOptions files _ _ _))) = do
  filesExist' <- mapM doesFileExist files
  let filesExist = zip files filesExist'
  mapM_ warnIfNotExist filesExist
  if not . all (== True) $ filesExist'
    then exitFailure
    else do
      putStrLn $ "Pasting all of: " ++ show files
      confirmation <- if noconfirm global then return True else confirm
      if confirmation
        then mapM_ (pasteFile global pasteOptions) files
        else putStrLn "Aborting."

warnIfNotExist :: (String, Bool) -> IO ()
warnIfNotExist (f, False) = putStrLn $ f ++ " does not exist. Will abort."
warnIfNotExist _ = return ()

pasteFile :: GlobalOpts -> PasteOptions -> String -> IO ()
pasteFile global pasteOptions filename = do
  let pasteLanguage =
        case language pasteOptions of
          Nothing -> Just (extension (T.pack filename))
          Just x -> Just x
  input <- T.readFile filename
  paste global . createReq (pasteOptions { language = pasteLanguage }) $ input

createReq :: PasteOptions -> T.Text -> CreatePasteRequest
createReq (PasteOptions files title language password) text =
  CreatePasteRequest text Nothing title language password

paste :: GlobalOpts -> CreatePasteRequest -> IO ()
paste global req = do
  let cfg = Modernpaste (server global)
  res <- runMPResponseT cfg (createPaste req)
  case res of
    Nothing -> error "Error while contacting the paste server."
    Just (MPError (ErrorResponse failure msg)) ->
      error $ "Error from paste server (" ++ T.unpack failure ++ "): " ++
        T.unpack msg
    Just (MPSuccess resp) -> do
      let url = createRespUrl resp
      T.putStr url
      shortened <- shorten url
      putStrLn $ " -> " ++ shortened

opts :: ParserInfo Args
opts = info (parser <**> helper) idm

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  execRes <- customExecParser (prefs (noBacktrack <> showHelpOnError)) opts
  run execRes
