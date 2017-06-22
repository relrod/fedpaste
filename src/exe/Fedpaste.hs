{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Web.Modernpaste.Client
import Web.Modernpaste.Types
import Sysinfo
import System.IO

data Args = Args GlobalOpts Command
  deriving (Eq, Show)

data Command =
    Paste { pasteOptions :: PasteOptions }
  | Sysinfo { sysinfoNoConfirm :: Bool }
  deriving (Eq, Show)

data PasteOptions =
    PasteOptions { files :: [String]
                 , title :: Maybe String
                 , language :: Maybe String
                 , password :: Maybe String
                 } deriving (Eq, Show)

data GlobalOpts =
  GlobalOpts { server :: String }
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

sysinfo :: Parser Command
sysinfo = Sysinfo <$> switch
                        ( long "no-confirm"
                       <> help "Do NOT require confirmation" )

pasteOptionsParser :: Parser PasteOptions
pasteOptionsParser =
  (PasteOptions
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
                 <> short 'd'
                 <> help "Optional password"
                 <> metavar "PASSWORD" )))

parser :: Parser Args
parser =
  (Args
   <$> globalOpts
   <*> ((subparser (
           ( command "sysinfo"
             (info sysinfo
              (progDesc "Paste system information")))))
        <|> (Paste <$> pasteOptionsParser)))


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
run (Args global (Sysinfo noconfirm)) = do
  putStrLn "Obtaining system information. Please stand by."
  info <- runInfoList
  T.putStrLn $ info
  confirmation <- if noconfirm then return True else confirm
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
  input <- mapM T.readFile files
  mapM_ (paste global . createReq pasteOptions) input

createReq :: PasteOptions -> T.Text -> CreatePasteRequest
createReq (PasteOptions files title language password) text =
  CreatePasteRequest text
                     Nothing
                     (fmap T.pack title)
                     (fmap T.pack language)
                     (fmap T.pack password)

paste :: GlobalOpts -> CreatePasteRequest -> IO ()
paste global req = do
  let cfg = Modernpaste (server global)
  res <- runMPResponseT cfg (createPaste req)
  case res of
    Nothing -> error "Error while contacting the paste server."
    Just (MPError (ErrorResponse failure msg)) ->
      error $ "Error from paste server (" ++ T.unpack failure ++ "): " ++
        T.unpack msg
    Just (MPSuccess resp) -> print resp

opts :: ParserInfo Args
opts = info (parser <**> helper) idm

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  execRes <- customExecParser (prefs noBacktrack) opts
  run execRes
