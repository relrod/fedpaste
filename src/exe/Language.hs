{-# LANGUAGE OverloadedStrings #-}
module Language where

import Data.List (find, isSuffixOf)
import Data.Maybe (isJust, maybe)
import qualified Data.Text as T

languageExts :: [(T.Text, [T.Text])]
languageExts =
  [ ("clike", [".c", ".h", ".cpp", ".cxx", ".c++", ".cc"])
  , ("cmake", [".cmake"])
  , ("css", [".css", ".sass"])
  , ("d", [".d"])
  , ("diff", [".diff", ".patch"])
  , ("dockerfile", [".docker", "Dockerfile"])
  , ("erlang", [".erl"])
  , ("go", [".go"])
  , ("haskell", [".hs", ".lhs"])
  , ("htmlmixed", [".html", ".htm"])
  , ("javascript", [".js"])
  , ("jinja2", [".j2"])
  , ("lua", [".lua"])
  , ("markdown", [".markdown", ".md"])
  , ("perl", [".pl"])
  , ("php", [".php"])
  , ("python", [".py"])
  , ("rpm", [".spec"])
  , ("rst", [".rst"])
  , ("ruby", [".rb"])
  , ("rust", [".rs"])
  , ("shell", [".sh", ".bash"])
  , ("sql", [".sql"])
  , ("swift", [".swift"])
  , ("xml", [".xml"])
  , ("yaml", [".yaml", ".yml"])
  ]

extension :: T.Text -> T.Text
extension filename = maybe "text" fst (find (isJust . f . snd) languageExts)
  where
    f = find (`T.isSuffixOf` filename)
