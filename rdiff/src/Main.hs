{-# LANGUAGE OverloadedStrings #-}
import Turtle
import Prelude hiding (FilePath)

import Control.Applicative

signature :: Text -> Text -> IO ExitCode
signature file sigFile = shell command empty
  where command = "rdiff signature " <> file <> " " <> sigFile

delta :: Text -> Text -> Text -> IO ExitCode
delta sigFile file deltaFile = shell command empty
  where command = "rdiff delta " <> sigFile <> " " <> file <> " " <> deltaFile

patch :: Text -> Text -> Text -> IO ExitCode
patch oldFile deltaFile newFile = shell command empty
  where command = "rdiff patch " <> oldFile <> " " <> deltaFile <> " " <> newFile

patch
