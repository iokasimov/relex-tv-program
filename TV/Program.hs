module TV.Program (Program (..)) where

import "text" Data.Text (Text)

import TV.Hour (Hour)
import TV.Minute (Minute)

data Program = Program
	{ title :: Text
	, start :: (Hour, Minute)
	} deriving Show
