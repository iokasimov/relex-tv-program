module TV.Program (Program (..)) where

import TV.Hour (Hour)
import TV.Minute (Minute)

data Program = Program
	{ title :: String
	, start :: (Hour, Minute)
	, end :: (Hour, Minute)
	} deriving Show
