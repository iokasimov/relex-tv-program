module TV.Channel (Channel (..)) where

import "text" Data.Text (Text)

import TV.Program (Program)

data Channel = Channel
	{ channel :: Text
	, programs :: [Program]
	} deriving Show
