module TV.Channel (Channel (..)) where

import "aeson" Data.Aeson (ToJSON (toJSON), object, (.=))
import "text" Data.Text (Text)

import TV.Program (Program)

data Channel = Channel
	{ channel :: Text
	, programs :: [Program]
	} deriving Show

instance ToJSON Channel where
	toJSON c = object ["channel" .= channel c, "programming" .= programs c]
