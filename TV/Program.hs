module TV.Program (Program (..)) where

import "aeson" Data.Aeson (ToJSON (toJSON), object, (.=))
import "text" Data.Text (Text, pack)

import TV.Timeslot (Timeslot)

data Program = Program
	{ title :: Text
	, timeslot :: Timeslot
	} deriving Show

instance ToJSON Program where
	toJSON c = object ["name" .= title c, "timeslot" .= (pack . show . timeslot $ c)]
