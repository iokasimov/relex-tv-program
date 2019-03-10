module TV.Timeslot (Timeslot (..), parse_timeslot) where

import "aeson" Data.Aeson (ToJSON (toJSON), object, (.=))
import "base" Text.Read (readMaybe)
import "lens" Control.Lens (preview)
import "text" Data.Text (pack)

import TV.Hour (Hour, hour)
import TV.Minute (Minute, minute)

data Timeslot = Timeslot Hour Minute

instance Show Timeslot where
	show (Timeslot h m) = show h <> ":" <> show m

instance ToJSON Timeslot where
	toJSON t = object ["timeslot" .= (pack . show $ t)]

parse_timeslot :: String -> Maybe Timeslot
parse_timeslot (h1 : h2 : ':' : m1 : m2 : []) = Timeslot
	<$> (preview hour =<< readMaybe (h1 : h2 : []))
	<*> (preview minute =<< readMaybe (m1 : m2 : []))
