module TV.Week (Week (..)) where

import "aeson" Data.Aeson (ToJSON (toJSON), Value (Array), object, (.=))
import "text" Data.Text (Text)
import "vector" Data.Vector (fromList)

import TV.Channel (Channel)

data Week = Week
	{ monday :: [Channel]
	, tuesday :: [Channel]
	, wednesday :: [Channel]
	, thursday :: [Channel]
	, friday :: [Channel]
	, saturday :: [Channel]
	, sunday :: [Channel]
	} deriving Show

instance ToJSON Week where
	toJSON w = Array . fromList $
		object ["channels" .= monday w, "day" .= ("monday"  :: Text)] :
		object ["channels" .= tuesday w, "day" .= ("tuesday" :: Text)] :
		object ["channels" .= wednesday w, "day" .= ("wednesday" :: Text)] :
		object ["channels" .= thursday w, "day" .= ("thursday" :: Text)] :
		object ["channels" .= friday w, "day" .= ("friday" :: Text)] :
		object ["channels" .= saturday w, "day" .= ("saturday" :: Text)] :
		object ["channels" .= sunday w, "day" .= ("sunday" :: Text)] : []
