module TV.Week (Week (..)) where

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
