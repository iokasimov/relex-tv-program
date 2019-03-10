module TV.Hour (Hour, hour) where

import "lens" Control.Lens (Prism', prism')

data Hour = AM Int | PM Int

instance Show Hour where
	show (AM n) = case show n of
		[c] -> '0':[c]
		cs -> cs
	show (PM n) = case show $ 12 + n of
		[c] -> '0':[c]
		cs -> cs

hour :: Prism' Int Hour
hour = prism' from to where

	from :: Hour -> Int
	from (AM hour) = hour
	from (PM hour) = hour

	to :: Int -> Maybe Hour
	to h@(flip elem [1..12] -> True) = Just . AM $ h
	to h@(flip elem [13..24] -> True) = Just . PM $ h - 12
	to _  = Nothing
