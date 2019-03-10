module TV.Minute (Minute, minute) where

import "lens" Control.Lens (Prism', prism')

newtype Minute = Minute Int

instance Show Minute where
	show (Minute m) = case show m of
		[c] -> '0':[c]
		cs -> cs

minute :: Prism' Int Minute
minute = prism' from to where

	from :: Minute -> Int
	from (Minute m) = m

	to :: Int -> Maybe Minute
	to m@(flip elem [0..59] -> True) = Just . Minute $ m
