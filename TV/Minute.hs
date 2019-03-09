module TV.Minute (Minute, minute) where

import "lens" Control.Lens (Prism', prism')

newtype Minute = Minute Int deriving Show

minute :: Prism' Int Minute
minute = prism' from to where

	from :: Minute -> Int
	from (Minute m) = m

	to :: Int -> Maybe Minute
	to m@(flip elem [0..59] -> True) = Just . Minute $ m
