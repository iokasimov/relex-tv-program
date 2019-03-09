module Main where

import "base" Data.Foldable (find)
import "base" Data.Function ((&))
import "base" Data.Maybe (isJust)
import "data-default-class" Data.Default.Class (Default (def))
import "lens" Control.Lens (Prism', view, preview, element, (^.), (^?), prism', toListOf)
import "xml-lens" Text.XML.Lens (Element (..), Node (..), (^..), (./)
	, attribute, attributeIs, root, elementName, el, nodes
	, prologue, name, named, entire, text, _Element, _Content)
import "text" Data.Text (Text)

import qualified "containers" Data.Map.Lazy as Map (lookup)
import qualified "xml-conduit" Text.XML as XML (readFile)

data Hour = AM Int | PM Int deriving Show

hour :: Prism' Int Hour
hour = prism' from to where

	from :: Hour -> Int
	from (AM hour) = hour
	from (PM hour) = hour

	to :: Int -> Maybe Hour
	to h@(flip elem [1..12] -> True) = Just . AM $ h
	to h@(flip elem [13..24] -> True) = Just . PM $ h - 12
	to _  = Nothing

newtype Minute = Minute Int deriving Show

minute :: Prism' Int Minute
minute = prism' from to where

	from :: Minute -> Int
	from (Minute m) = m

	to :: Int -> Maybe Minute
	to m@(flip elem [0..59] -> True) = Just . Minute $ m

data Program = Program
	{ title :: String
	, start :: (Hour, Minute)
	, end :: (Hour, Minute)
	} deriving Show

data Channel = Channel
	{ channel :: Text
	, programs :: [Program]
	} deriving Show

data Week = Week
	{ monday :: [Channel]
	, tuesday :: [Channel]
	, wednesday :: [Channel]
	, thursday :: [Channel]
	, friday :: [Channel]
	, saturday :: [Channel]
	, sunday :: [Channel]
	} deriving Show

elements :: Element -> [Element]
elements e = (preview _Element) <$> (e ^. nodes) &
	foldr (\x acc -> maybe acc (flip (:) acc) x) []

contents :: Element -> [Text]
contents e = (preview _Content) <$> (e ^. nodes) &
	foldr (\x acc -> maybe acc (flip (:) acc) x) []

extract_day_of_week :: Text -> [Element] -> Maybe Element
extract_day_of_week day = find ((==) (Just day) . Map.lookup "id" . elementAttributes)

extract_channels :: Element -> [Channel]
extract_channels = foldr (\x acc -> maybe acc (flip (:) acc) x) []
	. fmap parse_channel . filter ((==) "div" . view name) . elements

parse_channel :: Element -> Maybe Channel
parse_channel e = Channel
	<$> (fmap (mconcat . contents) $ find ((==) "h3" . view name) $ elements e)
	<*> Just []

parse_week :: [Element] -> Maybe Week
parse_week days = Week
	<$> (extract_channels <$> extract_day_of_week "monday" days)
	<*> (extract_channels <$> extract_day_of_week "tuesday" days)
	<*> (extract_channels <$> extract_day_of_week "wednesday" days)
	<*> (extract_channels <$> extract_day_of_week "thursday" days)
	<*> (extract_channels <$> extract_day_of_week "friday" days)
	<*> (extract_channels <$> extract_day_of_week "saturday" days)
	<*> (extract_channels <$> extract_day_of_week "sunday" days)

main = do
	doc <- XML.readFile def "tv.html"
	let days = doc ^.. root ./ named "body" ./ attributeIs "id" "content" ./ attributeIs "class" "day"
	print $ parse_week days
