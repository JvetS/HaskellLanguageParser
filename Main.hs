{--
Course: FP
Assignment: 3
Authors: Hugo van Krimpen (3686132), Steven van Rossum (3584518)
Implementation:
  - Basics.
  - IntSet instead of List adjacencies in the graph.
  - Queue-like (although inefficient because of concat calls) search algorithm, which handles nodes one at a time and skips nodes already in the list.
--}
module Main where
	import Control.Applicative
	import Data.Char
	import Data.Functor
	import Data.List
	import Data.String
	
	import qualified Data.IntMap as IntMap
	
	
	import Country
	import Graph
	import CountryParser
	import TooSimpleParseLib
	
	main :: IO()
	main = do
		putStrLn "Filepath of the region description:"
		fp <- getLine
		r <- countryGraph . snd . head <$> runParser (determ pCountries) <$> readFile fp
		putStrLn "\nThis is the total list of nodes and their adjacencies:"
		putStrLn (countryAdjacencies r)
		putStrLn "\nThis is the minimal spanning path:"
		putStrLn (countrySpan r)
		graphOperations r
		
	graphOperations :: Graph -> IO()
	graphOperations g = do
		putStrLn "Enter a source and destination country in the graph to find the shortest path between them. Use Q to quit."
		putStr "Source: "
		x <- getLine
		putStr "Destination: "
		y <- getLine
		if (x == "q" || x == "Q" || y == "q" || y == "Q")
			then
				return ()
			else
				if (elem x (map (fst . unN) (getNodes g))) && (elem y (map (fst . unN) (getNodes g)))
					then do
						putStrLn (countryPath g (getNodeString g x) (getNodeString g y))
						graphOperations g
					else do
						putStrLn "Couldn't find one of the given countries in this description."
						graphOperations g