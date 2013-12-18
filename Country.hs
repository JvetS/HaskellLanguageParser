module Country where
	import qualified Data.IntSet as IntSet
	import Data.List
		
	import Graph
	import Hash

	data Country = Country {countryName :: String, adjacentCountryNames :: [String]} deriving (Show)
	instance Eq Country where
		(==) c c' = countryName c == countryName c'
		
	tupleCountry :: Country -> (String, [String])
	tupleCountry c = (countryName c, adjacentCountryNames c)
	
	countryAdjacencies :: Graph -> String
	countryAdjacencies g = intercalate "\n" (map formatNode ns)
		where
			ns = getNodes g
			name = fst . unN
			names n = (map (name . getNodeInt g) (keys n))
			keys = IntSet.toList . snd . unN
			formatNode n = name n ++ " has borders with " ++ (intercalate ", " (names n)) ++ "."
	
	countryGraph :: [Country] -> Graph
	countryGraph = (foldr (uncurry insertNodeWithEdges) Graph.empty) . (map tupleCountry)
	
	countryPath :: Graph -> Node -> Node -> String
	countryPath g n n' = verbose (shortestPath g n n')
		where
			verbose ns = "Shortest path from " ++ (name (last ns)) ++ " to " ++ (name (head ns)) ++ " is " ++ (intercalate ", " (map name (reverse ns))) ++ "."
			name = fst . unN
			
	countrySpan :: Graph -> String
	countrySpan g = verbose (minimumSpan g)
		where
			verbose ns = (intercalate ", " (map name ns)) ++ "."
			name = fst . unN