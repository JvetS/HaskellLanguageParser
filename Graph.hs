module Graph where
	import qualified Data.IntMap as IntMap
	import qualified Data.IntSet as IntSet
	import Data.List
	
	import Hash
	
	hash :: String -> Int
	hash = fromIntegral . hashString
	
	newtype Graph = G { unG :: IntMap.IntMap Node } deriving (Show)
	newtype Node = N { unN :: (String, IntSet.IntSet)} deriving (Show)
	
	empty :: Graph
	empty = G IntMap.empty
	
	getNodeString :: Graph -> String -> Node
	getNodeString g s = getNodeInt g (hash s)
	
	getNodeInt :: Graph -> Int ->  Node
	getNodeInt (G g) i = (IntMap.!) g i
	
	getNodes :: Graph -> [Node]
	getNodes = IntMap.elems . unG
	
	insertNode :: String -> Graph -> Graph
	insertNode s = insertNodeWithEdges s []
	
	insertNodeWithEdges :: String -> [String] -> Graph -> Graph
	insertNodeWithEdges s e (G g) = G (adjustEdges (adjustNodes g))
		where
			adjustNodes = IntMap.insertWith insertWith k (N (s, foldr IntSet.insert IntSet.empty ks))
			adjustEdges = (flip (foldr id)) (zipWith (IntMap.insertWith insertWith) ks ns)
			insertWith = (\(N (s, e)) (N (s', e')) -> N (s, IntSet.union e e'))
			ks = map hash e
			k = hash s
			ns = map (N . (flip (,) (IntSet.singleton k))) e
		
	deleteNodeString :: String -> Graph -> Graph
	deleteNodeString s = deleteNodeInt . hash $ s
	
	deleteNodeInt :: Int -> Graph -> Graph
	deleteNodeInt i (G g) = G (IntMap.map (deleteEdgeInt i) (IntMap.delete i g))
	
	insertEdgeString :: String -> Node -> Node
	insertEdgeString s = insertEdgeInt . hash $ s
	
	insertEdgeInt :: Int -> Node -> Node
	insertEdgeInt i (N (s, e)) = N (s, IntSet.insert i e)
	
	deleteEdgeString :: String -> Node -> Node
	deleteEdgeString s = deleteEdgeInt . hash $ s
	
	deleteEdgeInt :: Int -> Node -> Node
	deleteEdgeInt i (N (s, e)) = N (s, IntSet.delete i e)
	
	shortestPath :: Graph -> Node -> Node -> [Node]
	shortestPath (G g) n (N (s', e')) = shortestPath' [[n]]
		where
			shortestPath' :: [[Node]] -> [Node]
			shortestPath' [[]] = []
			shortestPath' (((N (s, e)):ns):nss) = if (s == s') then ((N (s, e)):ns) else (shortestPath' (nss ++ (zipWith (:) (map ((IntMap.!) g) (IntSet.toList e)) (repeat ((N (s, e'')):ns)))))
				where
					e'' = IntSet.difference e (IntSet.intersection e ns')
					ns' = IntSet.fromList (map (hash . fst . unN) ns)
					
	minimumSpan :: Graph -> [Node]
	minimumSpan (G g) = minimumSpan' [IntMap.elems g]
		where
			minimumSpan' :: [[Node]] -> [Node]
			minimumSpan' [[]] = []
			minimumSpan' (((N (s, e)):ns):nss) = if IntSet.null e'' then ((N (s, e)):ns) else (minimumSpan' (nss ++ (zipWith (:) (map ((IntMap.!) g) (IntSet.toList e)) (repeat ((N (s, e'')):ns)))))
				where
					e'' = IntSet.difference e (IntSet.intersection e ns')
					ns' = IntSet.fromList (map (hash . fst . unN) ns)