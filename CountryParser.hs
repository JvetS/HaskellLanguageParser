module CountryParser where
	import Data.String
	import Data.Char
	import Data.List

	import Country
	import NaturalLanguageParser
	import TooSimpleParseLib

	pCountryString :: Parser Char String
	pCountryString = (:) <$> pAlphaUpper <*> many pAlpha
	
	pCountryNoun :: Parser Char String
	pCountryNoun = (pChoice . (<$>) pPackWord $ ["border", "edge"]) `opt` ""
	
	pCountryVerb :: Parser Char String
	pCountryVerb = pChoice . (<$>) pPackWord $ ["borders", "edges", "shares a", "lies"]
	
	pDirectionString :: Parser Char String
	pDirectionString = (pPackWord "in the" <* pDirection) `opt` ""
	
	pCloset :: Parser Char String
	pCloset = pChoice . (<$>) pPackWord $ ["on", "with", "under", "above", "next to"]
	
	pDirection :: Parser Char String
	pDirection = (pChoice . (<$>) pPackWord $ ["north", "south", "east", "west"]) `opt` ""
	
	pCountryCons :: Parser Char String
	pCountryCons = pCountryVerb <* pCountryNoun <* pDirectionString <* pCloset
	
	pAnd :: Parser Char String
	pAnd = pPackWord "and"
	
	pAswell :: Parser Char String
	pAswell = pPackWord "as well as"
	
	pPackWord :: String -> Parser Char String
	pPackWord s = pPack pManyWhiteSpace (pToken s) pSomeWhiteSpace
	
	pCountryAdjacencies :: Parser Char [String]
	pCountryAdjacencies = (some (pCountryString <* (pAnd <|> pAswell <|> pCommaSeq `opt` "")))
	
	pCountry :: Parser Char Country
	pCountry = Country <$> (pCountryString <* pCountryCons) <*> pCountryAdjacencies <* pEnd
	
	pNonsense :: Parser Char Country
	pNonsense = Country <$> pure "" <*> pure [] <* pSentence
	
	pCountries :: Parser Char [Country]
	pCountries = {-adjustEdges . adjustNodes . -}filter (Country "" [] /=) <$> many ((pCountry <|> pNonsense) <* pManyWhiteSpace)
		{-where
			adjustNodes :: [Country] -> [Country]
			adjustNodes xs = adjustNodes' xs xs
				where
					adjustNodes' [] ys = ys
					adjustNodes' (c:cs) ys = adjustNodes' cs (foldr insertNode ys (map (flip Country []) (adjacentCountryNames c)))
					insertNode cc ccs = if (elem cc ccs) then ccs else (cc:ccs)
			
			adjustEdges :: [Country] -> [Country]
			adjustEdges xs = adjustEdges' xs xs
				where
					adjustEdges' [] ys = ys
					adjustEdges' (c:cs) ys = adjustEdges' cs (map (insertEdge c) ys)
					c `insertEdge` c' = Country (countryName c') (if (elem (countryName c') (adjacentCountryNames c)) then (nub ((countryName c):(adjacentCountryNames c'))) else (nub (adjacentCountryNames c')))-}