module NaturalLanguageParser where
	import Data.String
	import Data.Char
	import Data.List

	import TooSimpleParseLib

	pWhiteSpace :: Parser Char Char
	pWhiteSpace = pSatisfy (\c -> isSpace c)
	
	pSomeWhiteSpace :: Parser Char String
	pSomeWhiteSpace = some pWhiteSpace
	
	pManyWhiteSpace :: Parser Char String
	pManyWhiteSpace = many pWhiteSpace
	
	pEnd :: Parser Char String
	pEnd = some (pPoint <|> pExclm <|> pQstnm)
	
	pAlpha :: Parser Char Char
	pAlpha = pAlphaLower <|> pAlphaUpper
	
	pAlphaLower :: Parser Char Char
	pAlphaLower = pSatisfy (\c -> isLetter c && isLower c)
	
	pAlphaUpper :: Parser Char Char
	pAlphaUpper = pSatisfy (\c -> isLetter c && isUpper c)
	
	pWord :: Parser Char String
	pWord = some pAlpha
	
	pPoint :: Parser Char Char
	pPoint = pSym '.'
	
	pExclm :: Parser Char Char
	pExclm = pSym '!'
	
	pQstnm :: Parser Char Char
	pQstnm = pSym '?'
	
	pComma :: Parser Char Char
	pComma = pSym ','
	
	pInterWord :: Parser Char String
	pInterWord = pCommaSeq <|> pSomeWhiteSpace
	
	pCommaSeq :: Parser Char String
	pCommaSeq = (:) <$> pComma <*> pManyWhiteSpace
	
	pSentence :: Parser Char [String]
	pSentence = some (pWord <* many pInterWord) <* pEnd
	
	pSentences :: Parser Char [[String]]
	pSentences = some (pSentence <* pManyWhiteSpace)
	
	pExample = head . runParser pSentences $ "Hello World! Goodbye, World!"