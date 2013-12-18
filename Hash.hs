{--
Hash algorithm courtesy of the Data.HashTable module.
We defined it instead of importing it out of precaution, because the module is deprecated.
--}

module Hash where
	import Data.Bits
	import Data.Char
	import Data.Int
	import Data.List
	
	golden :: Int32
	golden = 1013904242 -- = round ((sqrt 5 - 1) * 2^32) :: Int32
	
	mulHi :: Int32 -> Int32 -> Int32
	mulHi a b = fromIntegral (r `shiftR` 32)
	   where r :: Int64
	         r = fromIntegral a * fromIntegral b
	
	hashInt32 :: Int32 -> Int32
	hashInt32 x = mulHi x golden + x
	
	hashString :: String -> Int32
	hashString = foldl' f golden
	   where f m c = fromIntegral (ord c) * magic + hashInt32 m
	         magic = 0xdeadbeef