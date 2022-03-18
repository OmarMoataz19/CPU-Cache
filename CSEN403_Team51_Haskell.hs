data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
-----------------------------------------------------Other Methods----------------------------------------------
zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (x:xs) (y:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

removeLastN string n | n> 0 = removeLastN (init string) (n-1)
					 | otherwise = string


toInt :: Float -> Int
toInt = round

getDataMem a []=error "Invalid index" 
getDataMem a (x:xs)| a==0 = x
				   | otherwise = getDataMem (a-1) xs
				   
getDataMem2 a []=error "Invalid index" 
getDataMem2 a [(x:xs)]| a==0 = x
				      | otherwise = getDataMem (a-1) xs
				   
numDigits 0 = 0
numDigits n = toInteger (round (logBase 10 (fromIntegral n)) + 1)
				   
convertStoI string = read string :: Int

replaceDirect 0 d tag (x:xs) = (It (T tag) (D d) True 0):xs
replaceDirect n d tag (x:xs) = x:replaceDirect (n - 1) d tag xs

flatten = concat
checkIfFalse [] = False
checkIfFalse ((It (T tag) (D d) True n):xs) = checkIfFalse xs
checkIfFalse ((It (T tag) (D d) False n):xs) = True

getFalseIndex ((It (T tag) (D d) False n):xs) = 0
getFalseIndex (x:xs) = 1 + getFalseIndex xs

incTrue [] = []
incTrue ((It (T tag) (D d) vald ord):xs) = if (vald == True)
											then (It (T tag) (D d) vald (ord + 1):incTrue xs)
										 else
											(It (T tag) (D d) vald ord):incTrue xs
											
highestOrder [] = 0											
highestOrder ((It (T tag) (D d) vald n):xs) = max n (highestOrder xs)

highestOrderIndex h ((It (T tag) (D d) True n):xs) = if (n == h)
														then 0
													else 
														1 + (highestOrderIndex h xs)
														
-----------------------------------------------------General----------------------------------------------

convertBinToDec :: Integral a => a -> a
convertBinToDec x = convertBinToDecHelper x 0
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper x c = if (mod x 10 == 0)
								then convertBinToDecHelper (div x 10) (c + 1)
							else 
								(2 ^ c) + convertBinToDecHelper (div x 10) (c + 1)

replaceIthItem :: t -> [t] -> Int -> [t]
replaceIthItem item (x:xs) 0 = (item:xs)
replaceIthItem item (x:xs) index = (x:replaceIthItem item xs (index - 1))

splitEvery :: Int -> [a] -> [[a]]
splitEvery n l = takeNth n (splitEveryHelpr n n l [])

splitEveryHelpr _ _ [] _ = []
splitEveryHelpr n 0 l acc = splitEveryHelpr n n l []
splitEveryHelpr n k (x:xs) acc = (acc ++ [x]):splitEveryHelpr n (k - 1) xs (acc ++ [x])

takeNth n (x:xs) = takeNthHelper n n (x:xs)
takeNthHelper _ _ [] = []
takeNthHelper n 1 (x:xs) = x:takeNthHelper n n xs
takeNthHelper n k (x:xs) = takeNthHelper n (k - 1) xs

logBase2 0 = 0
logBase2 1 = 0
logBase2 num = 1 + logBase2 (div num 2)


getNumBits numOfSets "fullyAssoc" cache = 0
getNumBits numOfSets "setAssoc" _ = logBase2 numOfSets
getNumBits _ "directMap" cache =  logBase2 (length cache)


fillZeros :: [Char] -> Int -> [Char]
fillZeros s 0 = s
fillZeros s n = fillZeros ('0':s) (n - 1)


-----------------------------------------------------Convert----------------------------------------------
convertAddress :: (Integral b1, Integral b2) => b1 -> b2 -> [Char] -> (b1, b1)
convertAddress binAddress bitsNum "directMap" = ((div binAddress l), (mod binAddress l))
                                                 where
                                                 l= 10^bitsNum	
convertAddress binAddress bitsNum "fullyAssoc" =(binAddress,0)

convertAddress binAddress bitsNum "setAssoc" = ((div binAddress l), (mod binAddress l))
                                                 where
                                                 l= 10^bitsNum	

-----------------------------------------------------GetData----------------------------------------------
getDataFromCache :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a
getDataSetHelp stringAddress [] 0 hop = NoOutput
getDataSetHelp stringAddress ((It (T t) (D d) b n):xs) 0 hops =if (convertStoI stringAddress==t) && b==True then Out(a, x) else getDataSetHelp stringAddress xs  0 (hops+1)
																		where
																			a = d
																			x = hops

getDatafullHelp stringAddress [] "fullyAssoc" 0 hop = NoOutput

getDatafullHelp stringAddress ((It (T t) (D d) b n):xs) "fullyAssoc" 0 hops  = if (convertStoI stringAddress==t) && b==True then Out(a, x) else getDatafullHelp stringAddress  xs "fullyAssoc" 0 (hops+1)
																		where
																			a = d
																			x = hops
																			

getDataFromCache stringAddress cache "directMap" bitsNum | tag==a && vbit==True = Out (datas,0)
														 |otherwise = NoOutput
														 where
															(a, b)= convertAddress (convertStoI stringAddress) bitsNum "directMap"
															idx= convertBinToDec b
															(It (T tag) (D datas) vbit o) = ( getDataMem idx cache)													

getDataFromCache stringAddress [] "fullyAssoc" _ = NoOutput
																			
getDataFromCache stringAddress ((It (T t) (D d) b n):xs) "fullyAssoc" z =getDatafullHelp stringAddress ((It (T t) (D d) b n):xs) "fullyAssoc" 0 0 


getDataFromCache stringAddress [] "setAssoc" _ = NoOutput

getDataFromCache stringAddress cache "setAssoc" bitsNum  = getDataSetHelp tag targeted 0 0
														where
															tag = removeLastN stringAddress bitsNum 
															splittedCache = (splitEvery (div (length cache) (2 ^ bitsNum)) cache)
															targeted = getDataMem idx splittedCache
															(a, b) = convertAddress (convertStoI stringAddress) bitsNum "setAssoc"
															idx = convertBinToDec b






-----------------------------------------------------Replace----------------------------------------------
replaceInCache tag idx memory oldCache "directMap" bitsNum = (d, replaceDirect (convertBinToDec idx) d tag oldCache)
															where d = getDataMem (convertBinToDec (convertStoI (show tag ++ l))) memory
															      l = fillZeros (show idx) (bitsNum - (length (show idx)))
															
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = if ((checkIfFalse oldCache) == True)
																then (d, replaceDirect (getFalseIndex oldCache) d tag (incTrue oldCache))
															  else
																(d, replaceDirect (highestOrderIndex (highestOrder oldCache) oldCache) d tag (incTrue oldCache))
															where d = getDataMem (convertBinToDec tag) memory
															
replaceInCache tag idx memory oldCache "setAssoc" bitsNum = if (checkIfFalse targeted == True) 
																then (d, flatten (replaceIthItem newSet1 splittedCache idx))
															else
																(d, flatten (replaceIthItem newSet2 splittedCache idx))
															where
																d = getDataMem (convertBinToDec (convertStoI (show tag ++ show idx))) memory
																splittedCache = (splitEvery (div (length oldCache) (2 ^ bitsNum)) oldCache)
																targeted = getDataMem idx splittedCache
																newSet1 = replaceDirect (getFalseIndex (getDataMem idx splittedCache)) d tag (incTrue targeted)
																newSet2 = replaceDirect (highestOrderIndex (highestOrder (targeted)) (targeted)) d tag (incTrue targeted)

-----------------------------------------------------IN ALL----------------------------------------------
getData stringAddress cache memory cacheType bitsNum
							| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
							| otherwise = (getX x, cache)
								where
									x = getDataFromCache stringAddress cache cacheType bitsNum
									address = read stringAddress:: Int
									(tag, index) = convertAddress address bitsNum cacheType
									getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
														where
															bitsNum = round(logBase2 numOfSets)
															(d, updatedCache) = getData addr cache memory cacheType bitsNum
															(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets