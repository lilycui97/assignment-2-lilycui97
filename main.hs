--recaMan :: Int -> Int
--recaMan 0 = 0
--recaMan n
--    |prevElem - n <= 0 = prevElem + n
--    |(prevElem - n) `elem` recaSeq(n-1) = prevElem + n
--   |otherwise = prevElem - n
--    where prevElem = recaMan(n-1)

--recaSeq :: Int -> [Int]
--recaSeq 0 = [0]
--recaSeq n = map recaMan[0..n]

--recaList :: [Int] -> [Int]
--recaList n = [recaMan x|x<-n]

recaSeq :: Int -> [Int]
recaSeq 0 = [0]
recaSeq n 
    |prevElem - n <= 0 = prevSeq ++ [prevElem + n]
    |(prevElem - n) `elem` prevSeq = prevSeq ++ [prevElem + n]
    |otherwise = prevSeq ++ [prevElem - n]
    where prevSeq = recaSeq(n-1)
          prevElem = last(prevSeq)

recaMan :: Int -> Int
recaMan 0 = 0
recaMan n = last(recaSeq n)

recaList :: [Int] -> [Int]
recaList n = [recaMan x|x <- n]



