
import System.IO
somar::Int->Int->Int
somar a b = a + b


main = do   
    handle <- openFile "arquivo.txt" ReadMode  
    contents <- hGetContents handle 
    --contentesReverse <- reverse contents
    putStr (show (findAllOc '\n' contents))
    putStr contents
    hClose handle

-- Retorna uma substring de uma string
-- e.g substring 0 5 melancia ira retornar melan
substring:: Int -> Int -> [Char] -> [Char]
substring 0 x (c:r)  
	| x > 0 = c:substring 0 (x-1) r
	| x == 0 = []
substring y x (c:r) 
	| y > 0 = substring (y-1) x r

findAllOc::  Char -> [Char] -> [Int]
findAllOc element list = findAllOc' element list 0
	where 
		findAllOc' _ [] _ = [] 
		findAllOc' element (c:r) position 
			| c == element = position:findAllOc' element r (position+1)
			| otherwise = findAllOc' element r (position+1)








