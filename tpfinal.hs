somar::Int -> Int -> Int
somar a b = a + b

main = do 
	x <- getLine
	if (trim x == "0 0 0") then
		putStr "Execucao Terminou"
	else 
		putStr "Continua..."



trim::[Char]->[Char]
trim lista = 
	trimFinal(trimInicio lista)
	
trimInicio::[Char] -> [Char]
trimInicio (c:r)
	| [c] == " " = trimInicio r
	| otherwise = (c:r)

trimFinal::[Char] -> [Char]
trimFinal l = inverte(trimInicio (inverte l))
	
--Inverte lista recursão de cauda
-- Entrada "abc" -> Saida "cba"
inverte::[t] -> [t]
inverte lista = inverte' [] lista
	where 
	inverte' listaInvertida [] = listaInvertida
	inverte' listaInvertida (c:r) =
		inverte' (c:listaInvertida) r

--Passa 2 valores inteiros e retorna a substring
--que comeca na posicao 0 e vai ate a posicao 1

-- Entrada 0 1 "abc" -> Saida "a" 
substring::Int->Int->[Char]->[Char]
substring 0 0 _ = []
substring 0 end (c:r) = c:(substring 0 (end-1) r)
substring begin end (c:r) = substring (begin-1) end r


--Encontra posicao da primeira ocorrencia do elemento
--Entrada firstOc 'a' "abc" -> Saida 0
firstOc::(Eq t) => t -> [t] -> Int
--firstOc elem [] = -1
firstOc elem lista = firstOc' elem lista 0
	where
	firstOc' elem (c:r) pos
		| c == elem = pos
		| otherwise = firstOc' elem r (pos+1)

--Retorna um vetor de inteiros com todas as ocorrencias do elemento 
--desejado no vetor/string passado

--Entrada allOc 'a' "abca" -> Saida [0,3]
allOc::(Eq t) => t -> [t] -> [Int]
allOc elem lista = allOc' elem lista 0
	where 
	allOc' _ [] _ = []
	allOc' elem (c:r) pos
		| c == elem = pos:(allOc' elem r (pos+1))
		|otherwise = allOc' elem r (pos+1)


--Passa 2 mensagens do log de mensagens e checa se eh possivel fazer a uniao das duas
joinSMS:: [Char] -> [Char] -> [Int]-> 


