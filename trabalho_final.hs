


-- Trabalho Final de Linguanges de Programação
-- Problema - Economizando SMS
-- Descrição do Trabalho - http://br.spoj.com/problems/SMSMG14/
-- Aluno: Rafael Batista Andrade
-- Professor:Bruno Schneider


import System.IO


-- Função main chama função mainAux passando o vetor de economias vazio, pois não leu nada ainda.

main = do
	mainAux []



-- Funcão MainAaux, onde ocorre a leitura das entradas

-- EconomiaTotal guarda todos os valores das economias em um vetor de tuplas da seguinte forma
-- [(eLorena1, eGustavo1), ... (eLorenaN,eGustavoN)]
-- Onde cada posição da tupla representa a economia de cada um em logs diferentes
-- o vetor total representa todos os históricos
mainAux economiaTotal = do
	entrada <- getLine
	let (qtdMsgs, valorLorena, valorGustavo) = inicializa entrada in
		if (qtdMsgs == 0) && (valorLorena == 0) && (valorGustavo == 0) then do
			putStrLn("")
			imprime economiaTotal
	 	else if (qtdMsgs < 4096) && (valorLorena < 81) && (valorGustavo < 81) then do
		 	ler qtdMsgs [] (0,0) (valorLorena,valorGustavo) economiaTotal
		else do 
			mainAux economiaTotal


--Imprime todas as economias do vetor [(eLorena1, eGustavo1), ... (eLorenaN,eGustavoN)]
--no seguinte formato:
--eLorena1 eGustavo1
--eLorena2 eGustavo2
-- ...
-- ...
--eLorenaN eGustavoN
imprime economiaTotal = do
	let primeiroElem = primeiroElemento economiaTotal in
		if (economiaTotal /= []) then do
			putStr (show (primeiro primeiroElem))
			putStr (" ")
			putStrLn (show (segundo primeiroElem))
			imprime (semPrimeiroElemento economiaTotal)
		else do 
			putStr("")
		

-- Caso a quantidade de linhas a ser lidas seja igual a 0 ele retorna pra função MainAux pra continuar a leitura dos históricos

-- Ler Parametro1 Paramentro2 Parametro3 Parametro4 Parametro5 onde:

-- Parametro1 = quantidade de linha a serem lidas nesses histórico
-- Parametro2 = última SMS lida no log
-- Parametro3 = quantidade de mensagens que poderiam ser economizadas
-- Parametro4 = valor da mensagem de gustavo e lorena
-- Parametro5 = Vetor de todas as economias
ler:: Int -> [Char] -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> IO ()
ler 0 _ (eLorena,eGustavo) (custoLorena, custoGustavo) economiaTotal= do
	-- Quando termina de ler ele chama a função MainAux com a economia desse histórico inseria no vetor economiaTotal
	mainAux (insere (eLorena * custoLorena, eGustavo * custoGustavo) economiaTotal)

ler qtdMsgs sms1 (eLorena,eGustavo) valores economiaTotal = do
	sms2 <- getLine
	let (juntou,novaSMS) = juntarSMS sms1 sms2
	-- Se puder juntar verificará de quem é o remetente e somará um no total de mensagens economizadas
	if (juntou) then do
		if (compara (getRemetente novaSMS) "Lorena") then do
			ler (qtdMsgs-1) novaSMS (eLorena +1,eGustavo) valores economiaTotal
		else do
			ler (qtdMsgs-1) novaSMS  (eLorena, eGustavo+1) valores economiaTotal
	else do
		ler (qtdMsgs-1) sms2 (eLorena,eGustavo) valores economiaTotal 



-- Inicializa os dados de entrada retornando uma tripla com qtd de linhas a ser lido, valor de SMS da empresa de Lorena e valor SMS do Gustavo
-- eg: inicializa ("5 10 11") = (5,10,11)
inicializa::[Char]->(Int,Int,Int)
inicializa x = (primeiroValor x,segundoValor x,terceiroValor x)
 
-- Recebe uma string e retorna o primeiro valor inteiro presente na na string
-- eg:  primeiroValor ("5 10 11") = 5
primeiroValor::[Char] -> Int
primeiroValor x = toInteiro (substring 0 (elemento 0 (	findAllOc ' ' x)) x)

-- Recebe uma string e retorna o segundo valor inteiro presente na string 
-- eg:  segundoValor ("5 10 11") = 10
segundoValor::[Char] -> Int
segundoValor x = toInteiro (substring  ((elemento 0 (findAllOc ' ' x))+1) (elemento 1 (findAllOc ' ' x)) x)

-- Recebe uma string e retorna o terceiro valor inteiro presente na string
-- eg:  primeiroValor ("5 10 11") = 11
terceiroValor::[Char] -> Int
terceiroValor x = toInteiro (substring ((elemento 1 (findAllOc ' ' x))+1) (tamanho x) x)



-- Recebe duas listas e retornam True caso sejam iguais ou false caso contrário
-- eg: compara "Gustavo" "Gustavo" = True
compara:: (Eq t) => [t] -> [t] -> Bool
compara [] [] = True
compara [] (c:r) = False
compara (c:r) [] = False
compara (c:r) (c2:r2)
	| c == c2 = compara r r2
	| otherwise = False

-- Recebe uma lista e retorna seu tamanho
-- eg: tamanho "Gustavo" = 7
tamanho::[t] -> Int
tamanho [] = 0
tamanho (c:r) = 1 + (tamanho r)

-- Recebe um SMS e retorna o seu texto
-- eg: getTexto "Lorena:Oi tudo bem?" = "Oi tudo bem?"
getTexto::[Char] -> [Char] 
getTexto sms = substring ((firstOc ':' sms)+1) ((tamanho sms)) sms

-- Recebe um SMS e retorna seu remetente
-- eg: getRemetente "Lorena:Oi tudo bem?" = "Lorena"
getRemetente::[Char] -> [Char]
getRemetente sms = substring 0 (firstOc ':' sms) sms

-- Recebe dois SMS's e retorna True caso sejam de mesmo remetentes
-- mesmoRemetente "Lorena:Oi." "Lorena:Tudo bem?" = True
mesmoRemetente::[Char] -> [Char] -> Bool
mesmoRemetente msg1 msg2 = compara (getRemetente msg1) (getRemetente msg2)

-- Recebe dois SMS's e retorna a sua concatenação
-- concatenaSMS "Lorena:Oi." "Lorena:Tudo bem?" = "Lorena:Oi. Tudo bem?"
concatenaSMS:: [Char] -> [Char] -> [Char]
concatenaSMS sms1 sms2 = concatena (getRemetente sms1) (':':(concatena (getTexto (sms1))) (' ':(getTexto (sms2))))

-- Recebe dois SMS's e retorna uma tupla onde o primeiro elemento é True se puder juntar os SMS's e o segundo elemento é o novoSMS gerado
-- juntarSMS "Lorena:Oi." "Lorena:Tudo bem?" = (True, "Lorena:Oi. Tudo bem?") 
-- Caso texto do SMS ultrapasse 160 caracteres ou são de remetentes diferentesretornará (False, []) 
juntarSMS::[Char] -> [Char] -> (Bool, [Char])
juntarSMS sms1 sms2 
	| (mesmoRemetente sms1 sms2 ) && (tamanho (getTexto (concatenaSMS sms1 sms2))) < 160 = (True,concatenaSMS sms1 sms2)
	| otherwise = (False, [])

--Retorna primeiro elemento de uma tupla
primeiro::(t,t) -> t
primeiro (fst, _) = fst

--Retorna segundo elemento de uma tupla
segundo::(t,x) -> x
segundo (_, snd) = snd


-- Recebe um numero inteiro X e uma lista L e retorna a posição de X em L
-- elemento 2 ["2","3","4"] = 4  (lista começa em 0) 
elemento::Int->[t]-> t
elemento 0 (c:r) = c
elemento x (_:r) = elemento (x-1) r

-- Retorna primeiro o primeiro elemento de uma lista L
-- primeiroElemento ["2","3","4"] = 2 
primeiroElemento::[t] -> t
primeiroElemento (c:r) = c

--Retorna lista sem o primeiro seu primeiro elemento
-- primeiroElemento ["2","3","4"] = ["3","4"] 
semPrimeiroElemento:: [t] -> [t]
semPrimeiroElemento (c:r) = r




-- Concatena 2 listas
-- eg: concatena "melan" "cia" = "melancia" 
concatena:: [t] -> [t] -> [t]
concatena lista [] = lista
concatena lista (c:r) = concatena (insere c lista) r


-- Insere no final de uma lista
-- eg: insere 'a' "melanci" = "melancia"
insere :: t -> [t] -> [t]
insere x [] = [x]
insere x (c:r) = c:(insere x r)

-- Retorna uma substring de uma string
-- e.g substring 0 5 melancia ira retornar melan
substring:: Int -> Int -> [Char] -> [Char]
substring _ _ [] = []
substring 0 x (c:r)  
	| x > 0 = c:substring 0 (x-1) r
	| x == 0 = []
substring y x (c:r) 
	| y > 0 = substring (y-1) (x-1) r

-- Recebe um elemento e uma lista de elementos
-- Retorna posicao da primeia ocorrencia do elemento
-- firstOc 'z' "mariazinha" = 5
firstOc::(Eq t) => t -> [t] -> Int
firstOc e (c:r)
	| e == c = 0
	| otherwise = 1 + firstOc e r
--encontra todas as ocorrencias 
findAllOc::  Char -> [Char] -> [Int]
findAllOc element list = findAllOc' element list 0
	where 
		findAllOc' _ [] _ = [] 
		findAllOc' element (c:r) position 
			| c == element = position:findAllOc' element r (position+1)
			| otherwise = findAllOc' element r (position+1)


-- Converte uma string em inteiro
-- eg: toInteiro "22" = 22
toInteiro::[Char] -> Int
toInteiro valor = read valor















