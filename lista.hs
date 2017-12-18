--problema 1
--Defina uma função	recursiva para o cálculo de	 potência de dois	
--números inteiros,onde	o primeiro número	é elevado ao segundo. Não	
--se pode usar o operador de potência.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia b e = b * potencia b (e-1)


--problema 2
--Calcular o somatório dos elementos ímpares de	uma	lista de inteiros
somaImpares :: [Int] -> Int
somaImpares [] = 0
somaImpares (a:x)
        | (a `mod` 2) == 1 = a + somaImpares x
        | otherwise        = somaImpares x

		
--problema 3
--Substituir todos elementos de	um determinado valor de uma lista de	
--inteiros por um outro valor
substituir :: Int -> Int -> [Int] -> [Int]
substituir _ _ []    = []
substituir a b (z:c)
        | a == z        = [b] ++ substituir a b c
        | otherwise     = [z] ++ substituir a b c

		
--problema 4		
--Verificar se um número é primo.
primo :: Int -> Bool
primo 0 = False
primo 1 = False
primo a = not ( existeDivisor a [ i | i <- [2..(a-1)] ] )
-- se existir uma numero entre 1 e (a-1) que o resto da divisao
-- com a for 0
-- entao podemos dizer que esse numero a nao eh primo
-- caso contrario podemos dizer que eh primo
-- funcao que verifica se existe pelo menos UM divisor de um numero A em uma Lista de Inteiros
existeDivisor :: Int -> [Int] -> Bool
existeDivisor _ [] = False
existeDivisor a (b:c)
        | a `mod` b == 0 = True
        | otherwise      = existeDivisor a c
		

--problema 5		
--Verifique	se um número é perfeito, isto é, é igual a soma de seus	
--divisores (exceto	o próprio número).
perfeito :: Int -> Bool
perfeito 0 = True
perfeito a = semelhantes a ( somaLista (listaDivisores a [ i | i <- [1..(a-1)] ] ) )
--funcao que define se dois valores sao semelhantes
semelhantes :: Int -> Int -> Bool
semelhantes a b
        | a == b    = True
        | otherwise = False
-- funcao que soma um lista de inteiros
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a:b) = a + somaLista b
-- funcao que gera uma lista formada por elementos de uma outra lista
-- que sao divisores de um numero A
listaDivisores :: Int -> [Int] -> [Int]
listaDivisores _ [] = []
listaDivisores 0 d  = []
listaDivisores a (b:c)
        | a `mod` b == 0 = [b] ++ listaDivisores a c
        | otherwise      = listaDivisores a c

				
--problema 6
--Função que retorna uma lista com a representação em binário de um	
--número	inteiro.
binario :: Int -> [Int]
binario 0 = [0]
binario 1 = [1]
binario a = binario ( a `div` 2) ++ [ a `mod` 2 ] 


--problema 7
--Verificar	se todos os	elementos de uma lista são	distintos.
distintos :: [Int] -> Bool
distintos [] = True
distintos a = not ( true (tamanhoLista a) (igual a a) )
--funcao que diz se existe mais de Int trues em um lista
--a ideia é que se .. houver um numero maior de true que o tamanho da lista
--podemos dizer que a lista não é composta por elementos distintos
true :: Int -> [Bool] -> Bool
true a [] = False
true a b
        | a < (contaTrue b)  = True
        | otherwise        = False
--funcao que conta ocorrências de Trues
contaTrue :: [Bool] -> Int
contaTrue [] = 0
contaTrue (a:b)
        | a == True = 1 + contaTrue b
        | a == False = 0 + contaTrue b
--funcao que verifica ocorrencias de elementos
--em outra lista
igual :: [Int] -> [Int] -> [Bool]
igual []  y = [False]
igual  z [] = [False]
igual (a:b) c = (makeListaExiste a c) ++ igual b c
--funcao que cria uma lista de Booleanos
--a funcao corre a lista procurando pela
--ocorrencia de o numero passado
--se existir na posicao analisada
--atribui True , senao atribui False
makeListaExiste :: Int -> [Int] -> [Bool]
makeListaExiste _ [] = [False]
makeListaExiste a (b:c)
        | a == b    = [True] ++ makeListaExiste a c
        | otherwise = [False] ++ makeListaExiste a c
-- funcao que retorna o tamanho de uma lista
tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista (a:b) = 1 + tamanhoLista b


--problema 8
--Verificar	se duas listas são disjuntas
disjuntas :: [Int] -> [Int] -> Bool
disjuntas    []  [] = False
disjuntas     a  [] = True
disjuntas    []   b = True
disjuntas (a:b)   c
        | existeNumLista a c == True  = False
        | existeNumLista a c == False = disjuntas b c
--funcao que diz se um elemento existe na lista
existeNumLista :: Int -> [Int] -> Bool
existeNumLista _ [] = False
existeNumLista a (b:c)
        | a == b    = True
        | otherwise = existeNumLista a c
		

--problema 9
--Verificar se uma lista de	inteiros é palíndromo.
palindromo :: [Int] -> Bool
palindromo [] = True
palindromo a  = comparaListas a z
        where
                z = inverteLista a
--funcao compara listas
comparaListas :: [Int] -> [Int] -> Bool
comparaListas    []    [] = True
comparaListas     a    [] = False
comparaListas    []     b = False
comparaListas (a:b) (c:d)
        | a == c    = comparaListas b d
        | otherwise = False
--funcao que inverte uma lista
inverteLista :: [Int] -> [Int]
inverteLista    []  =  []
inverteLista (a:b)  = inverteLista b ++ [a]


--questao 10
--Calcular todas as somas parciais de uma lista	de inteiros.
somaParciais :: [Int] -> [Int]
somaParciais    [] = []
somaParciais    a = make ( inverteLista a)
--constroi a lista com a soma parciais
make :: [Int] -> [Int]
make [] = []
make (a:b) = make b ++ [somaElementosLista ([a] ++ b)]
--inverte a lista
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:b) = inverteLista b ++ [a]
--soma elementos de uma lista
somaElementosLista :: [Int] -> Int
somaElementosLista [] = 0
somaElementosLista (a:b) = a + somaElementosLista b


--questao 11
--Linearizar uma lista de listas de inteiros
linearizar :: [[Int]] -> [Int]
linearizar []          = []
linearizar (a:b)       = a ++ linearizar b


--questao 12
--Deslocar todos elementos de uma lista de inteiros k posições para a esquerda.
shift :: Int -> [Int] -> [Int]
shift _ [] = []
shift a b = (tiraNprimeiros a b) ++ (mantemNprimeiros a b)
mantemNprimeiros :: Int -> [Int] -> [Int]
mantemNprimeiros _ [] = []
mantemNprimeiros a (b:c)
        | a == 0      = mantemNprimeiros a []
        | otherwise   = [b] ++ mantemNprimeiros (a-1) c
tiraNprimeiros :: Int -> [Int] -> [Int]
tiraNprimeiros _ [] = []
tiraNprimeiros a (b:c)
        | a == 0    = [b] ++ c
        | otherwise = tiraNprimeiros (a-1) c

		
--problema 13
--Remover os n últimos	elementos de	uma	lista de inteiros.
removerFim :: Int -> [Int] -> [Int]
removerFim _ [] = []
removerFim 0  z = z
removerFim a b = inverteLista ( removeN a (inverteLista b) )
--funcao que retira os N primeiro de uma Lisa
removeN :: Int -> [Int] -> [Int]
removeN _ [] = []
removeN 0  z = z
removeN a (b:c) = [] ++ removeN (a-1) c
--funcao que inverte uma lista
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:b) = inverteLista b ++ [a]


--problema 14
--Dadas	duas listas	ordenadas de forma crescente,obter a	
--lista	ordenada resultante	da intercalação delas.
intercalar :: [Int] -> [Int] -> [Int]
intercalar [] [] = []
intercalar  a [] = qsort a
intercalar []  b = qsort b
intercalar  a  b = qsort ( a ++ b )
qsort :: [Int] -> [Int]
qsort [] = []
qsort (a:x) = qsort [ b | b <- x, b <= a ] ++ [a] ++ qsort [ b | b <- x, b > a ]


--problema 15
--Desenvolver uma solução para um quiosque de saque eletrônico que,	
--para um determinado valor,deve entregar o menor número de cédulas	
--de R$1, R$5, R$10, R$50 e R$100, da menor para a maior.
trocar :: Int -> [Int]
trocar n = qsort (make n)
--funcao que faz a lista com o valores de cedulas
make :: Int -> [Int]
make 0 = []
make n
        | modN n 100 == 0 = make (n-100) ++ [100]
        | modN n 50  == 0 = make (n-50)  ++ [50]
        | modN n 10  == 0 = make (n-10)  ++ [10]
        | modN n 5   == 0 = make (n-50)  ++ [5]
        | otherwise       = make (n-1)   ++ [1]
--funcao de modulo
modN :: Int -> Int -> Int
modN a  b = a `mod` b
qsort :: [Int] -> [Int]
qsort [] = []
qsort (a:x) = qsort [ b | b <- x, b <= a ] ++ [a] ++ qsort [ b | b <- x, b > a ]
		