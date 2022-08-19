{-
Camila Przendziuk Franco Felix
-}

{-
Questão número 1 
Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
-}
soma1 :: Int -> Int
soma1 x= x +1

{-Questão número 2 
Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
-}
sempre :: Num x => x -> Int
sempre x = 0

{-Questão número 3
Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
-}
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

{-Questão número 4
Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
-}
resto :: Int -> Int -> Int
resto x y = mod x y

{-Questão número 5
Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
-}
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior w x y z
  | w > x && w > y && w > z = w
  | x > w && x > y && x > z = x
  | y > w && y > x && y > z = y
  | otherwise = z

{-Questão número 6 
Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
-}
impar :: Int -> Int -> Bool
impar x y
  | mod prod 2 == 1 = True
  |otherwise = False
  where prod = x * y

{-Questão número 6.5
Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
-}
somaPar :: (Int, Int) -> Int
somaPar (x, y) = x + y

{-Questão número 7
Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦/2 +𝑧.
-}
equacao :: Double -> Double -> Double -> Double
equacao x y z = x^2 + y/2 + z

{-Questão número 8
Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: 
Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br). 
Observe  que  este  diagnóstico  é  meramente  estatístico  e  não 
tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico. 
-}
diagnostico :: Double -> Double -> String
diagnostico x y
  | res <= 17 = "Muito abaixo do peso"
  | res > 17 && res <= 18.49 = "Abaixo do peso"
  | res > 18.49 && res <= 24.99 = "Peso normal"
  | res > 24.99 && res <= 29.99 = "Sobrepeso"
  | res > 29.99 && res <= 34.99 = "Obesidade leve"
  | res > 34.99 && res <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade mórbida"
  where res = x / y^2

{-Questão número 9
Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. 
-}
bissexto :: Int -> Bool
bissexto x 
  | mod x 400 == 0 = True
  | mod x 100 == 0 = False
  | mod x 4 == 0 = True
  | otherwise = False

main = do
  putStrLn $ "Func. 1: Entrada 2; Ressultado: " ++ show (soma1 2)
  putStrLn $ "Func. 2: Entrada 3.4; Ressultado: " ++ show (sempre 3.4)
  putStrLn $ "Func. 3: Entrada 4.5 5.6 6.7; Ressultado: " ++ show (treco 4.5 5.6 6.7)
  putStrLn $ "Func. 4: Entrada 8 9; Ressultado: " ++ show (resto 8 9)
  putStrLn $ "Func. 5: Entrada 1.3 2.4 3.5 4.6; Ressultado: " ++ show (precoMaior 1.3 2.4 3.5 4.6)
  putStrLn $ "Func. 6: Entrada 5 5; Ressultado: " ++ show (impar 5 5)
  putStrLn $ "Func. 6.5: Entrada (6, 8); Ressultado: " ++ show (somaPar (6, 8))
  putStrLn $ "Func. 7: Entrada 7 8 9; Ressultado: " ++ show (equacao 7 8 9)
  putStrLn $ "Func. 8: Entrada 42.2 1.52; Ressultado: " ++ show (diagnostico 42.2 1.52)
  putStrLn $ "Func. 9: Entrada 2024; Ressultado: " ++ show (bissexto 2024)
