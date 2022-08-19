-- Aluno: Carlos Henrique Moreira dos Santos
import Text.Printf

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
-} 
soma1 :: Integer -> Integer
soma1 x = x + 1


{-
2. Escreva  uma  função  chamada  sempre  que,  não  importando  o  valor  de  entrada,  devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
-}
-- Sempre que a função for chamada, independente do tipo, ela retornará Int 0
sempre :: x-> Int
sempre x = 0


{-
3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
-}
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z


{-
4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros. 
-}
-- Aqui foi usada uma função do proprio Haskell chamada div, que retorna o divisão entre 2 números.
resto :: Integer -> Integer -> Integer
resto x y = x - y * (x `div` y)


{-
5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários. 
-}
-- A função faz duas verificações de maximo entre dois numeros, para depois escolher o maior entre os dois maiores de cada grupo.
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior x y z w = max (max x y) (max z w)


{-
6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar. 
-} 
-- Foi aproveitada a função anterior "resto" para fazer a verificação se um número é impar
impar :: Integer -> Integer -> Bool
impar x y = resto (x * y) 2 == 1


{-
Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros. 
-}
somaPar :: (Int,Int) -> Int
somaPar par = fst par + snd par


{-
7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦/2 +𝑧. 
-}
equacao :: Double -> Double -> Double -> Double
equacao x y z = x*x + y/2 + z


{-
8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.
-} 
--Como no enunciado está específicado que o aluno escreva somente o seu peso, sem a sua altura, foi utlizado uma média da altura dos jovens de 20 anos do Brasil.
diagnostico :: Double -> String
diagnostico peso =
  if peso/(1.68*1.68) < 17 then "Muito abaixo do peso"
  else if peso/(1.68*1.68) <= 18.49 then "Abaixo do Peso"
  else if peso/(1.68*1.68) <= 24.99 then "Peso Normal"
  else if peso/(1.68*1.68) <= 29.99 then "Sobrepeso"
  else if peso/(1.68*1.68) <= 34.99 then "Obesidade Leve"
  else if peso/(1.68*1.68) <= 39.99 then "Obesidade severa"
  else "Obesidade morbida"


{-
9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
  𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
        𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
              𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto. 
-}
-- Foi feita uma função que faz a verificação de cada uma das exceções.
bissexto :: Integer -> Bool
bissexto ano = resto ano 400 == 0 || (resto ano 4 == 0 && resto ano 100 /= 0) 


main = do
  --1
  printf "\nFunc. 1: entrada:%d; resultado:%d\n" (4::Int) (soma1 4)

  --2
  printf "\nFunc. 2: entrada:%d; resultado:%d\n" (2312::Int) (sempre 2312)

  --3
  printf "\nFunc. 3: entrada:%f %f %f; resultado:%f\n" (2.5::Double) (3.2::Double) (1.7::Double) (treco 2.5 3.2 1.7)

  --4
  printf "\nFunc. 4: entrada:%d %d; resultado:%d\n" (100::Int) (48::Int) (resto 100 48)

  --5
  printf "\nFunc. 5: entrada:%f %f %f %f; resultado:%f\n" (4.57::Double) (3.24::Double) (2.56::Double) (1.86::Double) (precoMaior 4.57 3.24 2.56 1.86)

  --6
  printf "\nFunc. 6: entrada:%d %d; resultado:%s\n" (4::Int) (3::Int) (show (impar 4 3))

  --6.5
  printf "\nFunc. 6.5: entrada:%d %d; resultado:%d\n" (20::Int) (32::Int) (somaPar (20,32))

  --7
  printf "\nFunc. 7: entrada:%f %f %f; resultado:%f\n" (2.5::Double) (3.0::Double) (4.5::Double) (equacao 2.5 3.0 4.5)

  --8
  printf "\nFunc. 8: entrada:%f; resultado:%s\n" (72::Double) (diagnostico 72)

  --9
  printf "\nFunc. 9: entrada:%d; resultado:%s\n" (2000::Int) (show (bissexto 2000))