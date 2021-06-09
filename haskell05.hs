-- Prática 01 de Haskell
-- Nome: Bianca Sabrina Bublitz

-- 01 nao consegui
{-bmi :: Float -> Float -> String
bmi peso alt 
 let imc = peso / (alt^2)
  in 
   | imc <= 18.5 = "ABAIXO"
   | imc < 30 = "NORMAL"
   | imc >= 30 = "ACIMA"-}

-- 02
bmi' :: Float -> Float -> String
bmi' peso alt 
 | imc <= 18.5 = "ABAIXO"
 | imc < 30 = "NORMAL"
 | imc <= 18.5 = "ABAIXO"
 where imc = peso / (alt^2)

-- 03
cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
 let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr

cpfValid :: [Int] -> Bool
cpfValid cpf = (dv1 == cpf !! 9 && dv2 == cpf !! 10)
 where digits = take 9 cpf
       dv1 = cpfDV digits [10,9..]
       dv2 = cpfDV (digits ++ [dv1]) [11,10..]

-- 04
andTable :: [(Bool, Bool, Bool)]
andTable = [(x,y,z) | x <- [True, False], y <- [True, False], z <- [x == True && y == True]]