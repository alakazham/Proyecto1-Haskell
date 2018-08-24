-- Ejercicio 2 
-- 1)
--a)
esCero :: Int -> Bool
esCero x = x == 0
-- b)
esPositivo :: Int -> Bool 
esPositivo x = x>0 
-- c) 
esVocal :: Char -> Bool
esVocal x = x == 'a'|| x == 'e'|| x== 'i' || x == 'o' || x == 'u'  
 
-- 2)
-- a) 
paraTodo :: [Bool] -> Bool 
paraTodo [] = True
paraTodo (x:xs) = x == True && paraTodo xs   

-- b)
sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs

-- c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- d)
factorial :: Int -> Int 
factorial 0 = 1 
factorial x = x * factorial (x-1)

--e)
promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)

-- 3) 
pertenece :: Int -> [Int] -> Bool 
pertenece x [] = False  
pertenece x (y:ys) = x == y || pertenece x ys 

--4)
encuentra :: Int -> [(Int,String)] -> String
encuentra x [] = ""
encuentra x ((y1, y2):ys) | x == y1 = y2
                          | otherwise = encuentra x ys 

--5) 

--b)
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] p = True 
paraTodo' (x:xs) p | p x == True = paraTodo' xs p 
                   | p x == False = False

--b) 
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] p = False
existe' (x:xs) p = p x == True || existe' xs p 

--c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] p = 0
sumatoria' (x:xs) p = p x + sumatoria' xs p 

--d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] p = 1
productoria' (x:xs) p = p x * productoria' xs p

--6)
paraTodo2 :: [a] -> (a -> Bool) -> Bool
paraTodo2 xs p = paraTodo' xs p

--7) 
todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs (\x -> x `mod` 2 == 0) 

primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA x (y:ys) | x == y = y:(primIgualesA x ys)
                      | x /= y = [] 
                          






