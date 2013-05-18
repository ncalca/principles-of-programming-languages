
-- 1
-- Sum elements of an array
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs

-- 2
-- Length
length' :: (Num b) => [a] -> b  
length' [] = 0 
length' (_:xs) = 1 + length' xs


-- 3
-- BMI - Indice massa corporea
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "Underweight!"  
    | bmi <= normal = "Just normal!"  
    | bmi <= fat    = "Fatty!"  
    | otherwise     = "A whale!"  
    where bmi = weight / height ^ 2  
        skinny = 18.5  
        normal = 25.0  
        fat = 30.0  

-- 4
-- Sum using fold
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

-- 4'
-- Sum using fold1
sum' :: (Num a) => [a] -> a
sum' xs = foldl1 (+) xs

--
-- Length strict
length' :: [t] -> Integer
length' xs = len xs 0 
        where 
        len [] k = k
        len (_:xs) k = let inc = k + 1
                       in seq inc len xs inc 

-- 5
-- Fibonacci using an infinite array
fibM = ((map fib' [0 ..]) !!)
    where
      fib' 0 = 0
      fib' 1 = 1
      fib' n = fibM (n - 1) + fibM (n - 2)


-- 6
-- List comprehensions
let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  


-- 7
-- Formula definition
infixr 9 :=>
data Fma = Boolean Bool | Atom String | And [Fma] | Or [Fma]
     | (:=>) Fma Fma | Iff Fma Fma
     deriving (Show, Eq)


-- 8
-- Quicksort
qsort []     = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]



