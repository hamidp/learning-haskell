--
-- Learn you a haskell.
-- 

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height
	| bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs =
	[bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  


head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' (x:[]) = x
maximum' (x:xs) =
  maxAcc xs x
  where maxAcc [] a = a
        maxAcc (y:ys) a
            | y >= a = maxAcc ys y
            | otherwise = maxAcc ys a