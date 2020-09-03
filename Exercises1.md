1. (\a (\b (\c. c b a))) z z (\w (\v.w)) [beta]
(\b (\c. c b a)) [a := z] z (\w (\v.w))
(\b (\c. c b z)) z (\w (\v.w)) [beta]
(\c. c b z) [b := z] (\w (\v.w))
(\c. c z z) (\w (\v.w)) [beta]
(c z z) [c := (\w (\v.w))]
(\w (\v.w)) z z



2. (\x. (\y. x y y)) (\a. a) b
(\x. (\y. x y y)) (\a. a) b [beta]
(\y. x y y) [x := (\a. a)] b
(\y. (\a. a ) y y) b [beta]
((\a. a) y y) [y := b]
(\a. a) b b [beta]
(a)[a := b] b
b b


3. (\y. y) (\x. x x) (\z. z q)
(\y. y) (\x. x x) (\z. z q) [beta]
(y) [y := (\x. x x)] (\z. z q)
(\x. x x) (\z. z q) [beta]
(x x) [x := (\z. z q)]
(\z. z q)(\z. z q)[beta]
( z q) [z := \z. z q] 
(\z. z q) q [beta]
(z q) [ z := q]
q q

4. (\z. z) (\z. z z) (\z. z y)
(\z. z) (\z. z z) (\z. z y) [beta]
(z) [z := \z. z z] (\z. z y)
(\z. z z) (\z. z y) [beta]
(z z) [z := \z. z y]
(\z. z y) (\z.  z y)
y y


exclusive_or :: Boolean -> Boolean -> Boolean
exclusive_or p q  =
    if p == q
        then false
        else true

sumOrProd :: Boolean -> Int -> Int -> Int
sumOrProd cond a b = 
    if cond
        then x + y
        else x * y

concatInts :: Int -> Int -> Int -> String
concatInts x y z = show x <> show y <> show z

applyF :: (Int -> String) -> Int -> String
applyF fn x = fn x