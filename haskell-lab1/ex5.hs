-- sgn :: Int->Int
-- sgn n= if n<0 then -1 else if n==0 then 0 else 1
absInt :: Int->Int
absInt n
 | n>=0=n 
 | otherwise = -n
min2Int :: (Int,Int)->Int
min2Int(a,b)= if a<=b then a else b
min3Int :: (Int,Int,Int)->Int
min3Int(a,b,c)
 | a<=b && a<=c = a
 | b<=a && b<=c = b
 | c<=a && c<=a = c
