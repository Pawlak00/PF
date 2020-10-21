-- absInt :: Int->Int
-- absInt n= case (n>=0) of
--  True ->n
--  _  -> -n
-- isItTheAnswer :: String->Bool
-- isItTheAnswer a=case a of
--  "Love" -> True
--  _  -> False 
roots :: (Double, Double, Double) -> (Double, Double)
roots (a,b,c)=((-b-d)/e,(-b+d)/e)
 where d = sqrt (b * b - 4 * a * c)
       e = 2 * a
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a,b)=
 let d=sqrt(a^2+b^2)
 in (a/d,b/d)