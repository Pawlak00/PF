-- Piotr Pawłowski 
sqr :: Double -> Double
sqr x=x*x
vec2DLen :: (Double,Double)->Double
vec2DLen (x,y)=sqrt(x^2+y^2)
vec3DLen :: (Double,Double,Double)->Double
vec3DLen (x,y,z)=sqrt(x^2+y^2+z^2)
swap :: (Int,Char)->(Char,Int)
swap(a,b)=(b,a)
threeEqual :: (Int,Int,Int)->Bool
threeEqual (x,y,z)=x==y && y==z
triArea :: (Double,Double,Double)->Double
triArea(a,b,c)=sqrt((a+b+c)*(a+b-c)*(a-b+c)*(-a+b+c))*0.25
sgn :: Int->Int
sgn n= if n<0 then -1 else if n==0 then 0 else 1
absInt :: Int->Int
absInt x= if x<0 then x*(-1) else x
sgn :: Int->Int
sgn n
 | n<0 = -1
 | n>0 = 1
 | otherwise = 0
min3Int	:: (Int,Int,Int)->Int
min3Int(a,b,c)
 | a<=b && a<=c = a
 | b<=a && b<=c = b
 | c<=a && c<=a = c
or' :: (Bool,Bool)->Bool
or' (False,False)=False
or' _  = True
and' :: (Bool,Bool)->Bool
and' (True,True)=True
and' _  =False
nand' :: (Bool,Bool)->Bool
nand' (True,True)=False
nand' _  =True
xor' :: (Bool,Bool)->Bool
xor' (False,False)=False
xor' (True,True)=False
xor' _  =True
not' :: Bool->Bool
not' b=case b of
 True->False
 False->True
or' :: (Bool,Bool)->Bool
or' (a,b)= case a==False && b==False of
 True->False
 False->True
and' :: (Bool,Bool)->Bool
and' (a,b)= case a==False || b==False of
 True->False
 False->True 
nand' :: (Bool,Bool)->Bool
nand' (a,b)=case a==True && b==True of
 True->False
 False->True
xor' :: (Bool,Bool)->Bool
xor' (a,b)=case a==b of
 True->False
 False->True
{-
	niektore funkcje moga sie powtarzac
	wiec trzeba uwazac
-}
unitVec2D :: (Double, Double) -> (Double, Double)--ta funkcja oblicza wersor wektora
unitVec2D (a,b)=(a/d,b/d)
 where d=sqrt(a^2+b^2)
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a,b)=
 let d=sqrt(a^2+b^2)
 in (a/d,b/d)
roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
    where { d = sqrt (b * b - 4 * a * c); e = 2 * a }
f x = x*x + 2*x + 1
f1 x y= if y>x then y*y else x*x
f2 x y = if x>y then x else y
f3 x y = x++y
f4 x y= read x + read y
f5 x y = show x ++ show y
f6 (x1 ,y1) (x2,y2)=(x1-x2)**2 + (y2-y1)**2 
f7 x= if x<0 then -x else x
f8 (x1,y1) (x2,y2)= ((f7 (x1-x2)) + (f7 (y1-y2)))
f9 (x1,y1) (x2,y2) = [(x1*2,y1*2), (x2*2,y2*2)]  