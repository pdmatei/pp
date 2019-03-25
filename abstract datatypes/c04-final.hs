{- E1: Make a matrix showable:
   
   make each element a string
   make each list of strings a \n-terminated string
   make each list of strings a string


 -}

sh :: (Show a) => [[a]] -> String
sh = (foldr (\x y-> x++"\n"++y) "") .
     (map f)
			where f = (foldr (\x y->x++" "++y) "") .
					  (map show)

sh' :: (Show a) => [[a]] -> String
sh' m = "\\left(\\begin{array}{"++(take cols (repeat 'c'))++"}\n"++(format m)++"\\end{array}\\right)"
        where cols = length (head m)
              format = g "\\\\" (g "& " show)
              g sep f = reverse . tail . tail . reverse .        -- ineficient but preserves style
                        (foldr (\x y->x++sep++y) "") .
                        (map f)



{- 
	We would like to use plus to add both constants and other matrices.
    We can naturally achieve this by extending the types of values of our type.
    Lets call it ML type:

-}


{-
   a lot of matrix transformations require applying a function on all values of the matrix
   map f (M [[...]])

   it would be nice to be able to use 'map' in this way for our ML type, in the same way we
   do it for lists.


-}

{- this is our API -}
mult' :: Num a => [[a]] -> [[a]] -> [[a]]
mult' m1 m2 = map (\line -> map (\col -> foldr (+) 0 (zipWith (*) line col) ) (tr' m2) ) m1

tr' ([]:_) = []
tr' m = (map head m):(tr' (map tail m))


{- Now we can redesign a lot of our implementations-}


instance Functor MExpr where
    fmap f (M' m) = M' (fmap (fmap f) m)
    fmap f (C' c) = C' (f c)

pplus :: (Num a) => MExpr a -> MExpr a -> MExpr a
pplus (M' m1) (M' m2) = M' $ zipWith (zipWith (+)) m1 m2
pplus (M' m) (C' c) = pplus (C' c) (M' m)
pplus (C' c) x = fmap (+c) x

mmult :: (Num a) => MExpr a -> MExpr a -> MExpr a
mmult (M' m1) (M' m2) = M' $ mult' m1 m2
mmult (C' c) x = fmap (*c) x
mmult x y = mmult y x

ttr (M' m) = M' $ tr' m

{-
    Now, in many ML applications, Matrix operations can be reordered for efficiency.
    For instance, (a `mult` X) `plus` b takes 2 matrix traversals. It could be replaced 

    Similarly, (aXb) could be optimized to abX, and (aX) (bY) could be optimized to ab(XY)

    How to implement this?

    a*X*b*c
    a*bc*X

-}

data MExpr a = C' a | M' [[a]] | Plus (MExpr a) (MExpr a) | Mult (MExpr a) (MExpr a) | Tr (MExpr a)

-- we must keep the order in which the matrices are multiplied
-- we must bring constants outside
-- we must treat plus expressions as 'parentheses'


instance (Show a) => Show (MExpr a) where
   show = she'

she' :: (Show a) => (MExpr a) -> String
she' (C' x) = show x
she' (M' x) = sh' x
she' (Tr e@(Mult _ _)) = "\\left["++(show e)++"\\right]^T"
she' (Tr e@(Plus _ _)) = "\\left["++(show e)++"\\right]^T"
she' (Tr e) =(show e)++"^T"
she' (Plus e e') = (show e)++"+"++(show e')
she' (Mult e@(Plus _ _) e'@(Plus _ _)) = "\\left["++(show e)++"\\right]"++"*"++"\\left["++(show e')++"\\right]"
she' (Mult e@(Plus _ _) e') = "\\left["++(show e)++"\\right]"++"*"++(show e')
she' (Mult e e'@(Plus _ _)) = (show e)++"*"++"\\left["++(show e')++"\\right]"
she' (Mult e e') = (show e)++"*"++(show e')





-- compute :: (MExpr a) -> (ML a)
reorder =  pullCt . dist . pushTr  
        where 
              pushTr (Tr (e `Plus` e')) = (pushTr (Tr e)) `Plus` (pushTr (Tr e'))
              pushTr (Tr ((C' x) `Mult` e')) = (C' x) `Mult` (pushTr (Tr e'))
              pushTr (Tr (e `Mult` e')) = pushTr ((Tr e') `Mult` (Tr e))
              pushTr (Tr (Tr e)) = pushTr e
              pushTr (Tr (C' x)) = (C' x)
              pushTr (Tr e) = Tr (pushTr e) 
              pushTr (e `Plus` e') = (pushTr e) `Plus` (pushTr e')
              pushTr (e `Mult` e') = (pushTr e) `Mult` (pushTr e')
              pushTr x = x
              dist ((e1 `Plus` e2) `Mult` e) = (dist (e1 `Mult` e)) `Plus` (dist (e2 `Mult` e))
              dist (e `Mult` (e1 `Plus` e2)) = (dist (e `Mult` e1)) `Plus` (dist (e `Mult` e2))
              dist (e `Mult` e') = (dist e) `Mult` (dist e')
              dist (e `Plus` e') = (dist e) `Plus` (dist e')
              dist x = x
              pullCt (Mult e (C' x)) = Mult (C' x) (pullCt e) -- bring constant forward
              pullCt (Mult e e') =
                        case pullCt e' of
                            (Mult (C' x) ine) -> Mult (C' x) (pullCt (Mult e ine)) -- e * e' -> e * (c * e') 
                            re -> Mult (pullCt e) re
              pullCt (Plus e e') = Plus (pullCt e) (pullCt e')
              pullCt (Tr e) = Tr (pullCt e)
              pullCt x = x



compute (((C' x) `Mult` (C' y)) `Mult` e) = compute ((C' (x*y)) `Mult` e)
compute (((C' a) `Mult` e) `Plus` (C' b)) = fmap (\x-> a*x+b) (compute e)
compute (Tr e) = ttr (compute e)
compute (e `Mult` e') = (compute e) `mmult` (compute e')
compute (e `Plus` e') = (compute e) `pplus` (compute e')
compute x = x





m = [[1,2],[3,4]]

m2 = M' [[1,2],[3,4]]
m3 = M' [[0,1],[1,0]]

e1 = m2 `Mult` m3 `Mult` (C' 2)
e2 = m2 `Mult` (C' 3) `Mult` m3 `Mult` (C' 2)
e3 = (((C' 1) `Mult` m2) `Plus` (C' 2)) `Mult` (Tr (((C' 1) `Mult` m2) `Plus` (C' 2)))
e5 = (m2 `Mult` (C' 2)) `Mult` (m3 `Mult` (C' 1))

m4 = M' [[1,2],[3,4], [5,6]]
m5 = M' [[1,0, 0],[0,0,1]]

e4 = (((C' 1) `Mult` m4) `Plus` (C' 2)) `Mult` (Tr ((m5 `Mult` m4) `Plus` (C' 2)))


header = "\\documentclass[11pt]{article}\
\ \\title{Brief Article} \
\ \\begin{document} \ 
\ \\maketitle \
\ "

footer = "\\end{document}"

putLatex l = 
    do writeFile "Text.tex" (header++( ((foldr (++) "\n").(map (\x-> "\\["++(show x)++"\\]"))) l)++footer)




