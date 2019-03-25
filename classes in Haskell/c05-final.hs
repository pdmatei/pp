

-- basic datatype inspired from the previous lecture

data MExpr a = M [[a]]

-- making a Matrix show-able:
-- notice the requirement that a is a member of class Show, in order to make MExpr showable
instance (Show a) => Show (MExpr a) where
  show (M l) = toString l 
                  where toString = (foldr (\x y-> x++"\n"++y) "") .
                                   (map f)
                        f = (foldr (\x y->x++" "++y) "") .
                            (map show)

-- a different implementation of the show method was done in the previous lecture
-- the implementation strategy produces an output which can be rendered in Latex

-- making a Matrix comparable
-- again, notice the restriction: a must be comparable in order to compare values of type MExpr a
instance (Eq a) => Eq (MExpr a) where
      M l == M l' = f (zipWith (\x y-> f ((zipWith (==)) x y)) l l')
                  where f = foldr (&&) True 

-- making the Container (type constructor) MExpr a Functor, i.e. an object on which a function
-- may be applied
instance Functor MExpr where
  fmap f (M l) = M $ map (map f) l


m1 = M $ [[1,2],[3,4]]



