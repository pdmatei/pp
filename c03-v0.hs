import Graphics.EasyPlot
import Data.List

{- This exercise is taken from Andrew Ng's ML course -}

{- Suppose you want to start a food truck business. You want to decide how
   to explore cities to ensure profit.
   You gather a dataset containing city population / profit.
   
   -}


{- E1: Parse the dataset-}

{- a. Splitting function -}
splitBy :: Char -> String -> [String]
splitBy _ [] = [[]]
splitBy c (x:xs) 
    | x == c = []:rest
    | otherwise = (x:(head rest)):(tail rest)
        where rest = splitBy c xs

{-
    b. Assembling the parsing
    
    map (\s->let [x,y] = splitBy ',' s in (x,y)) $ splitBy ' ' l 
   
    make it more elegant 

    (map trans) . (splitBy ' ')
        where trans = (\[x,y]->(x,y)) . (splitBy ',')

    include reading the double
-}

makeds = (map trans) . (splitBy ' ')
        where trans = (\[x,y]->(x,y)) . 
                      (map (\i->(read i)::Double)) . 
                      (splitBy ',')


{- Visualising the dataset and better understanding the problem: -}
plot1 = plot' [Interactive] Aqua $ 
        [ Data2D [Title "Truck productivity per population", Color Red] [] ds ]

{- X axis: population in 10.000 ppl
   Y axis: profit in $10.000 -}


{- E2: Average profit & average population from ds

   single pass for the computation:
   this is useful since many ML algorithms NORMALIZE their values by dividing
   w.r.t. the average (to avoid working with large values)
-}

avg l = total / no
    where (total,no) = foldr (\x (s,n)->(s+x,n+1)) (0,0) l


{- If we examine the dataset and try to predict a value, we observe
   that a reasonably-good function (model in ML) is a linear function.

   Let us try to plot it.

   -}    
f a b = \x -> a*x + b

discretize f rng = map (\x->(x,f x)) rng

-- we simply use the interval [0, 0.5, 1, 1.5, ...]
fds = discretize (f 1.5 (-6.0)) (map (/2) [0..50])

plot2 = plot' [Interactive] Aqua $ 
        [ Data2D [Title "Truck productivity per population", Color Red] [] ds , 
          Data2D [Title "A model", Color Blue] [] fds ]

{- We have ilustrated the function 2x+1 which looks like a poor model.
   How should a better model be?

   After a few tentative tries, we may find that 1.5x-6.0 is a good model, hence
   a=1.5 and b=-6.0 are good parameters. 

   It may be the case that a better model exists, e.g. a quadratic function.
   But we do not go there.

   The aim of a ML algorithm is to AUTOMATICALLY discover good model parameters.
   The aim of the ML developer is to discover a good model!!

   Let us consider the squared error cost function:

   C(a,b) = 1/m sum (M(xi) - yi)^2

   Vectorial form:
      (a*X+b - Y)*(a*X+b - Y)^T

   -} 


ds = makeds dsraw

dsraw = "6.1101,17.592\
\ 5.5277,9.1302\
\ 8.5186,13.662\
\ 7.0032,11.854\
\ 5.8598,6.8233\
\ 8.3829,11.886\
\ 7.4764,4.3483\
\ 8.5781,12\
\ 6.4862,6.5987\
\ 5.0546,3.8166\
\ 5.7107,3.2522\
\ 14.164,15.505\
\ 5.734,3.1551\
\ 8.4084,7.2258\
\ 5.6407,0.71618\
\ 5.3794,3.5129\
\ 6.3654,5.3048\
\ 5.1301,0.56077\
\ 6.4296,3.6518\
\ 7.0708,5.3893\
\ 6.1891,3.1386\
\ 20.27,21.767\
\ 5.4901,4.263\
\ 6.3261,5.1875\
\ 5.5649,3.0825\
\ 18.945,22.638\
\ 12.828,13.501\
\ 10.957,7.0467\
\ 13.176,14.692\
\ 22.203,24.147\
\ 5.2524,-1.22\
\ 6.5894,5.9966\
\ 9.2482,12.134\
\ 5.8918,1.8495\
\ 8.2111,6.5426\
\ 7.9334,4.5623\
\ 8.0959,4.1164\
\ 5.6063,3.3928\
\ 12.836,10.117\
\ 6.3534,5.4974\
\ 5.4069,0.55657\
\ 6.8825,3.9115\
\ 11.708,5.3854\
\ 5.7737,2.4406\
\ 7.8247,6.7318\
\ 7.0931,1.0463\
\ 5.0702,5.1337\
\ 5.8014,1.844\
\ 11.7,8.0043\
\ 5.5416,1.0179\
\ 7.5402,6.7504\
\ 5.3077,1.8396\
\ 7.4239,4.2885\
\ 7.6031,4.9981\
\ 6.3328,1.4233\
\ 6.3589,-1.4211\
\ 6.2742,2.4756\
\ 5.6397,4.6042\
\ 9.3102,3.9624\
\ 9.4536,5.4141\
\ 8.8254,5.1694\
\ 5.1793,-0.74279\
\ 21.279,17.929\
\ 14.908,12.054\
\ 18.959,17.054\
\ 7.2182,4.8852\
\ 8.2951,5.7442\
\ 10.236,7.7754\
\ 5.4994,1.0173\
\ 20.341,20.992\
\ 10.136,6.6799\
\ 7.3345,4.0259\
\ 6.0062,1.2784\
\ 7.2259,3.3411\
\ 5.0269,-2.6807\
\ 6.5479,0.29678\
\ 7.5386,3.8845\
\ 5.0365,5.7014\
\ 10.274,6.7526\
\ 5.1077,2.0576\
\ 5.7292,0.47953\
\ 5.1884,0.20421\
\ 6.3557,0.67861\
\ 9.7687,7.5435\
\ 6.5159,5.3436\
\ 8.5172,4.2415\
\ 9.1802,6.7981\
\ 6.002,0.92695\
\ 5.5204,0.152\
\ 5.0594,2.8214\
\ 5.7077,1.8451\
\ 7.6366,4.2959\
\ 5.8707,7.2029\
\ 5.3054,1.9869\
\ 8.2934,0.14454\
\ 13.394,9.0551\
\ 5.4369,0.61705"