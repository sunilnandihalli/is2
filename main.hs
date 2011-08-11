module Main where
import Data.List as L

stringToInteger::String->Integer
stringToInteger str = read str

stringToInt::String->Int
stringToInt str = read str

stringToIntegral::(Read a,Integral a)=>String->a
stringToIntegral str = read str

dist::Integral a=>(a,a)->(a,a)->a
dist (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

cost::Integral a=>[(a,a)]->(a,a)->a
cost locs from= sum $ map (\x->dist from x) locs

solve::Integral a=>a->[(a,a)]->a
solve n locs = minimum (map (cost locs) locs)

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

main = 
    do input<-getContents
       let (nstr:locPairStrs) = lines input
           n = read nstr
           locs = take (fromIntegral n) $ map (tuplify2 . (\x-> map stringToIntegral $ words x)) locPairStrs
           answer = solve n locs
       print (answer::Integer)




{-|
cljPartition::[a]->b->


randomProblem::(Int,[(Integer,Integer)])
randomProblem=
    do
      gen<-getStdGen
      let locs = map tuplify2 $ cljPartition 2 $ randoms gen::[Int]
      
|-}

       