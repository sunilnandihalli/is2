module Main where
import qualified Data.List as L
import qualified Data.HashTable as H
import qualified Data.Map as M

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

solve::Integral a=>a->[(a,a)]->(a,a)
solve n locs = L.minimumBy (\x y -> (compare (snd x) (snd y))) (zip (map fromIntegral [0..]) (map (cost locs) locs))


plotAsString::(Integral a)=>[(a,a)]->(a,a)->String
plotAsString locs (ans,_) = let xs = map fst locs
                                ys = map snd locs
                                locsmap = M.fromList (zip locs [0..])
                                bufsize=10
                                min' = ((\x -> x-bufsize) . minimum)
                                max' = ((\x -> x+bufsize) . maximum)
                                (bmin@(x0,y0),bmax@(x1,y1)) = ((min' xs,min' ys),(max' xs,max' ys))
                                plotString = foldl (\curStr newRowId -> (foldl (\ccStr newColId -> (ccStr++(case (M.lookup (newRowId,newColId) locsmap) of
                                                                                                              (Just (id,_)) -> (if id==ans then ("|" ++ (show id) ++ "|") 
                                                                                                                                else ("." ++ (show id) ++ "."))
                                                                                                              (Nothing) -> " . ")))
                                                                            curStr [y0..y1]) ++ "\n") "" [x0..x1] 
                            in plotString

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

main = 
    do input<-getContents
       let (nstr:locPairStrs) = lines input
           n = read nstr
           locs = take (fromIntegral n) $ map (tuplify2 . (\x-> map stringToIntegral $ words x)) locPairStrs
           answer = solve n locs
       putStrLn $ plotAsString (locs::[(Int,Int)]) (answer::(Int,Int))

       