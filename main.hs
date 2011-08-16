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

boundingBox::(Num a,Ord a)=>[(a,a)]->((a,a),(a,a))
boundingBox locs = let bufsize=1
                       xs = map fst locs
                       ys = map snd locs
                       min' = ((\x -> x-bufsize) . minimum)
                       max' = ((\x -> x+bufsize) . maximum)
                   in ((min' xs,min' ys),(max' xs,max' ys))

getEdge::(Num a,Ord a,Integral b)=>M.Map (a,a) b->(a,a)->(a,a)->(b,b)
getEdge locsToId p1 p2 = let Just id1 = M.lookup p1 locsToId
                             Just id2 = M.lookup p2 locsToId
                         in (id1,id2)

beachFrontIntersects::(Num a,Ord a)=>[(a,a)]->(a,a)->a
beachFrontIntersects beachFront newNode = fst newNode

intersectParabolas::(Fractional a,Num a,Ord a)=>(a,a)->(a,a)->a->(a,a)
intersectParabolas p1@(x1,y1) p2@(x2,y2) s
    | x1 > x2 = intersectParabolas p2 p1 s
    | dy > (s - x1) / 2 = (s-dy, yav)
    | x1 == x2 = (sxav,yav)
    | y1 < y2 = (sxav,y2-hsx)
    | y1 > y2 = (sxav,y2+hsx)
    | y1 == y2 = error "intersection not defined!"
  where dy = abs (y2-y1) / 2
        yav = (y1+y2)/2
        sxav = (s+x1)/2
        hsx = (s-x1)/2

mconvert::[((a,a),(a,a))]->[(a,Maybe (a,a),Maybe (a,a))]
mconvert listOfRangeValPairs = helper Nothing listOfRangeValPairs
 where helper _ [] = []
       helper prev (((y1,y2),v):[]) = (y1,prev,Just v):(y2,Just v,Nothing):[]
       helper prev (((y1,y2),v):restOfRanges) = (y1,prev,Just v):(helper (Just v) restOfRanges)                                               

newYLocation::(Bounded a,Ord a,Fractional a)=>(a,Maybe (a,a),Maybe (a,a))->a->(a,a)
newYLocation (y,Nothing,_) _= (y,minBound)
newYLocation (y,_,Nothing) _= (y,maxBound)
newYLocation (y,(Just p1),(Just p2)) s = (y,snd $ intersectParabolas p1 p2 s)

expandToAdvance::(Fractional a,Bounded a,Num a,Ord a)=>((M.Map (a,a) (a,a)),a)->a->((M.Map (a,a) (a,a)),a)
expandToAdvance (rangeToOpenTrapeziaMap,s) s' = let lst = mconvert $ M.toAscList rangeToOpenTrapeziaMap
                                                    y' = M.fromAscList $ map (\x->newYLocation x s') lst
                                                in (M.mapKeysMonotonic (\(y1,y2) ->let (Just v1) = M.lookup y1 y'  
                                                                                       (Just v2) = M.lookup y2 y'
                                                                                   in (v1,v2))
                                                    rangeToOpenTrapeziaMap, s' )

deleteOutOfRangeTrapezia::(Num a,Ord a)=>(M.Map (a,a) (a,a))->(a,a)->(M.Map (a,a) (a,a))
deleteOutOfRangeTrapezia originalTrapeziaMap trimRange@(ymin,ymax) = let Just (((ymin0,ymin1),vmin),restMin) = M.minViewWithKey originalTrapeziaMap
                                                                         Just (((ymax0,ymax1),vmax),restMax) = M.maxViewWithKey originalTrapeziaMap
                                                                     in if (ymin0<ymin) 
                                                                        then deleteOutOfRangeTrapezia (if (ymin1>ymin) 
                                                                                                       then M.insert (ymin,ymin1) vmin restMin 
                                                                                                       else restMin) trimRange 
                                                                        else if (ymax1>ymax) 
                                                                             then deleteOutOfRangeTrapezia (if (ymax0<ymax) 
                                                                                                            then M.insert (ymax0,ymax) vmax restMax 
                                                                                                            else restMax) trimRange
                                                                             else originalTrapeziaMap                 

advanceSweepLineTo::(Fractional a,Bounded a,Num a,Ord a)=>((M.Map (a,a) (a,a)),a)->a->((M.Map (a,a) (a,a)),a)
advanceSweepLineTo front@(rangeToOpenTrapeziaMap,curSweepLineLocation) newSweepLineLocation = let delta = newSweepLineLocation - curSweepLineLocation
                                                                                                  (expandedTrapezia,_) = expandToAdvance front newSweepLineLocation
                                                                                                  ((ymin,_),_) = M.findMin rangeToOpenTrapeziaMap
                                                                                                  ((_,ymax),_) = M.findMax rangeToOpenTrapeziaMap
                                                                                                  trimmedTrapezia =  deleteOutOfRangeTrapezia expandedTrapezia (ymin,ymax)
                                                                                               in (trimmedTrapezia,newSweepLineLocation)

addNewPointLocatedAtTheFrontToBeachFront::(Num a)=>(((M.Map (a,a) (a,a)),a),[(b,b)])->(a,a)->(((M.Map (a,a) (a,a)),a),[(b,b)])
addNewPointLocatedAtTheFrontToBeachFront (beachFront,graphEdges) newPoint = (beachFront,graphEdges)
                   
addNode::(Fractional a,Bounded a,Num a,Ord a,Integral b)=>M.Map (a,a) b->(((M.Map (a,a) (a,a)),a),[(b,b)])->(a,a)->(((M.Map (a,a) (a,a)),a),[(b,b)])
addNode locsToId (beachFront,graphEdges) newPoint@(x,_) = let newBeachFront = advanceSweepLineTo beachFront x
                                                          in addNewPointLocatedAtTheFrontToBeachFront (newBeachFront,graphEdges) newPoint

             
vornoiGraph::(Fractional a,Bounded a,Num a,Ord a,Integral b)=>[(a,a)]->[(b,b)]
vornoiGraph locs = let locsToId = M.fromList (zip locs [0..])
                       idToLocs = M.fromList (zip [0..] locs)
                       xSortedLocs@((xmin,_):_) = L.sort locs
                       (beachFront,graphEdges) = L.foldl' (addNode locsToId) ((M.empty,xmin),[]) xSortedLocs
                   in graphEdges


plotAsString::(Integral a)=>[(a,a)]->(a,a)->String
plotAsString locs (ansPosId,lowestCost) = let ((x0,y0),(x1,y1)) = boundingBox locs
                                              locsmap = M.fromList (zip locs [0..])
                                          in L.foldl' (\curStr newRowId -> 
                                                        L.foldl' (\ccStr newColId -> 
                                                                      ccStr++(case M.lookup (newRowId,newColId) locsmap of
                                                                                Just id -> if id==ansPosId then "|" ++ (show id) ++ "|"
                                                                                           else " " ++ (show id) ++ " "
                                                                                Nothing -> " . "))
                                                        curStr [y0..y1] ++ "\n") ("cost : "++show lowestCost++"\n") [x0..x1] 
                            

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

main = 
    do input<-getContents
       let (nstr:locPairStrs) = lines input
           n = read nstr
           locs = take (fromIntegral n) $ map (tuplify2 . (\x-> map stringToIntegral $ words x)) locPairStrs
           answer = solve n locs
       putStrLn $ plotAsString (locs::[(Int,Int)]) (answer::(Int,Int))

       