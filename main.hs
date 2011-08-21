{-# OPTIONS_GHC -O2 #-}

module Main where
import qualified Data.List as L
import qualified Data.HashTable as H
import qualified Data.Map as M
import qualified Data.Ratio as R
import qualified Data.Set as S
import Data.Ratio

stringToInteger::String->Integer
stringToInteger str = read str

stringToInt::String->Int
stringToInt str = read str

stringToIntegral::(Read a,Integral a)=>String->a
stringToIntegral str = read str

stringToRatio::(Read a,Integral a)=>String->Ratio a
stringToRatio str = read str

dist::(Integral a)=>(Ratio a,Ratio a)->(Ratio a,Ratio a)->Ratio a
dist (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

cost::(Integral a)=>[(Ratio a,Ratio a)]->(Ratio a,Ratio a)->Ratio a
cost locs from= sum $ map (\x->dist from x) locs

solve::(Integral a)=>a->[(Ratio a,Ratio a)]->(a,Ratio a)
solve n locs = L.minimumBy (\x y -> (compare (snd x) (snd y))) (zip (map fromIntegral [0..]) (map (cost locs) locs))

boundingBox::(Integral a,Ord a)=>[(Ratio a,Ratio a)]->((Ratio a,Ratio a),(Ratio a,Ratio a))
boundingBox locs = let bufsize=1
                       xs = map fst locs
                       ys = map snd locs
                       min' = ((\x -> x-bufsize) . minimum)
                       max' = ((\x -> x+bufsize) . maximum)
                   in ((min' xs,min' ys),(max' xs,max' ys))

getEdge::(Integral a,Ord a,Integral b)=>M.Map (Ratio a,Ratio a) b->(Ratio a,Ratio a)->(Ratio a,Ratio a)->(b,b)
getEdge locsToId p1 p2 = let Just id1 = M.lookup p1 locsToId
                             Just id2 = M.lookup p2 locsToId
                         in (id1,id2)

{-|
beachFrontIntersects::(Integral a,Ord a)=>[(Ratio a,Ratio a)]->(Ratio a,Ratio a)->a
beachFrontIntersects beachFront newNode = fst newNode
|-}

intersectParabolas::(Integral a,Ord a)=>(Ratio a,Ratio a)->(Ratio a,Ratio a)->Ratio a->(Ratio a,Ratio a)
intersectParabolas p1@(x1,y1) p2@(x2,y2) s
    | x1 > x2 = intersectParabolas p2 p1 s
    | dy > (s - x1) / 2 = (s-dy, yav)
    | x1 == x2 = (sxav,yav)
    | y1 < y2 = (sxav,y2-hsx)
    | y1 > y2 = (sxav,y2+hsx)
    | y1 == y2 = error "intersection not defined!"
  where dy = abs (y2-y1) / 2
        yav = (y1+y2) / 2
        sxav = (s+x1) / 2
        hsx = (s-x1) / 2

mconvert::[((a,a),(a,a))]->[(a,Maybe (a,a),Maybe (a,a))]
mconvert listOfRangeValPairs = helper Nothing listOfRangeValPairs
 where helper _ [] = []
       helper prev (((y1,y2),v):[]) = (y1,prev,Just v):(y2,Just v,Nothing):[]
       helper prev (((y1,y2),v):restOfRanges) = (y1,prev,Just v):(helper (Just v) restOfRanges)                                               

newYLocation::(Ord a,Integral a)=>(Ratio a,Maybe (Ratio a,Ratio a),Maybe (Ratio a,Ratio a))->Ratio a->(Ratio a,Ratio a)
newYLocation (y,Nothing,Just (x2,y2)) s = (y,y-s)
newYLocation (y,Just (x1,y1),Nothing) s = (y,y+s)
newYLocation (y,(Just p1),(Just p2)) s = (y,snd $ intersectParabolas p1 p2 s)

expandToAdvance::(Integral a,Ord a)=>((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)->Ratio a->((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)
expandToAdvance (rangeToOpenTrapeziaMap,s) s' = let lst = mconvert $ M.toAscList rangeToOpenTrapeziaMap
                                                    y' = M.fromDistinctAscList $ map (\x->newYLocation x s') lst
                                                in (M.mapKeysMonotonic (\(y1,y2) ->let (Just v1) = M.lookup y1 y'  
                                                                                       (Just v2) = M.lookup y2 y'
                                                                                   in (v1,v2)) rangeToOpenTrapeziaMap,s')

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

advanceSweepLineTo::(Integral a,Ord a)=>((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)->Ratio a->((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)
advanceSweepLineTo front@(rangeToOpenTrapeziaMap,curSweepLineLocation) newSweepLineLocation = let delta = newSweepLineLocation - curSweepLineLocation
                                                                                                  (expandedTrapezia,_) = expandToAdvance front newSweepLineLocation
                                                                                                  ((ymin,_),_) = M.findMin rangeToOpenTrapeziaMap
                                                                                                  ((_,ymax),_) = M.findMax rangeToOpenTrapeziaMap
                                                                                                  trimmedTrapezia =  deleteOutOfRangeTrapezia expandedTrapezia (ymin,ymax)
                                                                                               in (trimmedTrapezia,newSweepLineLocation)

findParabolaX::(Integral a,Ord a)=>(Ratio a,Ratio a)->Ratio a->Ratio a->Ratio a
findParabolaX (x,y) s y' 
    | dy<=hsx = x+hsx
    | y' < y = s-dy
    | y' > y = s+dy 
 where dy = abs(y'-y)
       hsx = (s-x) / 2

data MaskType = Exclude | Intersect | Untouched deriving (Eq,Show)
data SideType = PP | PM | MM | MP | PassThrough deriving (Eq,Show)

side::(Num a)=>(a,a)->(a,a)->a->SideType
side (x,y) (x0,y0) s = PassThrough

masks::(Num a)=>((a,a),(a,a))->(a,a)->(Maybe (a,a),Bool)
masks ((y1,y2),(x,y)) (nx,ny) 
      | ny == y = (Nothing,True)

pairPartition::[b]->[(b,b)]
pairPartition (x1:x2:xs) = (x1,x2):(pairPartition xs)
pairPartition _ = []

revPairPartition::[b]->[(b,b)]
revPairPartition (x1:x2:xs) = (x2,x1):(revPairPartition xs)
revPairPartition _ = []

addNewPointLocatedAtTheFrontToBeachFront::(Num a)=>M.Map (a,a) b->(((M.Map (a,a) (a,a)),a),[(b,b)])->(a,a)->(((M.Map (a,a) (a,a)),a),[(b,b)])
addNewPointLocatedAtTheFrontToBeachFront loc2Id (beachFront,graphEdges) (nx,ny) = (let flatten xs = L.foldl' (\cur (x1,x2) -> x1:x2:cur) [] (reverse xs)
                                                                                       toDecendingList = reverse . S.toAscList 
                                                                                       ySet = S.fromAscList $ flatten $ M.keys beachFront
                                                                                       (prevSet@(prevY:_),foundExactY,postSet@(postY:_)) = S.splitMember ny ySet                  
                                                                                       prevList = toDecendingList prevSet
                                                                                       postList = S.toAscList postSet
                                                                                       (xPy,xMy) = (nx+ny,nx-ny)
                                                                                       nid = loc2Id M.!(nx,ny)            
                                                                                       pairsToBeDeletedOrModified = if foundExactY 
                                                                                                                    then ((takeWhile (\w@(_,y2)->
                                                                                                                                      let x2 = findParabolaX 
                                                                                                                                               (beachFront M.! w) nx y2
                                                                                                                                      in x2-y2<xMy)
                                                                                                                          $ revPairPartition (ny:prevList)) ++ 
                                                                                                                          (takeWhile (\w@(y1,_)->
                                                                                                                                      let x1 = findParabolaX 
                                                                                                                                               (beachFront M.! w) nx y1
                                                                                                                                      in x1+y1<xPy)
                                                                                                                          $ pairPartition (ny:postList)))
                                                                                                                    else ((takeWhile (\w@(_,y2)->
                                                                                                                                      let x2 = findParabolaX 
                                                                                                                                               (beachFront M.! w) nx y2
                                                                                                                                      in x2-y2<xMy)
                                                                                                                          $ revPairPartition prevList) ++ (prevY,postY):
                                                                                                                          (takeWhile (\w@(y1,_)->
                                                                                                                                      let x1 = findParabolaX 
                                                                                                                                               (beachFront M.! w) nx y1
                                                                                                                                      in x1+y1<xPy)
                                                                                                                          $ pairPartition postList))
                                                                                       (bf',ge') = (L.foldl' (\(bf,ge) k@(y1,y2) ->
                                                                                                                  let v = bf M.! k
                                                                                                                      bf' = M.delete k bf
                                                                                                                      id = loc2Id M.! v
                                                                                                                  in (bf',(nid,id)))
                                                                                                    (beachFront,graphEdges) pairsToBeDeletedOrModified)
                                                                                       bf'' = let h@(hy,_)=head pairsToBeDeletedOrModified
                                                                                                  hParabola@(hpx,hpy) = beachFront M.! h
                                                                                                  hx = findParabolaX hParabola nx hy
                                                                                              in if hx-hy<xMy 
                                                                                                 then M.insert (hy,if hpx-hpy>xMy 
                                                                                                                   then (hpy+ny)/2
                                                                                                                   else ny-(nx-hpx)/2) hParabola bf'
                                                                                                 else bf'
                                                                                       bf'''= let l@(_,ly) = last pairsToBeDeletedOrModified
                                                                                                  lParabola@(lpx,lpy) = beachFront M.! l
                                                                                                  lx = findParabolaX lParabola nx ly
                                                                                              in if lx+ly>xPy
                                                                                                 then M.insert (if lpx+lpy>xPy
                                                                                                                then (lpy+ny)/2
                                                                                                                else ny+(nx-lpx)/2,ly) lParabola bf''
                                                                                                 else bf''
                                                                                 in (bf''',ge'))     
                                                                                   
addNode::(Integral a,Ord a,Integral b)=>M.Map (Ratio a,Ratio a) b->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
       ->(Ratio a,Ratio a)->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
addNode locsToId (beachFront,graphEdges) newPoint@(x,_) = let newBeachFront = advanceSweepLineTo beachFront x
                                                          in addNewPointLocatedAtTheFrontToBeachFront locsToId (newBeachFront,graphEdges) newPoint
             
vornoiGraph::(Integral a,Ord a,Integral b)=>[(Ratio a,Ratio a)]->[(b,b)]
vornoiGraph locs = let locsToId = M.fromList (zip locs [0..])
                       idToLocs = M.fromList (zip [0..] locs)
                       xSortedLocs@((xmin,_):_) = L.sort locs
                       (beachFront,graphEdges) = L.foldl' (addNode locsToId) ((M.empty,xmin),[]) xSortedLocs
                   in graphEdges


plotAsString::(Integral a)=>[(Ratio a,Ratio a)]->(a,Ratio a)->String
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
           locs = take (fromIntegral n) $ map (tuplify2 . (\x-> map stringToRatio $ words x)) locPairStrs
           answer = solve n locs
       putStrLn $ plotAsString locs answer

       