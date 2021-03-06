--{-# OPTIONS_GHC -O2 -fspec-constr-count=6 #-}

module Main where
import qualified Data.List as L
import qualified Data.HashTable as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as E
import Debug.Trace
import Data.Ratio

stringToInteger::String->Integer
stringToInteger str = read str

stringToInt::String->Int
stringToInt str = read str

stringToIntegral::(Read a,Integral a)=>String->a
stringToIntegral str = read str

stringToRatio::(Read a,Integral a)=>String->Ratio a
stringToRatio str = fromIntegral (read str)

dist::(Integral a)=>(Ratio a,Ratio a)->(Ratio a,Ratio a)->Ratio a
dist (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

cost::(Integral a)=>[(Ratio a,Ratio a)]->(Ratio a,Ratio a)->Ratio a
cost locs from= sum $ map (\x->dist from x) locs

bruteForceSolve::(Integral a)=>a->[(Ratio a,Ratio a)]->(a,Ratio a)
bruteForceSolve n locs = L.minimumBy (\x y -> (compare (snd x) (snd y))) (zip (map fromIntegral [0..]) (map (cost locs) locs))

boundingBox::(Integral a,Ord a)=>[(Ratio a,Ratio a)]->((Ratio a,Ratio a),(Ratio a,Ratio a))
boundingBox locs = let bufsize=1
                       xs = map fst locs
                       ys = map snd locs
                       min' = ((\x -> x-bufsize) . minimum)
                       max' = ((\x -> x+bufsize) . maximum)
                   in ((min' xs,min' ys),(max' xs,max' ys))

getEdge::(Integral a,Ord a,Integral b)=>M.Map (Ratio a,Ratio a) b->(Ratio a,Ratio a)->(Ratio a,Ratio a)->(b,b)
getEdge locsToId p1 p2 = (locsToId|!|p1,locsToId|!|p2)

intersectParabolas::(Integral a,Ord a)=>(Ratio a,Ratio a)->(Ratio a,Ratio a)->Ratio a->(Ratio a,Ratio a)
intersectParabolas p1 p2 s 
    | trace ("----------entering intersectParabolas : "++(show (p1,p2,s))++"---------------\n") True = trace ((show ret)++"\n------------leaving intersectParabolas-------\n") ret
    where ret = intersectParabolas' p1 p2 s

intersectParabolas' p1@(x1,y1) p2@(x2,y2) s
    | x1 > x2 = intersectParabolas' p2 p1 s
    | dy >= hsx = (s-dy,yav)
    | x1 == x2 = (sxav,yav)
    | y1 < y2 = (sxav,y2-hsx)
    | y1 > y2 = (sxav,y2+hsx)
    | y1 == y2 = error "intersection not defined!"
  where dy = abs (y2-y1) / 2
        yav = (y1+y2) / 2
        sxav = (s+x1) / 2
        hsx = (s-x1) / 2
            
mconvert::(Show a)=>[((a,a),(a,a))]->[(a,Maybe (a,a),Maybe (a,a))]
mconvert xs | trace ("mconvert : "++(show xs)++"\n") True = trace ("mconvert ret : "++(show ret)++"\n") ret 
            where ret = mconvert' xs

mconvert' listOfRangeValPairs = reverse $ helper [] Nothing listOfRangeValPairs
 where helper acc _ [] = acc
       helper acc prev (((y1,y2),v):[]) = (y2,Just v,Nothing):(y1,prev,Just v):acc
       helper acc prev (((y1,y2),v):restOfRanges) = helper ((y1,prev,Just v):acc) (Just v) restOfRanges                                               

newYLocation::(Ord a,Integral a)=>(Ratio a,Maybe (Ratio a,Ratio a),Maybe (Ratio a,Ratio a))->Ratio a->(Ratio a,Ratio a)
newYLocation (y,Nothing,Just (x2,y2)) s = (y,y-s)
newYLocation (y,Just (x1,y1),Nothing) s = (y,y+s)
newYLocation (y,(Just p1),(Just p2)) s = (y,snd $ intersectParabolas p1 p2 s)

isDistinctAscList::(Ord a,Eq a,Show a)=>[a]->Bool
isDistinctAscList x | trace ("----entered isDistinctAscList--------\n"++(show x)) False = undefined
isDistinctAscList (x1:x2:xs) = if x1<x2  then isDistinctAscList (x2:xs) else error ("loc : "++(show (x1,x2)))
isDistinctAscList _ = True

expandToAdvance::(Integral a,Ord a)=>((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)->Ratio a->((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a)
expandToAdvance a b | trace ("----------------expandToAdvance----------------------\n"++(show (a,b))++"\n") False = undefined
expandToAdvance a b = trace ("---------------- entering expandToAdvance--------------------\n (bf-ge,s) : "++ (show (a,b)) ++ "\n ret : "++(show ret)
                                                                                                               ++"\n----------leaving expandToAdvance---------") ret
    where ret = expandToAdvance' a b 


validTrapeziaMap::(Integral a,Ord a)=>M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)->Bool
validTrapeziaMap m = isValid $ M.keys m
                     where isValid ((_,y1):rst@((y2,_):_)) = if (y1/=y2) 
                                                             then False 
                                                             else isValid rst
                           isValid _ = True                                     
                     

expandToAdvance' w@(_,s) s' | s==s' = trace "short circuiting expandToAdvance\n" w
expandToAdvance' (rangeToOpenTrapeziaMap,s) s' = E.assert (validTrapeziaMap rangeToOpenTrapeziaMap) $
                                                 let lst = mconvert $ M.toAscList (if M.valid rangeToOpenTrapeziaMap then rangeToOpenTrapeziaMap else error "invalide input map")
                                                     lst' = trace (" lst : "++(show lst)++"\n trpMap : "++(show rangeToOpenTrapeziaMap)++"\n") $ map (\x->newYLocation x s') lst  
                                                     y' = trace (" lst' : "++(show lst')) $ if (isDistinctAscList lst') 
                                                                                            then M.fromDistinctAscList lst' 
                                                                                            else error ("function not monotonic\n orig"++(show lst)++"\nmapped : "++(show lst'))
                                                 in (M.mapKeysMonotonic (\(y1,y2) ->let [v1,v2] = map (y'|!|) [y1,y2]
                                                                                    in (v1,v2)) rangeToOpenTrapeziaMap,s')

deleteOutOfRangeTrapezia::(Num a,Ord a)=>(M.Map (a,a) (a,a))->(a,a)->(M.Map (a,a) (a,a))
deleteOutOfRangeTrapezia originalTrapeziaMap trimRange@(ymin,ymax) 
    | (M.null originalTrapeziaMap) = M.empty
    | otherwise      = let Just (((ymin0,ymin1),vmin),restMin) = M.minViewWithKey originalTrapeziaMap
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
                                    

removeDisappearingValleys::(Integral a,Ord a,Integral b)=>
                           (M.Map (Ratio a,Ratio a) b)->
                           (((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])->Ratio a->
                           (((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
removeDisappearingValleys _ ((_,sOld),graphedges) s 
    | trace ("entering removeDisappearingValleys for (sOld,s) : "++(show (sOld,s))++" ge : "++(show graphedges)) False = undefined
removeDisappearingValleys _ w@((_,sOld),_) s 
    | sOld==s = trace "short-circuiting \n" w                                                                                                                         
removeDisappearingValleys loc2Id ((trapMap,sOld),graphedges) s  =
        let kVList = M.toAscList trapMap
            (revRet,decs,flat,ge') =  trace ("kVList : "++(show kVList)) $ L.foldl' h ([],[],[],graphedges) kVList
              where 
               h a b = trace ("------------------start---------------------\n"
                              ++" s : "++(show s)++"\n before : revRet,decs,flat,ge : \n"
                                    ++(show a)++"\n newVal : "++(show b)++"\n") $ trace ("after : "++(show ret)++"\n----------------------end-------------------\n") ret 
                          where ret = g a b
               g a b = trace ("------>(a,b) : "++(show (a,b))) $ f a b
               f (revRet,[],flat@(flatF@(_,(x0,_)):[]),ge) newVal@(_,(x2,y2)) 
                  | x0<=x2 = ((flatF:revRet),[],[newVal],ge)
                  | x0>x2 = (revRet,flat,[newVal],ge)   
               f (revRet,decs@(decsF@(_,decLoc@(x1,y1)):decsR),flat@((_,(x0,_)):_),ge) newVal@(_,incLoc@(x2,y2)) 
                  | (x0==x1) = g (revRet,decsR,decsF:flat,ge) newVal
                  | (x0==x2) = (revRet,decs,newVal:flat,ge)
                  | (x0>x2) = (revRet,flat++decs,[newVal],ge)
                  | (s-x0)<(y2-y1) = (flat++decs++revRet,[],[newVal],ge)
                  | otherwise = let decId = loc2Id|!|decLoc
                                    incId = loc2Id|!|incLoc
                                in g (revRet,decsR,[decsF],(decId,incId):ge) newVal
               f (revRet,decs,[],ge) nv = (revRet,decs,[nv],ge)                     
               f (revRet,decs,flat,ge) nv = error ("no match found for \n revRet : "++(show revRet)++" \n decs : "++(show decs)
                                                      ++"\n flat : "++ (show flat)++"\n ge : "++(show ge)++"\n")
            fst:rst = flat++decs++revRet
            kVlistAfterRemovingDissappearingValleys = L.foldl' (\res@(((y2,y3),p2):rres) inp@((y0,y1),p1) -> if y1==y2 
                                                                                                             then inp:res
                                                                                                             else let (_,y) = intersectParabolas p1 p2 sOld
                                                                                                                  in ((y0,y),p1):((y,y3),p2):rres) [fst] rst
            newTrapMap = M.fromDistinctAscList kVlistAfterRemovingDissappearingValleys
        in ((newTrapMap,sOld),ge')
                                                                       

advanceSweepLineTo::(Integral a,Ord a,Integral b)=>
                    (M.Map (Ratio a,Ratio a) b)->
                    (((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])->Ratio a->
                    (((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
advanceSweepLineTo loc2Id (front@(rangeToOpenTrapeziaMap,curSweepLineLocation),ge) newSweepLineLocation = 
    let delta = newSweepLineLocation - curSweepLineLocation
        (newFront,ge') =  removeDisappearingValleys loc2Id (front,ge) newSweepLineLocation
        (expandedTrapezia,_) = expandToAdvance newFront newSweepLineLocation
        ((ymin,_),_) = M.findMin rangeToOpenTrapeziaMap
        ((_,ymax),_) = M.findMax rangeToOpenTrapeziaMap
        trimmedTrapezia =  deleteOutOfRangeTrapezia expandedTrapezia (ymin,ymax)
    in ((trimmedTrapezia,newSweepLineLocation),ge')

findParabolaX::(Integral a,Ord a)=>(Ratio a,Ratio a)->Ratio a->Ratio a->Ratio a
findParabolaX (x,y) s y' 
    | dy<=hsx   = x+hsx
    | otherwise = s-dy
 where dy = abs(y'-y)
       hsx = (s-x) / 2

pairPartitionHelper::[(b,b)]->[b]->[(b,b)]
pairPartitionHelper pairs (x1:x2:xs) = pairPartitionHelper ((x1,x2):pairs) (x2:xs)
pairPartitionHelper pairs _ = pairs

pairPartition::[b]->[(b,b)]
pairPartition xs = reverse $ pairPartitionHelper [] xs

revPairPartitionHelper::[(b,b)]->[b]->[(b,b)]
revPairPartitionHelper pairs (x1:x2:xs) = revPairPartitionHelper ((x2,x1):pairs) (x2:xs)
revPairPartitionHelper pairs _ = pairs

revPairPartition::[b]->[(b,b)]
revPairPartition xs = reverse $ revPairPartitionHelper [] xs

(|!|)::(Ord k,Show k,Show v)=>M.Map k v->k->v
m |!| k = case M.lookup k m of
            Just v -> v
            Nothing -> error ("\n--------key not found-----\n map : "++show m++"\n k : "++show k++"\n isValid : "++ (show $ M.valid m))  

addNewPointLocatedAtTheFrontToBeachFront::(Show a,Show b,Integral a)=>M.Map (Ratio a,Ratio a) b->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])->                
                                          (Ratio a,Ratio a)->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
addNewPointLocatedAtTheFrontToBeachFront loc2Id ((parabolas,_),_) np
    | (M.null parabolas) = error "should not have come here!"
addNewPointLocatedAtTheFrontToBeachFront loc2Id bfGe np
    | trace ("entering addNewPointLocatedAtTheFrontToBeachFront for "++(show np)++" id : "++(show (loc2Id|!|np))++"\n") False = undefined
addNewPointLocatedAtTheFrontToBeachFront loc2Id (beachFront,graphEdges) (nx,ny) = (let flatten xs = L.foldl' (\cur (x1,x2) -> x1:x2:cur) [] (reverse xs)
                                                                                       (parabolas,frontLoc) = beachFront
                                                                                       toDecendingList = reverse . S.toAscList 
                                                                                       ySet = S.fromAscList $ flatten $ M.keys parabolas
                                                                                       (prevSet,foundExactY,postSet) = S.splitMember ny ySet
                                                                                       prevList@(prevY:_) = toDecendingList prevSet
                                                                                       postList@(postY:_) = S.toAscList postSet
                                                                                       (xPy,xMy) = (nx+ny,nx-ny)
                                                                                       nid =  loc2Id |!| (nx,ny)        
                                                                                       pairsToBeDeletedOrModified = if foundExactY 
                                                                                                                    then ((takeWhile (\w@(_,y2)->
                                                                                                                                      let x2 = findParabolaX 
                                                                                                                                               (parabolas |!| w) nx y2
                                                                                                                                      in x2-y2<=xMy)
                                                                                                                          $ revPairPartition (ny:prevList)) ++ 
                                                                                                                          (takeWhile (\w@(y1,_)->
                                                                                                                                      let x1 = findParabolaX 
                                                                                                                                               (parabolas |!| w) nx y1
                                                                                                                                      in x1+y1<=xPy)
                                                                                                                          $ pairPartition (ny:postList)))
                                                                                                                    else (let pl= revPairPartition prevList
                                                                                                                          in (takeWhile 
                                                                                                                              (\w@(_,y2)->
                                                                                                                               let x2 = findParabolaX  (parabolas |!| w) nx y2
                                                                                                                               in x2-y2<=xMy) pl))
                                                                                                                             ++ (prevY,postY):
                                                                                                                                    (takeWhile (\w@(y1,_)->
                                                                                                                                      let x1 = findParabolaX 
                                                                                                                                               (parabolas |!| w) nx y1
                                                                                                                                      in x1+y1<=xPy)
                                                                                                                                    $ pairPartition postList)
                                                                                       (bf',ge') =  (L.foldl' (\(bf,ge) k@(y1,y2) ->
                                                                                                          let v = bf |!| k
                                                                                                              bf' = M.delete k bf
                                                                                                              id = loc2Id |!| v
                                                                                                          in (bf',(nid,id):ge))
                                                                                                     (parabolas,graphEdges) pairsToBeDeletedOrModified)
                                                                                       (nYmin,bf'') = let h@(hy,_)=head pairsToBeDeletedOrModified
                                                                                                          hParabola@(hpx,hpy) = parabolas |!| h
                                                                                                          hx = findParabolaX hParabola nx hy
                                                                                                      in if hx-hy>xMy 
                                                                                                         then let nYmin = if hpx-hpy>xMy 
                                                                                                                          then (hpy+ny)/2
                                                                                                                          else ny-(nx-hpx)/2
                                                                                                              in (nYmin,M.insert (hy,nYmin) hParabola bf')
                                                                                                         else (hy,bf')
                                                                                       (nYmax,bf''') = let l@(_,ly) = last pairsToBeDeletedOrModified
                                                                                                           lParabola@(lpx,lpy) = parabolas |!| l
                                                                                                           lx = findParabolaX lParabola nx ly
                                                                                                       in if lx+ly>xPy
                                                                                                          then let nYmax = if lpx+lpy>xPy
                                                                                                                           then (lpy+ny)/2
                                                                                                                           else ny+(nx-lpx)/2
                                                                                                               in (nYmax,M.insert (nYmax,ly) lParabola bf'')
                                                                                                          else (ly,bf'')
                                                                                       bf'''' =  M.insert (nYmin,nYmax) (nx,ny) bf'''            
                                                                                 in ((bf'''',nx),ge'))     
                                                                                   
addNode::(Integral a,Ord a,Integral b)=>M.Map (Ratio a,Ratio a) b->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
       ->(Ratio a,Ratio a)->(((M.Map (Ratio a,Ratio a) (Ratio a,Ratio a)),Ratio a),[(b,b)])
addNode locsToId _ np | trace ("locsToId : "++(show locsToId)++"\n adding "++(show (locsToId|!|np))++"\n") False = undefined
addNode _ ((f,_),_) np | (if M.valid f then False else error ("input front not valid"++show np++"\n map : "++show f)) = undefined
addNode locsToId (beachFront@(f0,_),graphEdges) newPoint@(x,_) = let (newBeachFront@(f,_),ge') = if (M.valid f0) 
                                                                                                 then advanceSweepLineTo locsToId (beachFront,graphEdges) x 
                                                                                                 else error ("tree not valid -1"++(show f0))
                                                                     retval@((f',_),_) = trace "reached here" $
                                                                                         if (M.valid f) 
                                                                                         then addNewPointLocatedAtTheFrontToBeachFront locsToId 
                                                                                                  (newBeachFront,ge') newPoint
                                                                                         else error ("tree not valid 000000 \n" 
                                                                                                     ++ "\n elems : "++show (L.sort (M.elems f))
                                                                                                     ++ "\n newPoint : "++show newPoint++"\n"
                                                                                                     ++ (plotAsString (L.sort (M.elems f)) (fromIntegral ((L.length (M.elems f))-1),0%1)) ++" \n new : "
                                                                                                     ++ (show f) ++ "\nold : "++(show f0) ++"\n np : "++show newPoint)
                                                                 in trace "reached here too" $ if (M.valid f') 
                                                                                               then retval 
                                                                                               else error ("tree not valid 1111 " ++ (show f'))
             
vornoiGraph::(Integral a,Ord a,Integral b)=>[(Ratio a,Ratio a)]->[(b,b)]
vornoiGraph locs = let locsToId = M.fromList (zip locs [0..])
                       idToLocs = M.fromList (zip [0..] locs)
                       ((_,y0),(_,y1)) = boundingBox locs 
                       xSortedLocs@(fp@(xmin,_):_) = L.sort locs
                       startingBeachFront = (M.insert (y0,y1) fp M.empty,xmin)
                       (beachFront,graphEdges) = L.foldl' (addNode locsToId) (startingBeachFront,[]) (drop 1 xSortedLocs)
                   in  graphEdges

constWidth::(Integral a)=>a->String
constWidth id 
    | id < 10 = (show id)++" "
    | id < 100 = (show id)
    | otherwise = error "cannot show"             

plotAsString::(Integral a)=>[(Ratio a,Ratio a)]->(a,Ratio a)->String
plotAsString locs (ansPosId,lowestCost) = let ((x0,y0),(x1,y1)) = boundingBox locs
                                              locsmap = M.fromList (zip locs [0..])
                                          in L.foldl' (\curStr newRowId -> 
                                                        L.foldl' (\ccStr newColId -> 
                                                                      ccStr++(case M.lookup (newRowId,newColId) locsmap of
                                                                                Just id -> if id==ansPosId then "|" ++ (constWidth id) ++ "|"
                                                                                           else " " ++ (constWidth id) ++ " "
                                                                                Nothing -> " .  "))
                                                        curStr [y0..y1] ++ "\n") ("cost : "++show lowestCost++"\n") [x0..x1] 
                            
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

graphMinimizeCost::(Integral a)=>(a->Ratio a)->M.Map a [a]->(a,Ratio a)->(a,Ratio a)
graphMinimizeCost cost gr cur@(curId,curMinCost) = let new@(newId,newMinCost) = L.minimumBy (\x y->(compare (snd x) (snd y))) [(cid,cost cid) | cid<-gr |!| curId]
                                                   in if curMinCost>newMinCost 
                                                      then graphMinimizeCost cost gr new 
                                                      else cur

solve::(Integral a)=>M.Map a [a]->M.Map a (Ratio a,Ratio a)->(a,Ratio a)
solve gr id2locs = let locs = M.elems id2locs
                       cost id = let from = id2locs|!|id
                                 in sum $ map (\to->dist from to) locs
                   in graphMinimizeCost cost gr (0,cost 0)

main = 
    do input<-getContents
       let w@(nstr:locPairStrs) = lines input
           n = read nstr::Integer
           locs = take (fromIntegral n) $ map (tuplify2 . (\x-> map stringToRatio $ words x)) locPairStrs
           bruteForceAnswer = bruteForceSolve n locs 
           edges = vornoiGraph locs
           graph = M.fromListWith (++) $ [(x,[y])|(x,y)<-edges]++[(y,[x])|(x,y)<-edges]
           answer = solve graph $ M.fromList (zip [0..] locs)  
       putStrLn $ plotAsString locs bruteForceAnswer
       putStrLn $ (show.numerator.snd) answer

       