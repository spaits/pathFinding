dfs :: Eq a => [(a, [a])] -> a -> a -> [(a, [a], a, Int)]
dfs graph start dft = depthFirstSearch readyGraph (head $ filter (\(id, _, _, _) -> id == start) readyGraph)
 where
  readyGraph = map (\(id, ngbh) -> (id, ngbh, dft, if id == start then 0 else -1)) graph
 
depthFirstSearch :: Eq a => [(a, [a], a, Int)] -> (a, [a], a, Int) -> [(a, [a], a, Int)]
depthFirstSearch graph node = depthFirstOnChildren $ processChildren graph node

depthFirstOnChildren :: Eq a => ([(a, [a], a, Int)], [(a, [a], a, Int)]) -> [(a, [a], a, Int)]
depthFirstOnChildren (graph, []) = graph
depthFirstOnChildren (graph, (x:xs)) = depthFirstOnChildren ((depthFirstSearch graph x), xs)

processChildren :: Eq a => [(a, [a], a, Int)] -> (a, [a], a, Int) -> ([(a, [a], a, Int)], [(a, [a], a, Int)])
processChildren [] _ = ([], [])
processChildren (top@(topId, topChildren, _, topDist):graphRem) nd@(id, children, _, dist)
 | elem topId children && (topDist == -1 || topDist > newDist) = (newTop : newGraph, newTop : stack)
 | otherwise = (top : newGraph, stack)
  where
   newDist = dist + 1
   newTop = (topId, topChildren, id, newDist)
   (newGraph, stack) = processChildren graphRem nd
