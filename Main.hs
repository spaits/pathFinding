bfs :: Eq a => [(a, [a])] -> a -> a -> [(a, [a], a, Int)]
bfs graph start dft = fst $ calculateDistances (newGraph, filter (\(x, _, _, _) -> x == start) newGraph)
 where
  newGraph = map (\(x,y) -> (x, y, dft, if x == start then 0 else -1)) graph

calculateDistances :: Eq a => ([(a, [a], a, Int)], [(a, [a], a, Int)]) -> ([(a, [a], a, Int)], [(a, [a], a, Int)])
calculateDistances fl@(graph, []) = fl
calculateDistances (graph, (hd:stack)) = calculateDistances (newGraph, stack ++ addStack)
 where
  (newGraph, addStack) = processNode graph hd

processNode :: Eq a => [(a, [a], a, Int)] -> (a, [a], a, Int) -> ([(a, [a], a, Int)], [(a, [a], a, Int)])
processNode [] _ = ([], [])
processNode (hd@(topId, topChildren, _, topDistance):graph) nd@(id, children, parent, distance)
 | (elem topId children) && (topDistance == -1) = ([newNode] ++ newGraph, [newNode] ++ stack)
 | otherwise = ([hd] ++ newGraph, stack)
  where
   (newGraph, stack) = processNode graph nd
   newNode = (topId, topChildren, id, distance + 1)
