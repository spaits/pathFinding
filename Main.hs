bfs :: [(Int, [Int])] -> (Int, [Int]) -> [(Int, [Int], Int)]
bfs graph start = fst (calculateDistances (map (\(x, y) -> (x, y, -1)) graph, [(\(x, y) -> (x, y, 0)) start]))

calculateDistances :: ([(Int, [Int], Int)], [(Int, [Int], Int)]) -> ([(Int, [Int], Int)], [(Int, [Int], Int)])
calculateDistances fl@(graph, []) = fl
calculateDistances (graph, ((hd@(topId, topChildren, topDistance)):stack)) = calculateDistances (newGraph, stack ++ addStack)
 where
  (newGraph, addStack) = processNode graph hd

processNode :: [(Int, [Int], Int)] -> (Int, [Int], Int) -> ([(Int, [Int], Int)], [(Int, [Int], Int)])
processNode [] _ = ([], [])
processNode (hd@(topId, topChildren, topDistance):graph) nd@(id, children, distance)
 | (elem topId children) && (topDistance == -1) = ([newNode] ++ newGraph, [newNode] ++ stack)
 | otherwise = ([hd] ++ newGraph, stack)
 where
   (newGraph, stack) = processNode graph nd
   newNode = (topId, topChildren, distance + 1)
