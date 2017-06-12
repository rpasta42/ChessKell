import Utils

--a is Board

data Tree a = NodeTerminal a | NodeSubtree a [Tree a]
   deriving (Show)

--a is Board, b is heuristic return
minimax :: (Num b) => Int -> (a -> b) -> Tree a -> Bool -> Maybe b
--minimax check tree depth isMaxi = 0
minimax depth checkScore (NodeTerminal board) _ = checkScore board
minimax depth checkScore (NodeSubtree board rest) isMaxi
   | depth == 0 = check board
   | otherwise  =
      foldl (\ currBoard acc ->
               let v = extractJust $ minimax (depth - 1) checkScore currBoard newIsMaxi
               in if not $ isJust acc
                  then Just v
                  else let accVal = extractJust acc
                       in Just $ foldAccFunc accVal v)
            Nothing
            rest
      where foldAccFunc = if isMaxi then max else min
            newIsMaxi = not isMaxi

      {-if isMaxi then
         foldl (\subx acc ->
                  let v = extractJust $ minimax (depth - 1) check subx False
                  in if not $ isJust acc
                     then Just v
                     else let acc' = extractJust acc
                          in if v > acc' then Just v else Just acc')
               Nothing
               rest
      else
         foldl (\subx acc ->
                  let v = extractJust $ minimax (depth - 1) subx True
                  in if not $ isJust acc
                     then Just v
                     else let acc' = extractJust acc
                          in if v < acc' then Just v else Just acc')
               Nothing
               rest-}




         --bestValue = Nothing
         --    v = mapTree (minimax (depth - 1) (minimax check rest) False) rest
         --    bestValue = max bestValue b

