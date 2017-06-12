import Utils

--a is Board

data MoveTree a = MoveTreeLeaf a | MoveTreeNode a [MoveTree a]
   deriving (Show)

getMoveTreeBoard (MoveTreeLeaf b) = b
getMoveTreeBoard (MoveTreeNode b _) = b

instance Foldable MoveTree where
   --foldMap f (MoveTreeLeaf board) = f board
   --foldMap f (MoveTreeNode trees) = map (foldMap f) trees
   foldr f acc (MoveTreeLeaf board) = f board acc
   foldr f acc (MoveTreeNode board []) = acc

   foldr f acc (MoveTreeNode board boards@(x:xs)) =
      foldr f (f (getMoveTreeBoard x) acc) (MoveTreeNode board xs)


--a is Board, b is heuristic return
minimax :: (Ord b) => Int -> (a -> b) -> MoveTree a -> Bool -> Maybe b
--minimax check tree depth isMaxi = 0
minimax depth checkScore (MoveTreeLeaf board) _ = checkScore board
minimax depth checkScore (MoveTreeNode board rest) isMaxi
   | depth == 0 = checkScore board
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




         --bestValue = Nothing
         --    v = mapTree (minimax (depth - 1) (minimax check rest) False) rest
         --    bestValue = max bestValue b

