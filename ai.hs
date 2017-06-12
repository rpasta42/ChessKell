import Utils

--a is Board

data MoveTree a = MoveTreeLeaf a | MoveTreeNode a [MoveTree a]
   deriving (Show)

getMoveTreeBoard (MoveTreeLeaf b) = b
getMoveTreeBoard (MoveTreeNode b _) = b

{-instance Foldable MoveTree where
   --foldMap f (MoveTreeLeaf board) = f board
   --foldMap f (MoveTreeNode trees) = map (foldMap f) trees
   foldr f acc (MoveTreeLeaf board) = f board acc
   foldr f acc (MoveTreeNode board []) = acc

   foldr f acc (MoveTreeNode board boards@(x:xs)) =
      foldr f (f (getMoveTreeBoard x) acc) (MoveTreeNode board xs)
-}

--a is Board, b is heuristic return
minimax :: (Ord b) => Int -> (a -> b) -> MoveTree a -> Bool -> Maybe b
--minimax check tree depth isMaxi = 0
minimax depth checkScore (MoveTreeLeaf board) _ = Just $ checkScore board
minimax depth checkScore (MoveTreeNode board rest) isMaxi
   | depth == 0 = Just $ checkScore board
   | otherwise = helper depth Nothing rest where
      helper depth acc [] = acc
      helper depth acc (x:xs) =
         let newIsMaxi = not isMaxi
             foldAccFunc = if isMaxi then max else min
             v = minimax (depth - 1) checkScore x newIsMaxi
         in case (v, acc) of
            (Nothing, _)            -> acc
            (_, Nothing)            -> v
            (Just v', Just acc')    -> Just $ foldAccFunc acc' v'



