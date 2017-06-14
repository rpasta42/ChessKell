module MiniMax
( MoveTree(MoveTreeLeaf, MoveTreeNode)
, minimax, minimax'
) where

import Utils
import Debug.Trace
import qualified Control.Parallel as P

--a is Board

data MoveTree a = MoveTreeLeaf a | MoveTreeNode a [MoveTree a]
   --deriving (Show)

getMoveTreeBoard (MoveTreeLeaf b) = b
getMoveTreeBoard (MoveTreeNode b _) = b



{-
mkSpaces 0 = []
mkSpaces n = ' ' : mkSpaces (n-1)

instance (Show a) => Show (MoveTree a) where
   show tree = "\n\n" ++ helper 0 tree
      where
         helper depth (MoveTreeLeaf _) = "L "
         helper depth (MoveTreeNode x sub) =
               "\n"
            ++  mkSpaces depth
            ++ "N " -- ++ (show x)
            ++ (concat $ map (helper $ depth+3) sub) -}



{-instance Foldable MoveTree where
   --foldMap f (MoveTreeLeaf board) = f board
   --foldMap f (MoveTreeNode trees) = map (foldMap f) trees
   foldr f acc (MoveTreeLeaf board) = f board acc
   foldr f acc (MoveTreeNode board []) = acc

   foldr f acc (MoveTreeNode board boards@(x:xs)) =
      foldr f (f (getMoveTreeBoard x) acc) (MoveTreeNode board xs)
-}


minimax' :: (Ord b, Show b) => Int -> (a -> b) -> Bool -> MoveTree a -> Maybe b
minimax' depth checkScore isMaxi moveTree =
   minimax depth checkScore moveTree isMaxi

--a is Board, b is heuristic return
minimax :: (Ord b, Show b) => Int -> (a -> b) -> MoveTree a -> Bool -> Maybe b
--minimax check tree depth isMaxi = 0
minimax depth checkScore (MoveTreeLeaf board) _ = Just $ checkScore board
minimax depth checkScore (MoveTreeNode board rest) isMaxi
   | depth == 0 = Just $ checkScore board
   | otherwise = helper Nothing rest where
      newIsMaxi = not isMaxi
      accPick = if not isMaxi then max else min

      --helper acc _  _ = acc
      helper acc [] = acc

      {-
      helper acc (x1:x2:xs) = --new pattern
         let v1 = minimax (depth - 1) checkScore x1 newIsMaxi
             v2 = minimax (depth - 1) checkScore x2 newIsMaxi
             newAcc =
               foldl (\acc x ->
                        case (acc, x) of
                           (_, Nothing) -> acc
                           (Nothing, _) -> x
                           (Just acc', Just x') -> Just $ accPick acc' x')
                     Nothing
                     (listParSeq3 [v1, v2, acc])
         in helper newAcc xs
      -}

      helper acc (x:xs) =
         let v = minimax (depth - 1) checkScore x newIsMaxi
             newAcc = case (acc, v) of
               (_, Nothing)            -> acc --trace "test1" $ acc
               (Nothing, _)            -> v --trace "test2" $ v
               (Just acc', Just v')    -> Just $ accPick acc' v'
         in helper newAcc xs



