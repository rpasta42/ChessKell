module MiniMax
( MoveTree(MoveTreeLeaf, MoveTreeNode)
, minimax, minimax'
, alphabeta
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


compareAi :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
compareAi f a b = case (a, b) of
   (_, Nothing)         -> a
   (Nothing, _)         -> b
   (Just a', Just b')   -> Just $ f a' b'



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
      accPick = if isMaxi then max else min

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
             newAcc = compareAi accPick acc v {-case (acc, v) of
               (_, Nothing)            -> acc --trace "test1" $ acc
               (Nothing, _)            -> v --trace "test2" $ v
               (Just acc', Just v')    -> Just $ accPick acc' v'-}
         in helper newAcc xs


negMax = -100000
posMax = 100000

alphabeta :: Int -> (a -> Int) -> Bool -> MoveTree a -> Int
alphabeta depth checkScore isMaxi moveTree =
   alphabeta' depth negMax posMax checkScore isMaxi moveTree
   --if isMaxi
   --then alphabeta' negMax posMax depth checkScore isMaxi moveTree
   --else alphabeta' posMax negMax depth checkScore isMaxi moveTree



alphabeta' :: Int -> Int -> Int -> (a -> Int) -> Bool -> MoveTree a -> Int
--alphabeta' a b depth checkScore isMaxi moveTree =
alphabeta' _     _  _  checkScore _      (MoveTreeLeaf board) = checkScore board
alphabeta' depth a1 b1 checkScore isMaxi (MoveTreeNode board rest)
   | depth == 0 = checkScore board
   | otherwise =
      let (v1, vPick) = if isMaxi then (negMax, max) else (posMax, min)

          helper a b v [] = v
          helper a b v (x:xs) =
             let v' = alphabeta' (depth - 1) a b checkScore (not isMaxi) x
                 newV = vPick v v'
                 (newA, newB) = if isMaxi
                                then (vPick a newV, b)
                                else (a, vPick b newV)
             in if newB <= newA
                then newV
                else helper newA newB newV xs

      in helper a1 b1 v1 rest


