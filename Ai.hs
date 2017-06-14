{-# LANGUAGE FlexibleInstances #-}
module Ai
( getPieceValue
, getBoardScore
, genBoardTree
, getAiMove
) where


import qualified Data.List as L
import ChessUtils (flipColor)
import Types
import Utils
import Logic
import ChessUtils
import MiniMax
import qualified Data.Monoid as Monoid
import Debug.Trace


mkSpaces 0 = []
mkSpaces n = ' ' : mkSpaces (n-1)


addTabs num lst = unlines $ map ((++) spaces)
                            (lines lst)
   where spaces = mkSpaces num

instance Show (MoveTree Board) where
   show tree = "\n\n" ++ helper 0 tree
      where
         helper depth (MoveTreeLeaf x) = {-"L " ++ -}
            "\n" ++ (addTabs depth . matrixToDisplay $ displayBoardByColor x White)

         helper depth (MoveTreeNode x sub) =
               "\n"
            ++  mkSpaces depth
            ++ "N " -- ++ (show x)
            ++ (concat $ map (helper $ depth+3) sub)


getPieceValue :: Piece -> Int
--100 centipawn (cp) = 1 pawn
getPieceValue Pawn = 100
getPieceValue Knight = 300
getPieceValue Bishop = 301
getPieceValue Rook = 500
getPieceValue Queen = 900
getPieceValue King = 10000


getBoardScore :: Board -> Int
getBoardScore b@(Board { getWhitePieces=wPieces
                       , getBlackPieces=bPieces
                       }) =
   let getTotalScore = Monoid.getSum . mconcat
       getPieceScore = Monoid.Sum . getPieceValue . getPiece
       wScore = getTotalScore $ map getPieceScore wPieces
       bScore = getTotalScore $ map getPieceScore bPieces
       boardScore = wScore - bScore

       (maybeWinnerB, isDrawB) = getMatesAndStales b Black
       (maybeWinnerW, isDrawW) = getMatesAndStales b White
       checkMateScore =
         case (maybeWinnerW, maybeWinnerB) of
            --(Just White, _) -> 100000
            --(_, Just Black) -> -100000
            _           -> 0

       finalScore = boardScore + checkMateScore

   in finalScore

   {-in trace (if finalScore > 0 then "White wining"
            else if finalScore == 0 then "Tie"
            else "Black winning")
            $ finalScore-}



genBoardTree :: Int -> Color -> Board -> MoveTree Board
genBoardTree depth toPlay board
   | depth == 0 = MoveTreeLeaf board --19 seconds!!
   | otherwise  = MoveTreeNode board subTree -- $ listParSeq3 subTree
      where newColor = flipColor toPlay
            subTree = map (genBoardTree (depth-1) newColor)
                          $ genPossibleMoveBoards board newColor


getAiMove :: Board -> Color -> Int -> Maybe (Move, Int)
getAiMove board color depth =
   let isMaxi = color == White
       testSign :: Ord a => a -> a -> Bool
       testSign = if isMaxi then (>) else (<)

       allMoves = getPossibleMoves board color

       --no seq: 1m 46 secs
       --listParSeq2: 0m31 secs
       --listParSeq3: 1m39 secs
       --movesAndBoards = genPossibleMoveBoards2 board color

       movesAndBoards = listParSeq2 $ genPossibleMoveBoards2 board color

       moveBoards = map snd movesAndBoards
       moves = map fst movesAndBoards

       moveTrees' = map (genBoardTree depth color) moveBoards
       moveTrees = --trace (showListLines moveTrees')
                          moveTrees'


       boardRatings = map (minimax' depth getBoardScore isMaxi) moveTrees

       moveAndBoardRatings1 = zip moves boardRatings

       moveAndBoardRatings2 =
         map (\ (move, maybeRating) ->
                  if isJust maybeRating
                  then Right (move, extractJust maybeRating)
                  else Left "couldn't get rating")
             moveAndBoardRatings1

       moveAndBoardRatings3 = listFilterLeft moveAndBoardRatings2

       goodMove =
         if null moveAndBoardRatings3
         then Nothing
         else Just $
            foldl1 (\acc@(accMove, accRating) pair@(move, rating)
                              -> if testSign rating accRating
                                 then pair
                                 else acc)
                   moveAndBoardRatings3

   in if False --set to false to disable debug stuff
      then trace (L.intercalate "\n" $ map show moveAndBoardRatings3)
            --("computer move:" ++ (show goodMove))
            $ goodMove
      else goodMove




