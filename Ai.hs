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
import MiniMax
import qualified Data.Monoid as Monoid
import Debug.Trace

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
   let getBoardScore = Monoid.getSum . mconcat
       getPieceScore = Monoid.Sum . getPieceValue . getPiece
       wScore = getBoardScore $ map getPieceScore wPieces
       bScore = getBoardScore $ map getPieceScore bPieces
       boardScore = wScore - bScore

       (maybeWinnerB, isDrawB) = getMatesAndStales b Black
       (maybeWinnerW, isDrawW) = getMatesAndStales b White
       checkMateScore =
         case (maybeWinnerW, maybeWinnerB) of
            (Just _, _) -> 100000
            (_, Just _) -> -100000
            _           -> 0

       finalScore = boardScore + checkMateScore

   in finalScore



genBoardTree' :: Int -> Color -> Board -> MoveTree Board
genBoardTree' depth color board = genBoardTree board depth color

genBoardTree :: Board -> Int -> Color -> MoveTree Board
genBoardTree board depth toPlay
   | depth == 0 = MoveTreeLeaf board
   | otherwise  = MoveTreeNode board subTree
      where subTree = map (genBoardTree' (depth - 1) (flipColor toPlay))
                          $ genPossibleMoveBoards board toPlay



getAiMove :: Board -> Color -> Int -> Maybe (Move, Int)
getAiMove board color depth =
   let allMoves = getPossibleMoves board color

       movesAndBoards = genPossibleMoveBoards2 board color
       moveBoards = map snd movesAndBoards
       moves = map fst movesAndBoards

       moveTrees = map (genBoardTree' depth color) moveBoards

       isMaxi = color == White
       testSign :: Ord a => a -> a -> Bool
       testSign = if isMaxi then (>) else (<)

       boardRatings = map (minimax' depth getBoardScore isMaxi) moveTrees

       moveAndBoardRatings1 = zip moves boardRatings

       moveAndBoardRatings2 =
         map (\ (move, maybeRating) ->
                  if isJust maybeRating
                  then Right (move, extractJust maybeRating)
                  else Left "couldn't get rating")
             moveAndBoardRatings1

       moveAndBoardRatings3 = listFilterLeft moveAndBoardRatings2

       goodMove = foldl (\acc pair@(move, rating)
                           -> if not $ isJust acc
                              then Just pair
                              else let (Just (moveAcc, ratingAcc)) = acc
                                   in if testSign rating ratingAcc
                                      then Just pair
                                      else acc)
                        Nothing
                        moveAndBoardRatings3

   in trace --(L.intercalate "\n" $ map show moveAndBoardRatings)
            ("computer move:" ++ (show goodMove))
            $ goodMove



