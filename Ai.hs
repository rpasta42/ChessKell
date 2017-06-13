module Ai
(
) where

import ChessUtils (flipColor)
import Types
import Utils
import Logic
import MiniMax
import qualified Data.Monoid as Monoid

--100 centipawn (cp) = 1 pawn
getPieceValue :: Piece -> Int
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
   in wScore - bScore


generateBoardTree' :: Color -> Int -> Board -> MoveTree Board
generateBoardTree' color depth board = generateBoardTree board depth color

generateBoardTree :: Board -> Int -> Color -> MoveTree Board
generateBoardTree board depth toPlay
   | depth == 0 = MoveTreeLeaf board
   | otherwise  = MoveTreeNode board subTree
      where subTree = map (generateBoardTree' (flipColor toPlay) (depth - 1))
                          $ genPossibleMoveBoards board toPlay




