module Ai
(
) where

import Types
import Utils
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


generateBoardTree :: Board -> Int -> MoveTree Board
generateBoardTree board depth
   | depth == 0 = MoveTreeLeaf board
   | otherwise  = MoveTreeNode board
                               (generate




