module Ai
(
) where

import Utils


--100 centipawn (cp) = 1 pawn
getPieceValue :: Piece -> Int
getPieceValue Pawn = 100
getPieceValue Knight = 300
getPieceValue Bishop = 301
getPieceValue Rook = 500
getPieceValue Queen = 900
getPieceValue King = 10000


getBoardScore :: Board -> Int
getWhitePieces

