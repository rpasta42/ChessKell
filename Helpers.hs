module Helpers
( posToCoord, coordToPos, moveToStr
, flipColor
, coordEq
, getPieceCoord
, pieceMovesTo2
) where

import qualified Data.Matrix as M
import qualified Data.Char as C
import Types


--posToCoord/coordToPos/moveToStr
posToCoord :: Position -> Coord
posToCoord (cX, y) = (numX, y)
   where numX = C.ord cX - C.ord 'A' + 1

coordToPos :: Coord -> Position
coordToPos (x, y) = (cX, y)
   where cX = C.chr (C.ord 'A' + x - 1)

numToChr x = C.chr $ C.ord '0' + x

--moveToStr $ Move (('a', 1), ('b', 2))
moveToStr (Move ((x1, y1), (x2,y2))) =
   [C.toLower x1] ++ [numToChr y1] ++ [C.toLower x2] ++ [numToChr y2]




--small funcs

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

coordEq a b = a == b

getPieceCoord :: BoardPiece -> Coord
getPieceCoord p = posToCoord $ getPosition p


pieceMovesTo2 :: PieceMoves -> PieceMoves2
pieceMovesTo2 (_, caps, moves) = (caps, moves)



