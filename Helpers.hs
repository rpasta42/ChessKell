module Helpers
( posToCoord, coordToPos, moveToStr
, mkBoard, mkBoardFromPair
, mkPiece, mkPieceW, mkPieceB, mkPieceNoMoves
, flipBoardColor, setPieceMoves
, flipColor
, coordEq
, getPieceCoord
, pieceMovesTo2
, isCoordOnBoard
, elem'
, strToMove
) where

import qualified Data.Matrix as M
import qualified Data.Char as C
import qualified Data.List as L
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


---start make functions

mkBoard whitePieces blackPieces lastMove nextToMove =
   Board { getWhitePieces=whitePieces
         , getBlackPieces=blackPieces
         , getLastMove=lastMove
         , getNextPlayer=nextToMove
         }

mkBoardFromPair lastMove nextPlayer
                (wPieces, bPieces) =
   mkBoard wPieces bPieces lastMove nextPlayer



mkPiece color piece pos moved moves =
   BoardPiece { getColor = color
              , getPiece = piece
              , getPosition = pos
              , getHaveMoved = moved
              , getMoves = moves
              }

mkPieceW = mkPiece White
mkPieceB = mkPiece Black

mkPieceNoMoves color piece pos moved =
   mkPiece color piece pos moved Nothing

--end make functions


--small funcs


setPieceMoves :: BoardPiece -> PieceMoves2 -> BoardPiece
setPieceMoves (BoardPiece { getPiece = piece
                          , getColor = color
                          , getPosition = pos
                          , getHaveMoved = haveMoved
                          })
              moves =
   mkPiece color piece pos haveMoved $ Just moves

flipBoardColor :: Board -> Board
flipBoardColor (Board { getWhitePieces = wPieces
                      , getBlackPieces = bPieces
                      , getNextPlayer = color
                      , getLastMove = lastMove
                      }) =
   mkBoard wPieces bPieces lastMove (flipColor color)

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

coordEq a b = a == b

getPieceCoord :: BoardPiece -> Coord
getPieceCoord p = posToCoord $ getPosition p


pieceMovesTo2 :: PieceMoves -> PieceMoves2
pieceMovesTo2 (_, caps, moves) = (caps, moves)

isCoordOnBoard :: Coord -> Bool
isCoordOnBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

elem' :: (Eq a, Foldable t) => t a -> a -> Bool
elem' = flip L.elem


--"a1,b2"
strToMove :: String -> ChessRet Move
strToMove s =
   let splitted@(part1, part2) = splitAt 2 s
       (fromStr, toStr) = (part1, tail part2)
   in Right $ Move (strToPos fromStr, strToPos toStr)
      where
         strToPos s =
            let (x, y) = splitAt 1 s
                pos = (C.toUpper $ head x, C.digitToInt $ head y)
            in pos




