module Utils
( boardToMatrix
, mkPiece
, getAllBoardPieces
, posToCoord
) where

import Types
import qualified Data.Matrix as M
import qualified Data.Char as C

newMatrix width height = M.matrix width height (\_ -> " ")

posToCoord :: Position -> Coord
posToCoord (cX, y) = (numX, y)
   where numX = C.ord cX - C.ord 'A' + 1


boardToMatrix :: Board -> M.Matrix String
boardToMatrix b@(white, black) =
   let m = newMatrix 8 8
       setFromPieces [] m = m
       setFromPieces (x:xs) m =
         let coord@(x_,y_) = posToCoord $ getPosition x
             piece = getPiece x
             color = getColor x
             pieceChar = if color == White
                         then map C.toUpper $ show piece
                         else show piece
             newM = M.setElem pieceChar (y_,x_) m
         in setFromPieces xs newM
   in setFromPieces black (setFromPieces white m)



mkPiece color piece pos = BoardPiece { getColor = color, getPiece = piece, getPosition = pos }
mkPieceW = mkPiece White
mkPieceB = mkPiece Black


getAllBoardPieces :: Board -> [BoardPiece]
getAllBoardPieces (Board { getWhitePieces = w
                         , getBlackPieces = b
                         }) =
   w ++ b



