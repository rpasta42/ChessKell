module ChessUtils
( posToCoord, coordToPos
, boardToMatrix
, mkPiece, mkBoard
, getAllBoardPieces, getBoardPieceByPos, getBoardPieceByCoord
, getPieceCoord
, movePiece
, flipColor
, coordEq
) where

--imports
import Types
import Utils
import qualified Data.Matrix as M
import qualified Data.Char as C
import qualified Data.List as L


--posToCoord/coordToPos
posToCoord :: Position -> Coord
posToCoord (cX, y) = (numX, y)
   where numX = C.ord cX - C.ord 'A' + 1

coordToPos :: Coord -> Position
coordToPos (x, y) = (cX, y)
   where cX = C.chr (C.ord 'A' + x - 1)


boardToMatrix :: Board -> M.Matrix String
boardToMatrix b =
   let m = newMatrix 8 8 " "
       setFromPieces m [] = m
       setFromPieces m (x:xs) =
         let coord@(x_,y_) = posToCoord $ getPosition x
             piece = getPiece x
             color = getColor x
             pieceChar = if color == White
                         then map C.toUpper $ show piece
                         else show piece
             newM = M.setElem pieceChar (y_,x_) m
         in setFromPieces newM xs
   in setFromPieces m $ getAllBoardPieces b


mkPiece color piece pos = BoardPiece { getColor = color
                                     , getPiece = piece
                                     , getPosition = pos
                                     }
mkPieceW = mkPiece White
mkPieceB = mkPiece Black

mkBoard whitePieces blackPieces =
   Board { getWhitePieces=whitePieces
         , getBlackPieces=blackPieces
         }


--getAllBoardPieces/getBoardPieceByPos/getBoardPieceByCoord

getAllBoardPieces :: Board -> [BoardPiece]
getAllBoardPieces b =
   getWhitePieces b ++ getBlackPieces b

getBoardPieceByPos :: Board -> Position -> ChessRet BoardPiece
getBoardPieceByPos b pos =
   let pieces = getAllBoardPieces b
       --pos = coordToPos coord
       coords = filter (\piece -> getPosition piece == pos)
                       pieces
   in listSingletonExtract coords

getBoardPieceByCoord :: Board -> Coord -> ChessRet BoardPiece
getBoardPieceByCoord b coord = getBoardPieceByPos b (coordToPos coord)


getPieceCoord :: BoardPiece -> Coord
getPieceCoord p = posToCoord $ getPosition p


--no checks for valid moves
movePiece :: Board -> BoardPiece -> Position -> ChessRet Board
movePiece board
          piece@(BoardPiece {getPiece=p, getColor=c})
          newPos =
   let newPiece = mkPiece c p newPos
       wPieces = getWhitePieces board
       bPieces = getBlackPieces board
       wIndex = L.elemIndex piece wPieces
       bIndex = L.elemIndex piece bPieces

   in if isJust wIndex
      then let (part1, (_:part2)) = L.splitAt (extractJust wIndex) wPieces
               newPiecesW = part1 ++ [newPiece] ++ part2
           in Right $ mkBoard newPiecesW bPieces
      else if isJust bIndex
           then let (part1, (_:part2)) = L.splitAt (extractJust bIndex)
                                                   bPieces
                    newPiecesB = part1 ++ [newPiece] ++ part2
                in Right $ mkBoard wPieces newPiecesB
           else Left "movePiece fail: piece not found"


--small funcs

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

coordEq a b = a == b


