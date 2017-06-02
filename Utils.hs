module Utils
( boardToMatrix
, listSingletonExtract
, mkPiece
, getAllBoardPieces, getBoardPieceByPos
, getPieceCoord
, posToCoord, coordToPos
, mkBoard
, flipColor
, movePiece
) where

import Types
import qualified Data.Matrix as M
import qualified Data.Char as C
import qualified Data.List as L


newMatrix width height = M.matrix width height (\_ -> " ")

--posToCoord/coordToPos
posToCoord :: Position -> Coord
posToCoord (cX, y) = (numX, y)
   where numX = C.ord cX - C.ord 'A' + 1

coordToPos :: Coord -> Position
coordToPos (x, y) = (cX, y)
   where cX = C.chr (C.ord 'A' + x - 1)


boardToMatrix :: Board -> M.Matrix String
boardToMatrix b =
   let m = newMatrix 8 8
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


flipColor White = Black
flipColor Black = White

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

getPieceCoord :: BoardPiece -> Coord
getPieceCoord p = posToCoord $ getPosition p


--no checks
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
      then let (part1, part2) = L.splitAt (extractJust wIndex) wPieces
               newPiecesW = part1 ++ [newPiece] ++ part2
           in Right $ mkBoard newPiecesW bPieces
      else if isJust bIndex
           then let (part1, part2) = L.splitAt (extractJust bIndex)
                                               bPieces
                    newPiecesB = part1 ++ [newPiece] ++ part2
                in Right $ mkBoard wPieces newPiecesB
           else Left "movePiece fail: piece not found"





--generic utilities

extractJust (Just y) = y

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

listSingletonExtract :: [a] -> ChessRet a
listSingletonExtract lst
   | length lst == 1    = Right $ Prelude.head lst
   | otherwise = Left "list Singleton extraction failed"


