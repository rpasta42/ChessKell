module ChessUtils
( posToCoord, coordToPos
, boardToMatrix, displayBoardByColor
, mkPiece, mkBoard
, getAllBoardPieces, getBoardPieceByPos, getBoardPieceByCoord
, getBoardPieceByPiece
, getPieceCoord
, removePiece, removePieceByPos
, movePiece, movePiece'
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



newGameBoard =
   let board1 = newMatrix 10 10 " "
       board2 =
         matrixMap board1
                   (\e (x,y) _ _ ->
                        if x == 10 && y <= 8
                        then [C.intToDigit y]
                        else if y == 10 && x <= 8
                             then [C.chr (C.ord 'A' + x - 1)]
                             else e)
                   " "
   in board2


boardToMatrix :: Board -> M.Matrix String
boardToMatrix b = displayBoardByColor b White

displayBoardByColor :: Board -> Color -> M.Matrix String
displayBoardByColor b c =
   let m = newGameBoard --newMatrix 8 8 " "
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
   in let ret1 = setFromPieces m $ getAllBoardPieces b
          ret2 = let lsts = M.toLists ret1
                     reversed = reverse . init . init $ lsts
                 in M.fromLists $ reversed ++ [last $ init lsts]
                                           ++ [last lsts]
                 --M.fromLists . reverse . M.toLists $ ret1
      in if c == Black then ret1 else ret2


mkPiece color piece pos moved =
   BoardPiece { getColor = color
              , getPiece = piece
              , getPosition = pos
              , getHaveMoved = moved
              }

mkPieceW = mkPiece White
mkPieceB = mkPiece Black

mkBoard whitePieces blackPieces =
   Board { getWhitePieces=whitePieces
         , getBlackPieces=blackPieces
         }

mkBoardFromPair (wPieces, bPieces) = mkBoard wPieces bPieces

--getAllBoardPieces/getBoardPieceByPos/getBoardPieceByCoord/GetBoardPieceByPiece

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

getBoardPieceByPiece :: [BoardPiece] -> Piece -> ChessRet BoardPiece
getBoardPieceByPiece pieces piece =
   foldl (\ acc bPiece@(BoardPiece { getPiece=p }) ->
            if isRight acc
            then acc
            else if p == piece
                 then Right bPiece
                 else acc)
         (Left "piece not found")
         pieces



getPieceCoord :: BoardPiece -> Coord
getPieceCoord p = posToCoord $ getPosition p

---removePiece/removePieceByPos

removePiece :: Board -> BoardPiece -> ChessRet Board
removePiece b p@(BoardPiece { getColor=c, getPosition=pos }) =
   let wPieces = getWhitePieces b
       bPieces = getBlackPieces b
       newPieces' = case c of
          White -> (deleteLstItem wPieces p, Right bPieces)
          Black -> (Right wPieces, deleteLstItem bPieces p)
       newPieces = pairEitherToEitherPair newPieces'
       newBoard = mkBoardFromPair <$> newPieces
   in newBoard

removePieceByPos :: Board -> Position -> ChessRet Board
removePieceByPos board pos =
   let piece = getBoardPieceByPos board pos
   in piece >>= removePiece board


movePiece' :: Board -> Position -> PieceMoves
           -> BoardPiece -> ChessRet Board
movePiece' b newPos pMoves piece = movePiece b piece pMoves newPos

--no checks for valid moves
movePiece :: Board -> BoardPiece -> PieceMoves
          -> Position -> ChessRet Board
movePiece board
          piece@(BoardPiece {getPiece=p, getColor=c})
          pMoves@(_, captures, moves)
          newPos =
   let newBoard = removePieceAtPos board newPos
       newPiece = mkPiece c p newPos True
       wPieces = getWhitePieces newBoard
       bPieces = getBlackPieces newBoard
       wIndex = L.elemIndex piece wPieces
       bIndex = L.elemIndex piece bPieces
   in case (wIndex, bIndex) of
         (Just wIndex', Nothing) ->
            Right $ mkBoard (replaceLstIndex wPieces wIndex' newPiece)
                            bPieces
         (Nothing, Just bIndex') ->
            Right $ mkBoard wPieces
                            (replaceLstIndex bPieces bIndex' newPiece)
         _ -> Left $ "movePiece pattern fail: piece not found"
                     ++ " or both in black/white"

removePieceAtPos :: Board -> Position -> Board
removePieceAtPos b pos =
   case getBoardPieceByPos b pos of
      (Left _)  -> b
      (Right piece) ->
         let color = getColor piece
             wPieces = getWhitePieces b
             bPieces = getBlackPieces b
         in if color == White
            then mkBoard (extractRight $ deleteLstItem wPieces piece) bPieces
            else mkBoard wPieces (extractRight $ deleteLstItem bPieces piece)



--100 centipawn (cp) = 1 pawn
getPieceValue :: Piece -> Int
getPieceValue Pawn = 100
getPieceValue Knight = 300
getPieceValue Bishop = 301
getPieceValue Rook = 500
getPieceValue Queen = 900
getPieceValue King = 10000




--small funcs

flipColor :: Color -> Color
flipColor White = Black
flipColor Black = White

coordEq a b = a == b


