--cabal install matrix

import Types
import Utils
import ChessUtils
import qualified Data.List as L


newGame :: Board
newGame =
   let mkPawns row color = map (\x -> mkPiece color Pawn (x, row) False)
                               ['A'..'H']
       wPawns = mkPawns 2 White
       bPawns = mkPawns 7 Black

       otherPieces = [Rook .. Queen] ++ reverse [Rook .. Bishop]
       mkOtherPieces color y = map (\(piece, x)
                                       -> mkPiece color piece (x, y) False)
                                   $ zip otherPieces ['A'..'H']
       wOtherPieces = mkOtherPieces White 1
       bOtherPieces = mkOtherPieces Black 8

   in mkBoard (wPawns++wOtherPieces) (bPawns++bOtherPieces)

mkMove :: Board -> Position -> Position -> ChessRet Board
mkMove board from to =
   do piece <- getBoardPieceByPos board from
      pieceMoves <- fmap L.concat $ getPieceMoves board piece
      ret <-
         if (posToCoord to) `elem` pieceMoves
         then movePiece board piece to
         else Left "mkMove: not a valid move"
      return ret


--movePiece board piece to =
--showBoard b =


moveOnOwnPiece :: Board -> BoardPiece -> Coord -> Bool
moveOnOwnPiece board fromPiece to =
   let pColor = getColor fromPiece
       destPieceEither = getBoardPieceByCoord board to
   in if isRight destPieceEither
      then let destPiece = extractRight destPieceEither
               destPieceColor = getColor destPiece
           in destPieceColor /= pColor
      else False

putUnderCheck board to = True

isIllegalJump board bPiece to = True

--takes board and bPiece and returns a pair:
--fst: with list of all pieces it can capture, and
--snd: a list of all the possible non-capture moves
getPieceCaptures :: Board ->  [[Coord]] -> BoardPiece -> ChessRet ([Coord], [Coord])

getPieceCaptures b moves bPiece@(BoardPiece { getPiece = Knight }) =
   let flatMoves = concat moves
       moveCapturePieces = listFilterLeft $ map (getBoardPieceByCoord b) flatMoves
       moveCaptureCoords = map (posToCoord . getPosition) moveCapturePieces
       nonCaptureMoves = filter (\x -> not $ x `elem` moveCaptureCoords) flatMoves
   in Right (moveCaptureCoords, nonCaptureMoves)


getPieceCaptures b moves
                 bPiece@(BoardPiece { getPiece = Pawn, getColor = c, getPosition = pos })
   = Right (getCaptures, getMoves)
      where
         (x,y) = posToCoord pos
         getMoves' = map (foldr (\ coord@(cX, cY) acc@(previousOccupied, accLst) ->
                                   let bPieceAtCoord = getBoardPieceByCoord b coord
                                   in if previousOccupied
                                      then acc
                                      else if isRight bPieceAtCoord
                                           then (True, accLst)
                                           else (False, coord:accLst))
                                (False, []))
                          moves
         getMoves = concat $ map snd getMoves'

         getCaptures =
            let y1 = if c == White then y+1 else y-1
                x1 = x+1
                x2 = x-1
                cap1 = (posToCoord . getPosition) <$> getBoardPieceByCoord b (y1, x1)
                cap2 = (posToCoord . getPosition) <$> getBoardPieceByCoord b (y1, x2)
             in listFilterLeft [cap1, cap2]



getPieceCaptures b moves bPiece = Right getMoves where
   pos = getPosition bPiece
   (x,y) = posToCoord pos
   getMoves' = map (foldr (\ coord@(cX, cY) acc@(prevOccupied, accCaps, accMoves) ->
                              if prevOccupied
                              then acc
                              else let bPieceAtCoord = getBoardPieceByCoord b coord
                                   in if isRight bPieceAtCoord
                                      then (True, coord:accCaps, accMoves)
                                      else (False, accCaps, coord:accMoves))
                          (False, [], []))
                   moves
   getMoves =
      let allCapts = concat $ map (\(_, caps, moves) -> caps) getMoves'
          allMoves = concat $ map (\(_, caps, moves) -> moves) getMoves'
       in (allCapts, allMoves)





--getPieceCaptures board bPiece moves = --Left ""
--   map foldr moves

getPieceMoves :: Board -> BoardPiece -> ChessRet [[Coord]]
getPieceMoves board bPiece =
   let pPiece = getPiece bPiece
       pCoord = getPieceCoord bPiece
       pieceMoves1 = map (filter isMoveOnBoard) $ getPieceMoves' board bPiece
       pieceMoves2 = map (filter (not . coordEq pCoord)) pieceMoves1
       pieceMoves3 = map (filter (not . putUnderCheck board)) pieceMoves2
       pieceMoves4 = map (filter (not . moveOnOwnPiece board bPiece)) pieceMoves3
       pieceMoves5 = map (filter (not . isIllegalJump board bPiece)) pieceMoves4
       possibleCaptures = getPieceCaptures board pieceMoves5 bPiece
   in if length pieceMoves5 == 0
      then Left "no moves"
      else Right pieceMoves5

isMoveOnBoard :: Coord -> Bool
isMoveOnBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

getPieceMoves' :: Board -> BoardPiece -> [[Coord]]
getPieceMoves' b bPiece =
   let piece = getPiece bPiece
       color = getColor bPiece
       pos   = getPosition bPiece
       moved = getHaveMoved bPiece

       helper' :: Piece -> (Int, Int) -> [[Coord]]
       helper' Rook (x,y) =
         let xs = [x-8..x+8]
             ys = [y+8..y-8]
             xMoves = map (\x -> (x,y)) xs
             yMoves = map (\y -> (x, y)) ys
         in [xMoves, yMoves]

       helper' Bishop (x,y) =
         let xs = [x-8..x+8]
             ys = [y-8..y+8]
         in [ zip xs           ys
            , zip (reverse xs) (reverse ys)
            , zip xs           (reverse ys)
            , zip (reverse xs) ys
            ]

       helper' Queen coord@(x,y) = helper' Rook coord ++
                                   helper' Bishop coord

       helper' Knight (x, y) =
         let f1 = (\n -> n+1)
             f2 = (\n -> n+2)
             f3 = (\n -> n-1)
             f4 = (\n -> n-2)
         in map (\(fx, fy) -> [(fx x, fy y)])
                [(f1, f2), (f1, f4), --(+1, +2), (+1, -2)
                 (f3, f2), (f3, f4), --(-1, +2), (-1, -2)
                 (f2, f1), (f2, f3), --(+2, +1), (+2, -1)
                 (f4, f1), (f4, f3)] --(-2, +1), (-2, -1)

       {-helper' King (x,y) = --TODO: check
         let xs = [x-1..x+1]
             ys = [y-1..y+1]
             xMoves1 = map (\x -> (x,y)) xs
             yMoves1 = map (\y -> (x,y)) ys
             horizontal = zip xs ys
         in zip xs ys-}

       helper' Pawn (x,y) = --TODO: capturing, 2 moves, en-passant
         let newWhite1 = (x, y+1)
             newWhite2 = (y, y+2)
             newBlack1 = (x, y-1)
             newBlack2 = (x, y-2)
             (newWhite, newBlack) =
                  if moved
                  then ([newWhite1], [newBlack1])
                  else ([newWhite1, newWhite2], [newBlack1, newBlack2])
          in if color == White
             then [newWhite]
             else [newBlack]

   in helper' piece $ posToCoord pos



getPossibleMoves :: Board -> [(Position, Position)]
getPossibleMoves _ = []


---Utils


{-
getPlayerMove = do
   putStrLn "enter move:"
   line <- getLine
   return line

loop board whosTurn =
   move <- getPlayerMove

   loop newBoard (flipColor whosTurn)


driver =
   board <- return newGame
   game <- loop board White
-}

x1 = newGame
x2 = boardToMatrix x1

x3 = getBoardPieceByPos x1 ('C', 1)
x4 = x3 >>= getPieceMoves x1

x5 = mkMove x1 ('C', 1) ('D', 2)
x6 = fmap boardToMatrix x5
(Right x7) = x6

--fmap (map coordToPos) x5


