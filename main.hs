--cabal install matrix

import Types
import Utils
import ChessUtils
import qualified Data.List as L
import System.Posix.Unistd (sleep)
import Debug.Trace


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
       board = mkBoard (wPawns++wOtherPieces) (bPawns++bOtherPieces)
   in extractRight $ removePieceByPos board ('A', 2)

mkMove :: Board -> Position -> Position -> ChessRet Board
mkMove board from to =
   do piece <- getBoardPieceByPos board from
      pieceMoves <- fmap L.concat $ getPieceMoves board piece
      ret <-
         if (posToCoord to) `elem` pieceMoves
         then movePiece board piece to
         else Left "mkMove: not a valid move"
      return ret



{-getPieceCaptures :: Board -> [[Coord]] -> BoardPiece -> ChessRet ([Coord], [Coord])
--takes board and bPiece and returns a pair:
--fst: with list of all pieces it can capture, and
--snd: a list of all the possible non-capture moves-}

getPieceCaptures :: Board ->  [[Coord]] -> BoardPiece -> ChessRet ([Coord], [Coord])

{-getPieceCaptures b moves bPiece@(BoardPiece { getPiece = Knight }) =
   let flatMoves = concat moves
       moveCapturePieces = listFilterLeft $ map (getBoardPieceByCoord b) flatMoves
       moveCaptureCoords = map (posToCoord . getPosition) moveCapturePieces
       nonCaptureMoves = filter (\x -> not $ x `elem` moveCaptureCoords) flatMoves
   in Right (moveCaptureCoords, nonCaptureMoves)-}

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
                cap1 = (posToCoord . getPosition) <$> getBoardPieceByCoord b (x1, y1)
                cap2 = (posToCoord . getPosition) <$> getBoardPieceByCoord b (x2, y1)

                pieceAtC1 = cap1 >>= getBoardPieceByCoord b
                pieceAtC2 = cap2 >>= getBoardPieceByCoord b

                colorC1 = (\x -> getColor x) <$> pieceAtC1
                colorC2 = (\x -> getColor x) <$> pieceAtC2

                goodP1 = case colorC1 of
                  Right color -> if color /= c && (isMoveOnBoard $ extractRight cap1)
                                 then cap1
                                 else Left "bad"
                  Left x -> Left "bad"

                goodP2 = case colorC2 of
                  Right color -> if color /= c && (isMoveOnBoard $ extractRight cap2)
                                 then cap2
                                 else Left "bad"
                  Left x -> Left "bad"

                {-goodP1 = if isRight pAtCap1'
                         then let (BoardPiece {getColor=color}) = extractRight pAtCap1'
                              in if color == c then Left "no" else cap1
                         else Left "no"
                goodP2 = if isRight pAtCap2'
                         then let (BoardPiece {getColor=color}) = extractRight pAtCap2'
                              in if color == c then Left "no" else cap2
                         else Left "no"-}

             in listFilterLeft [goodP1, goodP2]

getPieceCaptures b moves bPiece@(BoardPiece {getColor=color})  = Right getMoves where
   --moves = reverse moves'
   pos = getPosition bPiece
   (x,y) = posToCoord pos
   getMoves' = map (foldl (\ acc@(prevOccupied, accCaps, accMoves) coord@(cX, cY) ->
                              if {-trace (if getPiece bPiece == Bishop then ("prevOccupied: " ++ (show $ getPiece bPiece) ++ " " ++ (show acc) ++ " curr: " ++ (show pos)) else "")
                                       $-} prevOccupied
                              then acc
                              else let bPieceAtCoord' = getBoardPieceByCoord b coord
                                   in if isRight bPieceAtCoord'
                                      then let bPieceAtCoord = extractRight bPieceAtCoord'
                                               colorPCoord = getColor bPieceAtCoord
                                           in if colorPCoord == color
                                              then (True, accCaps, accMoves)
                                              else (True, coord:accCaps, accMoves)
                                      else (prevOccupied, accCaps, coord:accMoves))
                          (False, [], []))
                   moves
   getMoves =
      let allCapts = concat $ map (\(_, caps, moves) -> caps) getMoves'
          allMoves = concat $ map (\(_, caps, moves) -> moves) getMoves'
       in (allCapts, allMoves)





--does not account for putting king under check
getPieceMoves :: Board -> BoardPiece -> ChessRet ([Coord], [Coord])
getPieceMoves board bPiece =
   let pPiece = getPiece bPiece
       pCoord = getPieceCoord bPiece
       pieceMoves1 = map (filter isMoveOnBoard) $ getPieceMoves' board bPiece
       pieceMoves2 = map (filter (not . coordEq pCoord)) pieceMoves1
       pieceMoves3 = pieceMoves2 --map (filter (not . putUnderCheck board)) pieceMoves2
       pieceMoves4 = pieceMoves3 --map (filter (not . moveOnOwnPiece board bPiece)) pieceMoves3
       pieceMoves5 = pieceMoves4 --map (filter (not . isIllegalJump board bPiece)) pieceMoves4
   in do capsAndMoves@(captures, moves) <- getPieceCaptures board pieceMoves5 bPiece
         if length captures == 0 && length moves == 0
         then Left "no moves"
         else return capsAndMoves

isMoveOnBoard :: Coord -> Bool
isMoveOnBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

moveOnOwnPiece :: Board -> BoardPiece -> Coord -> Bool
moveOnOwnPiece board fromPiece to =
   let pColor = getColor fromPiece
       destPieceEither = getBoardPieceByCoord board to
   in if isRight destPieceEither
      then let destPiece = extractRight destPieceEither
               destPieceColor = getColor destPiece
           in destPieceColor == pColor
      else False

getPieceMoves' :: Board -> BoardPiece -> [[Coord]]
getPieceMoves' b bPiece =
   let piece = getPiece bPiece
       color = getColor bPiece
       pos   = getPosition bPiece
       moved = getHaveMoved bPiece

       helper' :: Piece -> (Int, Int) -> [[Coord]]

       helper' Rook (x,y) =
         let xs1 = [x..x+8]
             xs2 = reverse [x-8..x]
             ys1 = [y..y+8]
             ys2 = reverse [y-8..y]
             xMoves1 = map (\x -> (x,y)) xs1
             xMoves2 = map (\x -> (x,y)) xs2
             yMoves1 = map (\y -> (x,y)) ys1
             yMoves2 = map (\y -> (x,y)) ys2
         in [xMoves1, xMoves2, yMoves1, yMoves2]

       helper' Bishop (x,y) =
         let xs1 = [x+1..x+8]
             xs2 = reverse [x-8..x-1]
             ys1 = [y+1..y+8]
             ys2 = reverse [y-8..y-1]
         in [ zip xs1           ys1
            , zip xs2           ys2
            , zip xs2           ys1
            , zip xs1           ys2
            {-, zip
            , zip (reverse xs) (reverse ys)
            , zip xs           (reverse ys)
            , zip (reverse xs) ys-}
            ]
         {-let xs = [x-8..x+8]
             ys = [y-8..y+8]
         in [ reverse $ zip xs           ys
            , zip (reverse xs) (reverse ys)
            , zip xs           (reverse ys)
            , zip (reverse xs) ys
            ]-}

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

       helper' King (x,y) = --TODO: check
         let xs = [x-1..x+1]
             ys = [y-1..y+1]
             --xMoves1 = map (\x -> (x,y)) xs
             --yMoves1 = map (\y -> (x,y)) ys
             --horizontal = zip xs ys
         in [[(x,y)] | x <- xs, y <- ys] --zip xs ys

       helper' Pawn (x,y) = --TODO: capturing, 2 moves, en-passant
         let newWhite1 = (x, y+1)
             newWhite2 = (x, y+2)
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


putUnderCheck board to = True

--getPossibleMoves :: Board -> [(Position, Position)]
--getPossibleMoves _ = []


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

--tests:
x1 = newGame
x2 = boardToMatrix x1

x3 = getBoardPieceByPos x1 ('C', 1)
x4 = x3 >>= getPieceMoves x1

x5 = mkMove x1 ('C', 1) ('D', 2)
x6 = fmap boardToMatrix x5
(Right x7) = x6

--fmap (map coordToPos) x5

test1 =
   let board = newGame
       wPieces = getWhitePieces board
       wMoves = map (getPieceMoves board) wPieces

       zipped1 = zip (map Right wPieces) wMoves
       zipped2 = listFilterLeft $ map pairEitherToEitherPair zipped1

       --[(piece, [moves])]
       pieceMoveCombos1 = map (\(piece, (caps, movs)) -> (piece, caps ++ movs)) zipped2

       pieceMoveCombos2 = map (\(piece, moves) -> (map (\move -> (piece, move)) moves))
                             pieceMoveCombos1
       pieceMoveCombos3 = concat pieceMoveCombos2

       helper [] = return ()
       helper (x@(piece@(BoardPiece {getPiece=p}), moveTo):xs) =
          let pieceCoord = getPieceCoord piece
              moveToPos = coordToPos moveTo
              newBoard = movePiece board piece moveToPos
              bMatrix = boardToMatrix <$> newBoard
          in if isRight bMatrix && p /= Pawn && p == Knight -- && p /= Knight && p == Rook -- && p == Rook -- && p == Bishop
             then do --displayMatrix . matrixToDisplay . extractRight $ bMatrix
                     print $ extractRight bMatrix
                     --sleep 1
                     helper xs
             else helper xs

   in  helper pieceMoveCombos3

main = test1



