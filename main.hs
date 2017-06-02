import Types
import Utils
import qualified Data.List as L

--cabal install matrix



newGame :: Board
newGame =
   let mkPawns row color = map (\x -> mkPiece color Pawn (x, row)) ['A'..'H']
       wPawns = mkPawns 2 White
       bPawns = mkPawns 7 Black

       otherPieces = [Rook .. Queen] ++ reverse [Rook .. Bishop]
       mkOtherPieces color y = map (\(piece, x) -> mkPiece color piece (x, y))
                                   $ zip otherPieces ['A'..'H']
       wOtherPieces = mkOtherPieces White 1
       bOtherPieces = mkOtherPieces Black 8

   in mkBoard (wPawns++wOtherPieces) (bPawns++bOtherPieces)


--movePiece board piece to =



mkMove :: Board -> Position -> Position -> ChessRet Board
mkMove board from to =
   do piece <- getBoardPieceByPos board from
      pieceMoves <- getPieceMoves board piece
      ret <-
         if (posToCoord to) `elem` pieceMoves
         then movePiece board piece to
         else Left "mkMove: not a valid move"
      return ret




--showBoard b =



notPutUnderCheck x = True

getPieceMoves :: Board -> BoardPiece -> ChessRet [Coord]
getPieceMoves board piece =
   let pieceMoves1 = filter isMoveOnBoard $ getPieceMoves' piece
       pieceMoves2 = filter (\ coord -> getPieceCoord piece /= coord)
                            pieceMoves1
       pieceMoves3 = filter notPutUnderCheck pieceMoves2
   in if length pieceMoves3 == 0
      then Left "no moves"
      else Right pieceMoves3

isMoveOnBoard :: Coord -> Bool
isMoveOnBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

getPieceMoves' :: BoardPiece -> [Coord]
getPieceMoves' (BoardPiece {getPiece=piece, getColor=color, getPosition=pos}) = helper' piece $ posToCoord pos
   where helper' Rook (x,y) =
            let xs = [x-8..x+8]
                ys = [y+8..y-8]
                xMoves = map (\x -> (x,y)) xs
                yMoves = map (\y -> (x, y)) ys
            in xMoves ++ yMoves
         helper' Bishop (x,y) =
            let xs = [x-8..x+8]
                ys = [y-8..y+8]
            in zip xs ys
         helper' Queen coord@(x,y) = helper' Rook coord ++ helper' Bishop coord
         helper' King (x,y) =
            let xs = [x-1..x+1]
                ys = [y-1..y+1]
                xMoves1 = map (\x -> (x,y)) xs
                yMoves1 = map (\y -> (x,y)) ys
                horizontal = zip xs ys
            in zip xs ys





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


