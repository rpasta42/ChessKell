import Types
import Utils

import qualified Data.Char as C

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

   in (wPawns++wOtherPieces, bPawns++bOtherPieces)




--mkMove :: Board -> Postion -> Position -> ChessRet Board
--mkMove board from to =

--showBoard b =

getPossibleMoves :: Board -> [(Position, Position)]
getPossibleMoves _ = []




getPieceMoves board = filter isMoveOnBoard $ getPieceMoves' board

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


---Utils


