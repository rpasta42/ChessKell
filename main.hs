
import qualified Data.Matrix as M
import qualified Data.Char as C

--cabal install matrix

data Piece = Pawn | Rook | Knight | Bishop | King | Queen | Empty
   deriving (Enum) --Eq, Ord, Bounded, Enum)

instance Show Piece where
   show x = [show' x]
      where show' Pawn = 'p'
            show' Rook = 'r'
            show' Knight = 'h'
            show' Bishop = 'b'
            show' Queen = 'q'
            show' King = 'k'

instance Eq Piece where
   (==) Pawn Pawn = True
   (==) Rook Rook = True
   (==) Knight Knight = True
   (==) Bishop Bishop = True
   (==) Queen Queen = True
   (==) King King = True
   (==) _ _ = False


newtype OrdPiece = OrdPiece { ordPieceGet :: Piece }

instance Eq OrdPiece where
   (==) a b = helper (ordPieceGet a) (ordPieceGet b)
      where helper Pawn Pawn = True
            helper Rook Rook = True
            helper Knight Knight = True
            helper Bishop Bishop = True
            helper Queen Queen = True
            helper King King = True

            helper Knight Bishop = True
            helper Bishop Knight = True
            helper _ _ = False

instance Ord OrdPiece where
   compare a b = compare' (ordPieceGet a) (ordPieceGet b)
      where compare' Pawn Pawn = EQ
            compare' Rook Rook = EQ
            compare' Knight Knight = EQ
            compare' Bishop Bishop = EQ
            compare' Queen Queen = EQ
            compare' King King = EQ

            compare' Pawn b
               | b == Pawn = EQ
               | otherwise = LT

            compare' Rook b
               | b == Queen || b == King = LT
               | otherwise = GT

            compare' Knight b
               | b == Rook || b == King || b == Queen = LT
               | b == Bishop = EQ
               | otherwise = GT

            compare' Bishop b = compare' Knight b

            compare' Queen b
               | b == King = LT
               | otherwise = GT

            compare' King _ = GT

            compare' b a = compare' a b


data Color = White | Black
               deriving (Show, Eq)

type Position = (Char, Int)
type Coord = (Int, Int)

data BoardPiece = BoardPiece { getPiece :: Piece
                             , getColor :: Color
                             , getPosition :: Position
                             } deriving (Show)

type Board = ([BoardPiece], [BoardPiece])


type ChessRet a = Either String a


mkPiece color piece pos = BoardPiece { getColor = color, getPiece = piece, getPosition = pos }
mkPieceW = mkPiece White
mkPieceB = mkPiece Black

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




mkMove :: Board -> Postion -> Position -> ChessRet Board
mkMove board from to =

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

newMatrix width height = M.matrix width height (\_ -> " ")

posToCoord :: Position -> Coord
posToCoord (cX, y) = (numX, y)
   where numX = C.ord cX - C.ord 'A' + 1

