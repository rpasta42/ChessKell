
import Data.Matrix

--cabal install matrix

data Piece = Pawn | Rook | Knight | Bishop | Queen | King | Empty
   deriving (Show, Enum) --Eq, Ord, Bounded, Enum)

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
               deriving (Show)

type Position = (Char, Int)

data BoardPiece = BoardPiece { getPiece :: Piece
                             , getColor :: Color
                             , getPosition :: Position
                             } deriving (Show)

type Board = ([BoardPiece], [BoardPiece])




mkPiece color piece pos = BoardPiece { getColor = color, getPiece = piece, getPosition = pos }
mkPieceW = mkPiece White
mkPieceB = mkPiece Black


newGame :: Board
newGame =
   let mkPawns row color = map (\x -> mkPiece color Pawn (x, row)) ['A'..'H']
       wPawns = mkPawns 2 White
       bPawns = mkPawns 7 Black

       otherPieces = [Rook .. King] ++ reverse [Knight .. Rook]
       mkOtherPieces color y = map (\(piece, x) -> mkPiece color piece (x, y)) $ zip otherPieces ['A'..'H']
       wOtherPieces = mkOtherPieces White 1
       bOtherPieces = mkOtherPieces Black 8

   in (wPawns++wOtherPieces, bPawns++bOtherPieces)



boardToMatrix

showBoard b =

getPossibleMoves :: Board -> [(Position, Position)]
getPossibleMoves _ = []


