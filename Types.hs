module Types
( ChessRet
, Piece(..)
, OrdPiece(OrdPiece, ordPieceGet)
, Color(White, Black)
, Position
, Coord
, BoardPiece(..)
, Board(..)
, StepFailure(..)
, PieceMoves, PieceMoves2
, Move(..)
) where

type Position = (Char, Int)
type Coord = (Int, Int)

type ChessRet a = Either String a

data Piece = Pawn | Rook | Knight | Bishop | Queen | King | Empty
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

data BoardPiece = BoardPiece { getPiece :: Piece
                             , getColor :: Color
                             , getPosition :: Position
                             , getHaveMoved :: Bool
                             , getMoves :: Maybe ([Coord], [Coord])
                             --, getPossibleMoveBoards :: Maybe [Board]
                             } deriving (Show, Eq)

data Board = Board { getWhitePieces :: [BoardPiece]
                   , getBlackPieces :: [BoardPiece]
                   , getLastMove :: Maybe Move
                   , getNextPlayer :: Color
                   } deriving (Show)


--IsCheckMate has winner color
data StepFailure = IsStaleMate | IsCheckMate Color | IsInvalidMove String
                 | IsPieceNotFound String | IsOtherFailure String
                 | NeedPawnPromotion
                     deriving (Show)


type PieceMoves = (BoardPiece, [Coord], [Coord])
type PieceMoves2 = ([Coord], [Coord])


data Move = Move (Position, Position) | Castle Bool | EnPassant (BoardPiece, BoardPiece)
               deriving (Show, Eq)


{-
data Either3 a b c = E3Left a
                   | E3Middle b
                   | E3Right c
-}

