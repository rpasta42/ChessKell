--cabal install matrix
--cabal install either


import Types
import Utils
import ChessUtils
import Logic
import Ai
import qualified Data.List as L
import qualified Data.Char as C
import System.IO
import System.Posix.Unistd (sleep)
import Data.Either.Combinators (mapLeft)
import Debug.Trace

--step newGame Nothing White (extractRight $ strToMove "a2,a4") Nothing

getPlayerMove = do
   putStr "enter move: "
   line <- getLine
   putStrLn ""
   return line

gameLoop board whosTurn = do
   moveStr <- getPlayerMove

   let move = extractRight . strToMove $ moveStr
       newBoard' = step board Nothing whosTurn move

   if isRight newBoard'
   then let newBoard = extractRight newBoard'
            nextTurn = flipColor whosTurn
        in do putStrLn "board after move:"
              print $ displayBoardByColor newBoard whosTurn
              putStrLn $ "\n\n================" ++ (show nextTurn) ++ "'s Turn\n"
              print $ displayBoardByColor newBoard nextTurn
              gameLoop newBoard nextTurn
   else let (Left moveError) = newBoard'
        in --do print moveError
           case moveError of
               IsStaleMate ->
                  do putStrLn "StaleMate"
                     return ()
               IsCheckMate winner ->
                  do putStrLn $ "CheckMate! " ++ (show winner) ++ " wins"
                     return ()
               IsInvalidMove s ->
                  do putStrLn $ "Invalid move! (" ++ s ++ ")"
                     gameLoop board whosTurn
               IsPieceNotFound s ->
                  do putStrLn $ "Cannot find starting piece: " ++ s
                     gameLoop board whosTurn
               IsOtherFailure s ->
                  do putStrLn $ "Unknown error occured: " ++ s
                     gameLoop board whosTurn


gameDriver = do
   board <- return newGame
   print $ boardToMatrix board

   game <- gameLoop board White

   return ()


--"a1,b2"
strToMove :: String -> ChessRet Move
strToMove s =
   let splitted@(part1, part2) = splitAt 2 s
       (fromStr, toStr) = (part1, tail part2)
   in Right $ Move (strToPos fromStr, strToPos toStr)
      where
         strToPos s =
            let (x, y) = splitAt 1 s
                pos = (C.toUpper $ head x, C.digitToInt $ head y)
            in pos


--IsCheckMate has winner color
data StepFailure = IsStaleMate | IsCheckMate Color | IsInvalidMove String
                 | IsPieceNotFound String | IsOtherFailure String
                 | NeedPawnPromotion
                     deriving (Show)

--current player color = p1, other player = p2
--current board: currMove, board after move: nextMove
step :: Board -> Maybe Piece -> Color -> Move -> Either StepFailure Board
step board pawnPromo color (Move (from, to)) =
   do let nextColor = flipColor color
          toCoord = posToCoord to

      piece <- mapLeft (\errStr -> IsPieceNotFound $ "empty square selected: " ++ errStr)
                       (getBoardPieceByPos board from)

      let pColor' = getColor piece
      pColor <- if pColor' /= color
                then Left $ IsInvalidMove "wrong color piece"
                else Right pColor'

      pieceMoves1 <- mapLeft (\errStr -> IsOtherFailure $ "getPieceMoves failed: " ++ errStr)
                             $ getPieceMoves board piece

      let pieceMoves = L.concat . pairToList . (\(a, b, c) -> (b,c)) $ pieceMoves1

      newBoard <- mapLeft (\errStr -> IsOtherFailure $ "movePiece failed: " ++ errStr)
                          $ movePiece' board to pieceMoves1 piece

      --move next to up?
      checkCurrMoveP1 <- mapLeft (\errStr -> IsOtherFailure $ "isUnderCheckPrevMove failed: " ++ errStr)
                                 (isUnderCheck color board)

      checkCurrMoveP2 <- mapLeft (\errStr -> IsOtherFailure $ "isUnderCheckOtherColor1 failed: " ++ errStr)
                                 $ isUnderCheck color board

      checkNextMoveP1 <- mapLeft (\errStr -> IsOtherFailure $ "isUnderCheckNextMove failed: " ++ errStr)
                                 $ isUnderCheck color newBoard

      checkNextMoveP2 <- mapLeft (\errStr -> IsOtherFailure $ "isUnderCheckOtherColor2 failed: " ++ errStr)
                                 $ isUnderCheck nextColor newBoard

      let  (maybeWinner, isStaleMate) = getMatesAndStales newBoard nextColor

      case (maybeWinner, isStaleMate, checkNextMoveP1, checkCurrMoveP1) of
            (Just c, _, _, _)    -> Left $ IsCheckMate c
            (_, True, _, _)      -> Left IsStaleMate
            (_, _, True, False)  -> Left $ IsInvalidMove "cannot put yourself under check!"
            (_, _, True, True)   -> Left $ IsInvalidMove "move away from check!"

            _ -> Right newBoard


--misc Utils/tools:

removePieceByPos' = flip removePieceByPos
elem' :: (Eq a, Foldable t) => t a -> a -> Bool
elem' = flip L.elem


---Tests

--test1:
x1 = newGame
x2 = boardToMatrix x1

x3 = getBoardPieceByPos x1 ('C', 1)
x4 = x3 >>= getPieceMoves x1

--x5 = mkMove x1 ('C', 1) ('D', 2)
--x6 = fmap boardToMatrix x5
--(Right x7) = x6

--fmap (map coordToPos) x5


--test2:
{-
test2 =
   let board = newGame
       wPieces = getWhitePieces board
       bPieces = getBlackPieces board

       wMoves = map (getPieceMoves board) wPieces
       bMoves = map (getPieceMoves board) bPieces

       zipped1' = zip (map Right wPieces) wMoves
       zipped1 = zip (map Right bPieces) bMoves
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
          in if isRight bMatrix -- && p /= Pawn && p /= Knight -- && p == Rook -- && p == Bishop
             then do --displayMatrix . matrixToDisplay . extractRight $ bMatrix
                     print $ extractRight bMatrix
                     --sleep 1
                     helper xs
             else helper xs

   in  helper pieceMoveCombos3

-}

--main = test2
main = do
   System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering --System.IO.NoBuffering
   gameDriver



