--cabal install matrix
--cabal install either

import Types
import Utils
import ChessUtils
import Logic
import Ai
import qualified Data.List as L
import qualified Data.Char as C
import System.Random
import System.IO
import System.Posix.Unistd (sleep)
import Data.Either.Combinators (mapLeft)
import Debug.Trace

--step newGame Nothing White (extractRight $ strToMove "a2,a4") Nothing

botDepth = 4

--START NORMAL COMMAND LINE GAME

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

              randomGen <- newStdGen
              (randomNum, newGen) <- return $ random randomGen

              let aiMove = getAiMove newBoard nextTurn botDepth (Just randomNum)

              aiEval <- putStrLn $ (show aiMove) ++ ": chosen computer move"

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

--END NORMAL COMMAND LINE GAME

--START BOT GAME

getPlayerMoveBot board whosTurn = do
   randomGen <- newStdGen
   (randomNum, newGen) <- return $ random randomGen

   return . fst . extractJust $ getAiMove board whosTurn botDepth (Just randomNum)

gameLoopBot _ _ 0 = do return ()
gameLoopBot board whosTurn n = do
   move <- getPlayerMoveBot board whosTurn

   let newBoard' = step board Nothing whosTurn move

   if isRight newBoard'
   then let newBoard = extractRight newBoard'
            nextTurn = flipColor whosTurn
        in do putStrLn $ moveToStr move
              gameLoopBot newBoard nextTurn (n-1)
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
                     return () --gameLoopBot board whosTurn
               IsPieceNotFound s ->
                  do putStrLn $ "Cannot find starting piece: " ++ s
                     return () --gameLoopBot board whosTurn
               IsOtherFailure s ->
                  do putStrLn $ "Unknown error occured: " ++ s
                     return () --gameLoopBot board whosTurn

gameDriverBot = do
   putStrLn "usermove=1"
   putStrLn "go"
   putStrLn "new"

   board <- return newGame
   game <- gameLoopBot board White 100
   return ()

--END BOT GAME


--https://www.gnu.org/software/xboard/engine-intf.html#13

--START PLAYER VS ENGINE

xboardMoveToMove moveStr =
   let (_, moveStr1) = splitAt 9 moveStr
       (fromStr, toStr) = splitAt 2 moveStr1
       moveStr3 = fromStr ++ "," ++ toStr
       move = strToMove moveStr3
   in move

pVeCheck board currColor botColor move = do
   let newBoard' = step board Nothing currColor move

   if isRight newBoard'
   then let newBoard = extractRight newBoard'
            nextTurn = flipColor currColor
        in do
            if currColor == botColor
            then putStrLn $ "move " ++ (moveToStr move)
            else return ()
            pVeHelper newBoard nextTurn botColor Nothing
   else let (Left moveError) = newBoard'
        in --do print moveError
           case moveError of
               IsStaleMate ->
                  do putStrLn "#StaleMate"
                     return ()
               IsCheckMate winner ->
                  do putStrLn $ "#CheckMate! " ++ (show winner) ++ " wins"
                     return ()
               IsInvalidMove s ->
                  do putStrLn $ "#Invalid move! (" ++ s ++ ")"
                     return () --gameLoopBot board whosTurn
               IsPieceNotFound s ->
                  do putStrLn $ "#Cannot find starting piece: " ++ s
                     return () --gameLoopBot board whosTurn
               IsOtherFailure s ->
                  do putStrLn $ "#Unknown error occured: " ++ s
                     return () --gameLoopBot board whosTurn


pVeHelper board currColor botColor maybeMove = do

   if isJust maybeMove
   then let move = extractRight $ xboardMoveToMove (extractJust maybeMove)
        in pVeCheck board White Black move
   else return ()

   if currColor == botColor && (not $ isJust maybeMove)
   then do move <- getPlayerMoveBot board currColor
           putStrLn "#bot move"
           pVeCheck board currColor botColor move
   else do line <- getLine
           putStrLn $ "#got: " ++ line

           if substring "usermove" line && (not $ substring "accepted" line)
           then let opponentMove = extractRight $ xboardMoveToMove line
                in pVeCheck board currColor botColor opponentMove
           else pVeHelper board currColor botColor Nothing

personVsEngineLoop board color botColor hasStarted = do
   line <- getLine
   putStrLn $ "#got: " ++ line

   let newColor = if botColor == White then White else
                  if line == "white" then White else color

   if line == "go"
   then do putStrLn $ "#we have go! starting color: " ++ (show color)
                      ++ "\n#bot color:" ++ (show botColor)
           --personVsEngineLoop board color botColor True
           pVeHelper board color newColor Nothing
   else return ()

   if substring "usermove" line && (not $ substring "accepted" line)
   then do pVeHelper board color Black (Just line)
   else return ()


   --TODO: remove if statement
   --if hasStarted
   --then pVeHelper board color botColor
   --else personVsEngineLoop board color botColor False

   personVsEngineLoop board color newColor False

personVsEngineSetup = do
   line <- getLine
   putStrLn $ "#got: " ++ line

   if substring "protover" line
      then putStrLn "feature sigint=0 sigterm=0 time=0 usermove=1 colors=0 done=1"
      else return ()

   {-if line == "book"
   then do putStrLn "sdfd" --System.IO.hFlush System.IO.stdout
   else return ()-}


   let board = newGame
       haveProtoVer = substring "protover" line

   if haveProtoVer
   then  personVsEngineLoop newGame White Black False
   else return ()

   {-if line == "go"
   then do putStrLn "move a2a4"
           --System.IO.hFlush System.IO.stdout
   else return () --putStrLn " "

   if line == "c7c5"
   then do putStrLn "move b2b3"
   else return ()-}

   if line == "quit" -- || substring "result" line
   then return ()
   else personVsEngineSetup


--END PLAYER VS ENGINE


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
x5 = getPieceMoves' x1 <$> x3

x6 = getBoardPieceByPos x1 ('C', 2)
x7 = x6 >>= getPieceMoves x1

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
   --default for person vs person or bot vs bot
   --System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering --System.IO.NoBuffering


   --bot vs player
   System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
   System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering

   --gameDriverBot
   personVsEngineSetup
   --gameDriver



