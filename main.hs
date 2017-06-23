
import Types
import Utils
import ChessUtils
import Helpers
import Logic
import Ai
import Step
import qualified Data.Char as C
import System.Environment
import System.Random
import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering, LineBuffering))
import System.Posix.Unistd (sleep)
import Debug.Trace


--step newGame Nothing White (extractRight $ strToMove "a2,a4") Nothing
defaultScanDepth = 4

--START NORMAL COMMAND LINE GAME

getPlayerMove = do
   putStr "enter move: "
   line <- getLine
   putStrLn ""
   return line

gameLoop board whosTurn botDepth = do
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

              gameLoop newBoard nextTurn botDepth
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
                     gameLoop board whosTurn botDepth
               IsPieceNotFound s ->
                  do putStrLn $ "Cannot find starting piece: " ++ s
                     gameLoop board whosTurn botDepth
               IsOtherFailure s ->
                  do putStrLn $ "Unknown error occured: " ++ s
                     gameLoop board whosTurn botDepth


gameDriver botDepth = do
   board <- return newGame
   print $ boardToMatrix board

   game <- gameLoop board White botDepth

   return ()

--END NORMAL COMMAND LINE GAME

--START BOT GAME

getPlayerMoveBot board whosTurn botDepth = do
   randomGen <- newStdGen
   (randomNum, newGen) <- return $ random randomGen

   return . fst . extractJust $ getAiMove board whosTurn botDepth (Just randomNum)

gameLoopBot _ _ 0 _ = do return ()
gameLoopBot board whosTurn n botDepth = do
   move <- getPlayerMoveBot board whosTurn botDepth

   let newBoard' = step board Nothing whosTurn move

   if isRight newBoard'
   then let newBoard = extractRight newBoard'
            nextTurn = flipColor whosTurn
        in do putStrLn $ moveToStr move
              gameLoopBot newBoard nextTurn (n-1) botDepth
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

gameDriverBot botDepth = do
   putStrLn "usermove=1"
   putStrLn "go"
   putStrLn "new"

   board <- return newGame
   game <- gameLoopBot board White 100 botDepth
   return ()

--END BOT GAME

--START PLAYER VS ENGINE

xboardMoveToMove moveStr =
   let (_, moveStr1) = splitAt 9 moveStr
       (fromStr, toStr) = splitAt 2 moveStr1
       moveStr3 = fromStr ++ "," ++ toStr
       move = strToMove moveStr3
   in move

pVeCheck board currColor botColor move botDepth = do
   let newBoard' = step board Nothing currColor move

   if isRight newBoard'
   then let newBoard = extractRight newBoard'
            nextTurn = flipColor currColor
        in do
            if currColor == botColor
            then putStrLn $ "move " ++ (moveToStr move)
            else return ()
            pVeHelper newBoard nextTurn botColor Nothing botDepth
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


pVeHelper board currColor botColor maybeMove botDepth = do

   if isJust maybeMove
   then let move = extractRight $ xboardMoveToMove (extractJust maybeMove)
        in pVeCheck board White Black move botDepth
   else return ()

   if currColor == botColor && (not $ isJust maybeMove)
   then do move <- getPlayerMoveBot board currColor botDepth
           putStrLn "#bot move"
           pVeCheck board currColor botColor move botDepth
   else do line <- getLine
           putStrLn $ "#got: " ++ line

           if substring "usermove" line && (not $ substring "accepted" line)
           then let opponentMove = extractRight $ xboardMoveToMove line
                in pVeCheck board currColor botColor opponentMove botDepth
           else pVeHelper board currColor botColor Nothing botDepth

personVsEngineLoop board color botColor hasStarted botDepth = do
   line <- getLine
   putStrLn $ "#got: " ++ line

   let newColor = if botColor == White then White else
                  if line == "white" then White else color

   if line == "go"
   then do putStrLn $ "#we have go! starting color: " ++ (show color)
                      ++ "\n#bot color:" ++ (show botColor)
           --personVsEngineLoop board color botColor True
           pVeHelper board color newColor Nothing botDepth
   else return ()

   if substring "usermove" line && (not $ substring "accepted" line)
   then do pVeHelper board color Black (Just line) botDepth
   else return ()


   --TODO: remove if statement
   --if hasStarted
   --then pVeHelper board color botColor
   --else personVsEngineLoop board color botColor False

   personVsEngineLoop board color newColor False botDepth

personVsEngineSetup botDepth = do
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
   then  personVsEngineLoop newGame White Black False botDepth
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
   else personVsEngineSetup botDepth

--END PLAYER VS ENGINE



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



---START cmd args

data CmdArgs = HumanVsHuman | EngineVsEngine | HumanVsEngine
             | Usage String | ScanDepth Int deriving (Show)

getScanDepthArg :: [CmdArgs] -> Maybe Int
getScanDepthArg (ScanDepth x : xs) = Just x
getScanDepthArg (_:xs) = getScanDepthArg xs
getScanDepthArg [] = Nothing

getGameModeArg :: [CmdArgs] -> Maybe CmdArgs
getGameModeArg (HumanVsHuman : xs) = Just HumanVsHuman
getGameModeArg (EngineVsEngine :xs) = Just EngineVsEngine
getGameModeArg (HumanVsEngine :xs) = Just HumanVsEngine
getGameModeArg (_:xs) = getGameModeArg xs
getGameModeArg [] = Nothing


usage = "[hve|hvh|eve] -d [depth] - \n"
         ++ "hve = human vs engine\n"
         ++ "hvh = human vs human\n"
         ++ "eve = engine vs engine"

parse ("-h":xs)   = Usage usage : parse xs
parse ("-d":x:xs) = (ScanDepth $ strToInt x) : parse xs
parse ("hvh":xs)  = HumanVsHuman : parse xs
parse ("hve":xs)  = HumanVsEngine : parse xs
parse ("eve":xs)  = EngineVsEngine : parse xs
parse _ = []

---END cmd args


---START main setup stuff

setBufferingCmd = do
   hSetBuffering stdout NoBuffering
   hSetBuffering stdin NoBuffering

setBufferingGui =
   hSetBuffering stdin LineBuffering


--main = test2
main = do
   args <- getArgs

   let parsedArgs = parse args
       gameModeMaybe = getGameModeArg parsedArgs
       scanDepthMaybe = getScanDepthArg parsedArgs
       scanDepth = tryExtractJust scanDepthMaybe defaultScanDepth

   if not $ isJust scanDepthMaybe
   then putStrLn $ "setting default scan depth " ++ (show defaultScanDepth)
   else putStrLn $ "using scan depth " ++ (show scanDepth)

   if not $ isJust gameModeMaybe
   then putStrLn $ "incorrect arguments: \n" ++ usage
   else let gameMode = extractJust gameModeMaybe
        in case gameMode of
            HumanVsEngine ->
               do setBufferingGui
                  personVsEngineSetup scanDepth
            HumanVsHuman ->
               do setBufferingCmd
                  gameDriver scanDepth
            EngineVsEngine ->
               do setBufferingCmd
                  gameDriverBot scanDepth

---END main setup stuff


