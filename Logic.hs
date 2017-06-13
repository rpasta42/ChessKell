module Logic
( getPossibleMoves
, newGame
, genPossibleMoveBoards, genPossibleMoveBoards2
, getMatesAndStales
, isUnderCheck
, getPieceCaptures
, getPieceMoves, getPieceMoves' --' for testing
, isMoveOnBoard
) where

import Types
import Utils
import ChessUtils
import qualified Data.List as L
import qualified Data.Char as C
import System.IO
import System.Posix.Unistd (sleep)
import Data.Either.Combinators (mapLeft)
import Debug.Trace


newGame :: Board
newGame =
   let mkPawns row color = map (\x -> mkPiece color Pawn (x, row) False)
                               ['A'..'H']
       wPawns = mkPawns 2 White
       bPawns = mkPawns 7 Black

       otherPieces = [Rook .. King] ++ reverse [Rook .. Bishop]
       mkOtherPieces color y = map (\(piece, x)
                                       -> mkPiece color piece (x, y) False)
                                   $ zip otherPieces ['A'..'H']
       wOtherPieces = mkOtherPieces White 1
       bOtherPieces = mkOtherPieces Black 8

       --extraRook = [mkPiece Black Rook ('D', 4) False]
       extraRook = []

       board = mkBoard (wPawns++wOtherPieces) (bPawns++bOtherPieces ++ extraRook)
   in board
   --in extractRight $ (removePieceByPos' ('D', 2) board)
   --in extractRight $ (removePieceByPos' ('A', 2) board >>= removePieceByPos' ('B', 2))


getPossibleMoves :: Board -> Color -> [PieceMoves]
getPossibleMoves b White = listFilterLeft $ getPossibleMoves' b (getWhitePieces b) []
getPossibleMoves b Black = listFilterLeft $ getPossibleMoves' b (getBlackPieces b) []

getPossibleMoves' :: Board -> [BoardPiece] -> [ChessRet PieceMoves] -> [ChessRet PieceMoves]
getPossibleMoves' b [] acc = acc
getPossibleMoves' b bPieces@(x:xs) acc =
   let pieceMoves1 = getPieceMoves b x
       pieceMoves2 = pieceMoves1 --(\ (caps, moves) -> (x, caps, moves)) <$> pieceMoves1
   in getPossibleMoves' b xs (pieceMoves2 : acc)

--for genPossibleMoveBoards and getMatesAndStales
getMoveBoards' :: Board -> [PieceMoves] -> [[ChessRet Board]]
getMoveBoards' board moves =
   map (\pMoves@(bPiece, caps, moves)
         ->    (map (mvPiece board bPiece pMoves) caps)
            ++ (map (mvPiece board bPiece pMoves) moves))
       moves
      where mvPiece b bPiece pMoves coord = movePiece b bPiece pMoves $ coordToPos coord



genPossibleMoveBoards2 :: Board -> Color -> [(Move, Board)]
genPossibleMoveBoards2 board color =
   let allMoves1 = getPossibleMoves board color
       allMoves2 = trace ("\nmoves:" ++ (L.intercalate "\n" $ (map show) allMoves1))
                         $ allMoves1

       allMoves = allMoves1 --change this to allMoves2 for debug

       boards1 = getMoveBoards' board allMoves

       boards2 = zip boards1 allMoves

       boards3 = map (\ (boards, pMoves) -> zip (pieceMovesToMoves pMoves) boards)
                     boards2

       boards4 = map (\ (pos, eitherBoard) ->
                           if isRight eitherBoard
                           then Right $ (pos, extractRight eitherBoard)
                           else Left "empty board")
                     (concat boards3)

       boards5 = listFilterLeft boards4
   in boards5


genPossibleMoveBoards :: Board -> Color -> [Board]
genPossibleMoveBoards board color =
   let allMoves1 = getPossibleMoves board color
       allMoves2 = trace ("\nmoves:" ++ (L.intercalate "\n" $ (map show) allMoves1))
                         $ allMoves1
       allMoves = allMoves1 --change this to allMoves2 for debug

       boards1 = getMoveBoards' board allMoves
       boards2 = listFilterLeft $ concat boards1
   in boards2


--color -- next to move color to check for checkmate
--(Maybe winner, isStalemate)
getMatesAndStales :: Board -> Color -> (Maybe Color, Bool)
getMatesAndStales board color =
   let underCheckNow = eitherBoolExtract $ isUnderCheck color board --current board

       allMoves = getPossibleMoves board color

       boards1 = getMoveBoards' board allMoves
       boards2 = listFilterLeft $ concat boards1

       underCheckLst = map (getCheckLst color) boards2
       allUnderCheck = all id underCheckLst --next move

       --helpers
       getCheckLst color testBoard = extractRight $ isUnderCheck color testBoard

       isCheckMate1 = not underCheckNow && allUnderCheck

       isCheckMate2 =
         if underCheckNow
         then trace ((show color) ++ " is under check!")
                    $ isCheckMate1
         else isCheckMate1

       isCheckMate = isCheckMate2

   in if underCheckNow && allUnderCheck
      then (Just $ flipColor color, False)
      else if isCheckMate
           then (Nothing, True)
           else (Nothing, False)


isUnderCheck :: Color -> Board -> ChessRet Bool
isUnderCheck colorToCheck board@(Board { getWhitePieces=wPieces, getBlackPieces=bPieces }) =
   let wKing = getBoardPieceByPiece wPieces King
       bKing = getBoardPieceByPiece bPieces King

       wKingCoord = getPieceCoord <$> wKing
       bKingCoord = getPieceCoord <$> bKing

       wCaps = concat . listFilterLeft $ map (\x -> (\(a,b,c) -> b) <$> getPieceMoves board x) wPieces
       bCaps = concat . listFilterLeft $ map (\x -> (\(a,b,c) -> b) <$> getPieceMoves board x) bPieces

       wUnderCheck = elem' bCaps <$> wKingCoord
       bUnderCheck = elem' wCaps <$> bKingCoord

   in case colorToCheck of
         White -> wUnderCheck
         Black -> bUnderCheck


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
getPieceMoves :: Board -> BoardPiece -> ChessRet PieceMoves --([Coord], [Coord])
getPieceMoves board bPiece =
   let pPiece = getPiece bPiece
       pCoord = getPieceCoord bPiece
       pieceMoves1 = map (filter isMoveOnBoard) $ getPieceMoves' board bPiece
       pieceMoves2 = map (filter (not . coordEq pCoord)) pieceMoves1
       pieceMoves3 = pieceMoves2 --map (filter (not . putUnderCheck board)) pieceMoves2
       pieceMoves4 = pieceMoves3 --map (filter (not . moveOnOwnPiece board bPiece)) pieceMoves3
       pieceMoves5 = pieceMoves4 --map (filter (not . isIllegalJump board bPiece)) pieceMoves4
   in do capsAndMoves@(captures, moves) <- getPieceCaptures board pieceMoves5 bPiece
         return (bPiece, captures, moves) --capsAndMoves
         --if length captures == 0 && length moves == 0
         --then Left "no moves"
         --else return capsAndMoves


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
                  else ([newWhite2, newWhite1], [newBlack2, newBlack1])
          in if color == White
             then [newWhite]
             else [newBlack]

   in helper' piece $ posToCoord pos

isMoveOnBoard :: Coord -> Bool
isMoveOnBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8


{-old stuff
moveOnOwnPiece :: Board -> BoardPiece -> Coord -> Bool
moveOnOwnPiece board fromPiece to =
   let pColor = getColor fromPiece
       destPieceEither = getBoardPieceByCoord board to
   in if isRight destPieceEither
      then let destPiece = extractRight destPieceEither
               destPieceColor = getColor destPiece
           in destPieceColor == pColor
      else False

putUnderCheck board to = True

-}

--misc Utils/tools:

removePieceByPos' = flip removePieceByPos
elem' = flip L.elem


