module Step
( step
) where

import Types
import Helpers
import Logic
import ChessUtils
import Utils
import qualified Data.List as L (concat)
import Data.Either.Combinators (mapLeft)

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


