
--new Types

data GameInstance = GameInstance { getGameBoard :: Board
                                 , getNextGameBoards :: Maybe [Board]
                                 , getGameMoves :: [Move]
                                 , getGamePieceMoves :: [PieceMoves]
                                 , getNextPlayer :: Color
                                 }

--new Utils

makeGameInstance = undefined

--EndGameGenerator

--local structure
data PieceCoord = PieceCoord Piece Coord Bool

--black moves and white moves
generateEndGames :: [BoardPiece] ->
generateEndGames wPieces bPieces =
   let board = mkBoard



