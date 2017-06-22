
--new Types

data GameInstance = GameInstance { getGameBoard :: Board
                                 , getNextGameBoards :: Maybe [Board]
                                 , getGameMoves :: [Move]
                                 , getGamePieceMoves :: [PieceMoves]
                                 }

--new Utils

makeGameInstance board nextToPlay =
   let moves = getPossibleMoves board nextToPlay
       pieceMoves =

--EndGameGenerator

--local structure
data PieceCoord = PieceCoord Piece Coord Bool

--black moves and white moves
generateEndGames :: [BoardPiece] ->
generateEndGames wPieces bPieces =
   let board = mkBoard


