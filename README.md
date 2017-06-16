

Chess engine written in Haskell.

There are 3 supported game modes:
- human vs human with computer evaluation for the best move (terminal-only)
- engine vs engine where the output of the game is piped to GNU xboard (GUI)
- human vs engine (GUI with xboard)

Originally minimax algorithm was used for picking moves, but now engine also supports Alpha-Beta pruning.

Chesskell is a fun side-project and is not meant to compete with other modern chess engines, therefore it has not been optimized for performance.

Performance is decent with the default scan depth of 4, but starts getting slow when using 5-move scan-depth.


=== TODO
- [x] castling (works correctly)
- [ ] en-passant
- [ ] pawn promotion
   - [x] always promotes to queen
   - [ ] promoting to other pieces is not yet implemented
- [ ] heuristics for better picking moves
- [ ] opening book

