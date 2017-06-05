module Utils
( newMatrix
, extractJust, isJust
, extractRight, isRight
, listSingletonExtract
) where

import qualified Data.Matrix as M
import Types

newMatrix width height elem = M.matrix width height (\_ -> elem)

--notFunc x =

extractJust (Just y) = y

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

listSingletonExtract :: [a] -> ChessRet a
listSingletonExtract lst
   | length lst == 1    = Right $ Prelude.head lst
   | otherwise          = Left "list Singleton extraction failed"



extractRight :: Either a b -> b
extractRight (Right x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False


