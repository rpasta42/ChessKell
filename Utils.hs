module Utils
( newMatrix
, extractJust, isJust
, extractRight, isRight
, listSingletonExtract
, listFilterLeft
) where

import qualified Data.Matrix as M
import Types

newMatrix width height elem = M.matrix width height (\_ -> elem)

--notFunc x =

--extractJust/isJust/extractRight/isRight
extractJust :: Maybe a -> a
extractJust (Just y) = y

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

extractRight :: Either a b -> b
extractRight (Right x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False



listSingletonExtract :: [a] -> ChessRet a
listSingletonExtract lst
   | length lst == 1    = Right $ Prelude.head lst
   | otherwise          = Left "list Singleton extraction failed"

listHasAtLeast1 :: [a] -> ChessRet [a]
listHasAtLeast1 lst
   | length lst >= 1    = Right lst
   | otherwise          = Left "listHasAtLeast1Extract: less than 1 in the list"

listEitherToEitherList :: [Either a b] -> Either a [b]
listEitherToEitherList lst = helper' lst []
   where helper' [] acc = Right acc
         helper' ((Right x):xs) acc = helper' xs (x:acc)
         helper' ((Left x):xs) _ = Left x



listFilterLeft :: [Either a b] -> [b]
listFilterLeft lst = helper' lst []
   where helper' [] acc = acc
         helper' ((Right x):xs) acc = helper' xs (x:acc)
         helper' (_:xs) acc = helper' xs acc

