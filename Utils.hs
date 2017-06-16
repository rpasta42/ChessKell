module Utils
( deleteLstIndex, deleteLstItem
, replaceLstIndex
, newMatrix
, extractJust, isJust
, extractRight, isRight
, listSingletonExtract
, listFilterLeft
, pairToList
, eitherBoolExtract
, pairEitherToEitherPair
, matrixToDisplay, displayMatrix
, matrixMap, matrixMap'
, listParSeq2, listParSeq3, listParSeq4
, showListLines
, substring
) where

import qualified Data.Matrix as M
import qualified Data.List as L
import qualified Control.Parallel as P

import Types


--ghc main.hs +RTS -N3 -s -RTS -threaded; time ./main
--ghc main.hs +RTS -N3 -s -RTS -threaded -O2; time ./main +RTS -N3

listParSeq2 :: [x] -> [x]
listParSeq2 lst = helper lst []
   where helper [] acc = acc
         helper [x] acc = x:acc
         helper (x1:x2:xs) acc =
            helper xs $ P.par x1 (P.pseq x2 (x1:x2:acc))

listParSeq3 :: [x] -> [x]
listParSeq3 lst = helper lst []
   where helper [] acc = acc
         helper [x] acc = x:acc
         helper (x1:x2:x3:xs) acc =
            helper xs $ P.par x1 (P.par x2 (P.pseq x3 (x1:x2:x3:acc)))
         helper (x1:x2:xs) acc =
            helper xs $ P.par x1 (P.pseq x2 (x1:x2:acc))


listParSeq4 = 0

elemIndex' :: (Eq a) => a -> [a] -> Either String Int
elemIndex' item items =
   let i = item `L.elemIndex` items
   in if isJust i
      then Right $ extractJust i
      else Left "index not found"

deleteLstIndex :: [a] -> Int -> [a]
deleteLstIndex items index = reverse $ helper [] (zip [0..] items)
   where helper acc [] = acc
         helper acc ((i, x):xs) =
            let newAcc = if i == index then acc else x:acc
             in helper newAcc xs

deleteLstItem :: (Eq a) => [a] -> a -> Either String [a]
deleteLstItem items item =
   let i = item `elemIndex'` items
   in deleteLstIndex items <$> i


replaceLstIndex lst i newItem =
   let (part1, (_:part2)) = L.splitAt i lst
       newLst = part1 ++ [newItem] ++ part2
   in newLst

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

pairEitherToEitherPair :: (Either c a, Either c b) -> Either c (a,b)
pairEitherToEitherPair (Left x,  _      ) = Left x
pairEitherToEitherPair (_     ,  Left x ) = Left x
pairEitherToEitherPair (Right a, Right b) = Right $ (a, b)

listFilterLeft :: [Either a b] -> [b]
listFilterLeft lst = reverse $ helper' lst []
   where helper' [] acc = acc
         helper' ((Right x):xs) acc = helper' xs (x:acc)
         helper' (_:xs) acc = helper' xs acc

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a,b]

eitherBoolExtract :: Either a Bool -> Bool
eitherBoolExtract (Left _) = False
eitherBoolExtract (Right x) = x

newMatrix width height elem = M.matrix width height (\_ -> elem)

matrixChrToStr :: M.Matrix Char -> M.Matrix String
matrixChrToStr m =
   matrixMap m (\x _ _ _ -> [x]) " "

matrixStrToChr :: M.Matrix String -> M.Matrix Char
matrixStrToChr m =
   matrixMap m (\x _ _ _ -> head x) ' '

matrixToDisplay :: M.Matrix String -> String
matrixToDisplay m = L.intercalate "\n" . M.toLists . matrixStrToChr $ m

displayMatrix dispM = do
   putStr "\n\n"
   putStr dispM
   putStr "\n\n"



--matrix map

matrixMap :: M.Matrix a
          -> (a -> (Int, Int) -> M.Matrix a -> M.Matrix b -> b)
          -> b
          -> M.Matrix b
matrixMap m mapFunc defaultVal =
   matrixMap' m mapFunc 1 1
              (M.matrix (M.nrows m)
                        (M.ncols m)
                        (\(x,y) -> defaultVal))

matrixMap' :: M.Matrix a
           -> (a -> (Int, Int) -> M.Matrix a -> M.Matrix b -> b)
           -> Int -> Int
           -> M.Matrix b
           -> M.Matrix b
matrixMap' m f x y accM
   | x > M.ncols m = matrixMap' m f 1 (y+1) accM
   | y > M.nrows m = accM
   | otherwise     =
      let mappedElem = f (M.getElem y x m) (x, y) m accM
          newAccM = M.setElem mappedElem (y, x) accM
      in matrixMap' m f (x+1) y newAccM


--showListLines x = concat . map show $ x
showListLines x = L.intercalate "\n" . map show $ x


substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


