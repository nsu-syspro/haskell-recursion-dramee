{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Task2 where

import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)
import Task1 (reverse, map, sum, doubleEveryOther)

splitLast :: [a] -> ([a], a)
splitLast []     = error "splitLast: пустой список"
splitLast [x]    = ([], x)
splitLast (x:xs) = let (initPart, lastElem) = splitLast xs
                   in (x : initPart, lastElem)


normalizeModN :: Int -> Int -> Int
normalizeModN n x = if x >= n then x - (n - 1) else x

doubleEveryOtherMod :: Int -> [Int] -> [Int]
doubleEveryOtherMod _ []     = []
doubleEveryOtherMod n [x]        = [normalizeModN n (x * 2)]
doubleEveryOtherMod n (x:y:rest) = normalizeModN n (x * 2) : y : doubleEveryOtherMod n rest

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n f xs =
  let digits    = map f xs
      revDigits = reverse digits
      processed = doubleEveryOtherMod n revDigits
      total     = sum processed
  in (n - (total `mod` n)) `mod` n


luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt


digitToInt :: Char -> Int
digitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')
  | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')
  | otherwise = error "digitToInt: некорректный символ"

toDigitsDec :: Integer -> [Int]
toDigitsDec n
  | n <= 0    = []
  | otherwise = toDigitsDec (n `div` 10) ++ [fromIntegral (n `mod` 10)]


validateDec :: Integer -> Bool
validateDec n =
  let ds = toDigitsDec n
  in case ds of
       [] -> False
       _  -> let (initDigits, checkDigit) = splitLast ds
             in luhnDec initDigits == checkDigit

validateHex :: [Char] -> Bool
validateHex s =
  case s of
    [] -> False
    _  -> let (initChars, checkChar) = splitLast s
          in luhnHex initChars == digitToInt checkChar