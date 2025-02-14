{-# OPTIONS_GHC -Wall #-}
-- Note: the above pragma enables all warnings

module Task3 where

-----------------------
-- Helper type synonyms

type Peg = String
type Move = (Peg, Peg)

-----------------------

-- Usage examples
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 =
    case n of
        1 -> [(p1, p2)]
        m -> hanoi (m - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (m - 1) p3 p2 p1

