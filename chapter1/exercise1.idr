module Excercise1

import Data.Vect

%default total

pfx : Float
pfx = (5 + 4 + (2 - (3 - (6 + 4 / 5)))) / 3 * (6 - 2) * (2 - 7)

square : Num a => a -> a
square x = x * x

sumSquaresTwo : List Int -> Int
sumSquaresTwo (x :: y :: []) = (square x) + (square y)
sumSquaresTwo _ = ?fuck

sumSquaresTwoLarger : Int -> Int -> Int -> Int
sumSquaresTwoLarger a b c = sumSquaresTwo . take 2 . sort $ [a, b, c]

-- square root

-- TODO small and large numbers
goodEnough : Float -> Int -> Bool
goodEnough guess x = (abs ((square guess) - cast x)) < 0.001

average : Float -> Float -> Float
average x y = (x + y) / 2.0

improve : Float -> Int -> Float
improve guess x = average guess (cast x / guess)

sqrtIter : Float -> Int -> Float
sqrtIter guess x = if goodEnough guess x then guess
                   else sqrtIter (improve guess x) x
