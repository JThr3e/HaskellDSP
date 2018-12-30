module Main where

import HaskDSP as SP
import Data.Bits
import Data.List.Split

main :: IO()
main = do
        --putStrLn ("Enter a real number sequence to convolve")
        --real1 <- readRealList
        --putStrLn ("Enter another real number sequence to convolve")
        --real2 <- readRealList
        --print (convolve real1 real2)
        putStrLn ("Input desired function: fft, ifft")
        fun <- getLine
        listNums <- readNumList
        let len = (length listNums)
        if (((.&.) len (len - 1)) == 0) --checks if list length is power of 2
                then    --print (listNums)
                if (fun == "fft") then print (SP.fft listNums)
                else if (fun == "ifft") then print (SP.ifft listNums)
                else putStrLn ("invalid function")
        else putStrLn ("List length must be a power of 2")



--Read in a string, turn into list, turn into tuples
--Tuples here represent complex values (Re, Im)
readNumList = do
        putStrLn ("Input a list of real floats delimited by \",\"")
        realFloat <- getLine
        putStrLn ("Input a list of imaginary floats delimited by \",\"")
        imagFloat <- getLine
        let realList = splitOn "," realFloat
        let imagList = splitOn "," imagFloat
        return (zip (map toFloat realList) (map toFloat imagList))
toFloat s = (read s :: Float)

readRealList = do
        putStrLn ("Input a list of real floats delimited by \",\"")
        realFloat <- getLine
        let realList = splitOn "," realFloat
        return  (map toRealTuple realList)
toRealTuple s = ((read s :: Float), 0)

