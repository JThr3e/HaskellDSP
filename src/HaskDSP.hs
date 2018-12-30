module HaskDSP
    ( fft,
      ifft,
      convolve
    ) where

--Duplicate the elements in a list and append to the end
duplicate :: [(Float, Float)] -> [(Float, Float)]
duplicate x = x ++ x

--Add each of the tuples in list together
addLists :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
addLists [][] = []
addLists (a:as) (b:bs) =  ((fst a)+(fst b) , (snd a)+(snd b)) : (addLists as bs)

--Complex Multiply each of the tuples in the list together
mulLists :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
mulLists [][] = []
mulLists (a:as) (b:bs) = (complexMul a b) : (mulLists as bs)
complexMul :: (Float,Float) -> (Float,Float) -> (Float,Float)
complexMul (a, b) (c, d) = ((a*c + (-1)*b*d), (b*c+a*d))

--Construct a list of exp((-2pi*k)/N) 0 <= k < N values
pitchForkList :: Int -> [(Float, Float)]
pitchForkList n = map (pitchFork n) [0..(fromIntegral (n-1)::Float)]
pitchFork npt = (\k -> (cos(((-1.0)*k*2*pi)/(fromIntegral npt::Float))
                      , sin(((-1)*k*2*pi)/(fromIntegral npt::Float))))

--Section out even and odd indicies into separate lists
evenLst [] = []
evenLst (x:xs) = x : (evenLst (if xs == [] then [] else tail xs))
oddLst [] = []
oddLst (x:xs) = evenLst xs

--Divide element by n
divN n = \(a,b) -> (a/n , b/n)

--Round element to the nth decimal
roundN n = \(a,b) -> ((fromInteger $ round $ a * (10^n)) / (10.0^^n),
                     (fromInteger $ round $ b * (10^n)) / (10.0^^n))

--Find next nearest power of 2
nextPow2 :: Int -> Int
nextPow2 x = 2^(ceiling(logBase 2.0 (fromIntegral x::Float)))

--Create a list of n zeros
listOfNZeros n = take n (repeat (0.0,0.0))

--Decimation in time Fast Fourier Transform
fft :: [(Float, Float)] -> [(Float, Float)]
fft [] = []
fft x = map (roundN 3)(if (tail x) == [] then x else (addLists (duplicate(fft(evenLst x)))
            (mulLists (pitchForkList (length x)) (duplicate(fft(oddLst x))))))

--Decimation in time Inverse Fast Fourier Transform
ifft :: [(Float, Float)] -> [(Float, Float)]
ifft [] = []
ifft (x:xs) = map (roundN 3)(map (divN (fromIntegral (length (x:xs))::Float))
                  (fft (x : (reverse xs))))

--Zeropad n number of zero pairs to list x
zeropad n x = x ++ (listOfNZeros n)

--Map but with 2 args
map2 :: (t -> t -> t) -> [t] -> [t] -> [t]
map2 x [] [] = []
map2 x (a:as) (b:bs) = x a b : map2 x as bs

--Convolution of 2 Real Signals in the frequency domain
convolve :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
convolve [][] = []
convolve x y = ifft (map2 complexMul
        (fft(zeropad (nextPow2((length x)+(length y)-1)-(length x)) x))
        (fft( zeropad (nextPow2((length x)+(length y)-1)-(length y)) y)))

