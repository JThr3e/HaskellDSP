import Data.List.Split
import Data.Bits

main = do
	putStrLn ("Input a list of integers delimited by \",\"")
	listNums <- readNumList
	let len = (length listNums)
	if (((.&.) len (len - 1)) == 0) --checks if list length is power of 2
		then print (fft listNums)
	else putStrLn ("List length must be a power of 2")

--Read in a string, turn into list, turn into tuples
--Tuples here represent complex values (Re, Im)
readNumList = do
	line <- getLine
	let strList = splitOn "," line
	return (map toRealTuple strList)
toRealTuple s = ((read s :: Float) , 0)

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

--Decimation in time Fast Fourier Transform
fft :: [(Float, Float)] -> [(Float, Float)]
fft [] = []
fft x = if (tail x) == [] then x else (addLists (duplicate(fft(evenLst x)))
	  (mulLists (pitchForkList (length x)) (duplicate(fft(oddLst x)))))

