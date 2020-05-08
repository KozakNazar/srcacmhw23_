{------------------------------------------------------------------------------------------
- Lviv'2020 // ACM // Haskell functional programming(by quicksort implementation example) -
-    file: acmhw23.hs                                                                     -
-------------------------------------------------------------------------------------------}
import System.IO

input_data = [5, 7, 3, 4, 1, 9, 2, 8, 10, 6]

quicksort :: ( Ord a ) => [ a ] -> [ a ]
quicksort [] = []
quicksort ( x : xs ) =
    let lesserPart =  [ a | a <- xs , a <= x ]
        greaterPart = [ a | a <- xs , a > x ]
    in quicksort lesserPart ++ [ x ] ++ quicksort greaterPart

printElements :: [Int] -> IO()
printElements [] = return ()
printElements (x:xs) = do 
    putStr (show x ++ " ")
    printElements xs

main :: IO ()
main = do 
    let input = input_data 
    let output = quicksort input 
    putStr "input:\n"
    printElements input
    putStr "\n"       
    putStr "output:\n"
    printElements output
    putStr "\n" 