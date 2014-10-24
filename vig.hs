--Austin Wegner
import Data.Char

--function to rotate characters with too large handling
rotate x | x > 122 = rotate (x - 26)
         | x <= 122 = x

--function to cycle through characters by amount specified in key, calling rotate when neccessary
encode k m | (97 <= (ord m)) && (ord m) <= 122 = chr (rotate (ord m + ord k))
           | ((ord m) < 97) || (122 < (ord m)) = m

--takes message and key provided by user and runs encode function
vigenere message key = map (\(a,b) -> encode a b) (zip (cycle key) message)

--main function that asks for user input and runs program
main = do
	putStrLn "Enter text: "
	message <- getLine
	putStrLn "Enter key: "
	key <- getLine
	putStrLn (vigenere message key)
