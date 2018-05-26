import System.IO
import Control.Monad
import Data.Char

main = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        getLine
    putStrLn "The colors that you associate with 1,2,3 and 4 are : "
    mapM putStr colors

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

data Box a = Box a deriving (Show)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine