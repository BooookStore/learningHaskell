main = do
    let x = "A"
    return ()

reverseWords :: String -> String
reverseWords = unwords . map reverse . words