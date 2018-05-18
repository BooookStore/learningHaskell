import Data.Char
import Data.Maybe
import qualified Data.Map as Map

phoneBook =
  [("betty", "555-2938")
  ,("betty", "556-2938")
  ,("bonnie", "452-2928")
  ,("pasty", "493-2982")
  ,("licille", "205-2929")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")
  ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
     where add number1 number2 = number1 ++ ", " ++ number2
