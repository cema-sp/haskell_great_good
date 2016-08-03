import Data.List (words, unwords)
import Text.Regex.Posix

data Contact = Contact { firstName :: String
                       , lastName :: String
                       , phone :: String
                       }

instance Show Contact where
  show c = firstName c ++ " " ++
           lastName c ++ " (" ++
           phone c ++ ")"

instance Read Contact where
  readsPrec _ input =
    [(Contact { firstName = fn, lastName = ln, phone = p }, unwords rest)]
    where
      (fn : ln : rest1) = words input
      (_, _, _, p : rest) =
        unwords rest1 =~ "\\((.*)\\)(.*)" :: (String, String, String, [String])

instance Eq Contact where
  c1 == c2 = (firstName c1 == firstName c2) &&
             (lastName c1 == lastName c2) &&
             (phone c1 == phone c2)

