import qualified Data.Map as Map

data LockerState = Taken | Free
                   deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

{- Search for locker & check its availability -}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup n m =
  case Map.lookup n m of
    Nothing -> Left $ "Locker #" ++ show n ++ " not found!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker #" ++ show n ++ " has already been taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "ZD39I"))
  ,(101, (Free, "AF342"))
  ,(103, (Free, "LS4TF"))
  ]

