import qualified Data.Map as Map

data Pocket a = Pocket { color :: String
                       , value :: a
                       } deriving (Eq, Show)

instance Functor Pocket where
  fmap f Pocket { color = c, value = v} =
    Pocket { color = c, value = f v }

type Owner = String

newtype PocketsLedger a = PocketsLedger (Map.Map Owner (Pocket a))
                          deriving Show

instance Functor PocketsLedger where
  fmap f (PocketsLedger m) = PocketsLedger $ Map.map (fmap f) m

pockets = PocketsLedger $ Map.fromList
  [("Semyon", Pocket { color = "Brown", value = 110 })
  ,("Peter", Pocket { color = "Brown", value = 110 })
  ,("John", Pocket { color = "Brown", value = 110 })
  ,("Platon", Pocket { color = "Brown", value = 110 })
  ]

