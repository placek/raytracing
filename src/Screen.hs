module Screen where

-- | Screen. Screen resolution information.
data Screen = Screen Int Int deriving (Eq)

instance Show Screen where
  show (Screen w h) = show w ++ " " ++ show h
