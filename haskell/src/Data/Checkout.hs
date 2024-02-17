module Data.Checkout (Checkout(..)) where

import Data.Time (LocalTime)

data Checkout = Checkout
  { isbn :: String
  , createdOn :: String -- TODO: LocalTime
  , expiredOn :: String -- TODO: LocalTime
  }

instance Show Checkout where
  show (Checkout isbn _ expiredOn) = "Checkout " <> isbn <> " expires on " <> expiredOn