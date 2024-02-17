{-# LANGUAGE InstanceSigs #-}
module Data.Book (Book(..)) where

import Data.Time (LocalTime)

data Book = Book
  { isbn :: String
  , title :: String
  , authors :: String
  }

instance Show Book where
  show (Book isbn title authors) = "Book " <> title <> " (" <> isbn <> ") by " <> authors