module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Controls.Database (load, save)
import Data.Book 
import Data.Checkout
import Data.Foldable (traverse_)
import Data.Time

data Application = Application [Book] [Checkout]

main :: IO ()
main = do
  [filepath] <- getArgs
  (books, checkouts) <- load filepath
  app filepath (Application books checkouts)

app :: String -> Application -> IO ()
app filepath application@(Application books checkouts) = do
  choice <- menu

  case choice of
    0  -> return ()
    2 -> do
      checkout <- checkoutBook
      app filepath $ Application books (checkout : checkouts)
    91 -> do 
      book <- collectBook
      app filepath $ Application (book : books) checkouts
    99 -> do
      save (books, checkouts) filepath
      app filepath application
    100 -> do
      report application
      app filepath application

    otherwise -> app filepath application
  
menu :: IO Int
menu = do
  putStrLn $
    "+========================+\n" <>
    ":         MENU           :\n" <>
    "+------------------------+\n" <>
    ": 1. Search for book     :\n" <>
    ": 2. Checkout a book     :\n" <>
    ": 3. Return a lease      :\n" <>
    ":91. Add new book        :\n" <>
    ":99. Save                :\n" <>
    "100. Report              :\n" <>
    ": 0. Quit                :\n" <>
    "+========================+\n"
  hFlush stdout
  read <$> getLine


collectBook :: IO Book
collectBook = do
  title <- flushPutStr "Title: " >> getLine  
  isbn <- flushPutStr "ISBN: " >> getLine
  authors <- flushPutStr "Authors: " >> getLine

  return $ Book isbn title authors

checkoutBook :: IO Checkout
checkoutBook = do
  isbn <- flushPutStr "Enter ISBN: " >> getLine
  today <- utctDay <$> getCurrentTime
  let
    createdOn = formatYMD . toGregorian $ today
    expiredOn = formatYMD . toGregorian $ addDays 14 today
  return $ Checkout isbn createdOn expiredOn

  where
    pad m
      | m < 10 = "0" <> show m
      | otherwise = show m

    formatYMD (y, m, d) = 
      show y <> pad m <> pad d

report :: Application -> IO ()
report (Application books checkouts) = do
  putStrLn "BOOKS"
  traverse_ print books
  putStrLn "CHECKOUTS"
  traverse_ print checkouts

flushPutStr s = putStr s >> hFlush stdout
