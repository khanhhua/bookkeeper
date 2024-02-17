module Controls.Database (load) where

import System.IO (Handle, IOMode (ReadMode), withFile)
import Control.Monad (replicateM)

import Data.Binary.Get as B (Get, runGet, getByteString, getWord16le, getWord64le)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.ByteString.Lazy as BL (unpack, toStrict)
import Data.ByteString.Lazy (hGet)

import Data.Book ( Book(Book) )
import Data.Checkout ( Checkout(Checkout) )

load :: String -> IO ([Book], [Checkout])
load filepath = withFile filepath ReadMode doLoad

doLoad :: Handle -> IO ([Book], [Checkout])
doLoad handle = do
  bytes <- hGet handle 1024
  return $ B.runGet (do
    version <- fromIntegral <$> B.getWord16le
    if version == 1
      then getStorage
      else do fail $ "Invalid version: " <> show version
    ) bytes

getStorage :: Get ([Book], [Checkout])
getStorage = do
  bookCount <- fromIntegral <$> getWord64le
  checkoutCount <- fromIntegral <$> getWord64le
  books <- replicateM bookCount getBook
  checkouts <- replicateM checkoutCount getCheckout
  return (books, checkouts)

getCheckout :: Get Checkout
getCheckout =
  Checkout <$> getString 18 <*> getString 8 <*> getString 8

getBook :: Get Book
getBook =
  Book <$> getString 18 <*> getString 51 <*> getString 51

getString :: Int -> Get String
getString n = Char8.unpack <$> getByteString n
