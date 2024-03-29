module Controls.Database (load, save) where

import System.IO (Handle, IOMode (ReadMode), withFile)
import Control.Monad (replicateM, foldM)

import Data.Binary.Get as B (Get, runGet, getByteString, getWord16le, getWord64le)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.ByteString.Lazy as BL (unpack, toStrict)
import Data.ByteString.Lazy (hGet)

import Data.Book ( Book(Book, authors) )
import Data.Checkout ( Checkout(Checkout, isbn, createdOn) )
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (fold)


load :: String -> IO ([Book], [Checkout])
load filepath = withFile filepath ReadMode doLoad

save :: ([Book], [Checkout]) -> String -> IO ()
save (books, checkouts) pathname =
  Builder.writeFile pathname packed
  where 
    packed = Builder.word16LE 1
      <> Builder.word64LE (fromIntegral $ length books)
      <> Builder.word64LE (fromIntegral $ length checkouts)
      <> foldMap buildBook books
      <> foldMap buildCheckout checkouts

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

buildBook :: Book -> Builder.Builder
buildBook (Book isbn title authors) =
  fixedWidthBuilder isbn 18
  <> fixedWidthBuilder title 51
  <> fixedWidthBuilder authors 51
    

buildCheckout :: Checkout -> Builder.Builder
buildCheckout (Checkout isbn createdOn expiredOn) =
  fixedWidthBuilder isbn 18
  <> fixedWidthBuilder createdOn 8
  <> fixedWidthBuilder expiredOn 8

fixedWidthBuilder s width =
  let len = length s
  in Builder.string8 s <> fold (replicate (width - len) $ Builder.char8 '\0')