module Util where

import Data.Hashable as H
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.List as L
import qualified Data.Text as TXT
import System.IO.Unsafe
import System.Directory
import System.FilePath.Posix
import Debug.Trace

-- Create NodeID by hashing on ByteString
hashNodeID :: ByteString -> Int
hashNodeID str = H.hash str

-- Conversion functions
strToInt :: String -> Int
strToInt str = read str :: Int
intToByteString :: Int -> ByteString
intToByteString i = strToByteString (show i)
strToByteString :: String -> ByteString
strToByteString str = C.pack str
byteStringToStr :: ByteString -> String
byteStringToStr str = C.unpack str
emptyByteString :: ByteString
emptyByteString = BS.empty

-- Split string by a given char
splitString :: String -> Char -> [String]
splitString str chr =
  let res = splitStringHelper str chr
  in if res == [""] then [] else res
  where splitStringHelper [] _ = [""]
        splitStringHelper (c:cs) chr
          | c == chr = "" : rest
          | otherwise = (c : head rest) : tail rest
          where rest = splitStringHelper cs chr

-- Join a list of strings by some string
joinString :: String -> [String] -> String
joinString _ [] = ""
joinString str lst = (L.intercalate str lst)

-- Remove duplicates from a list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x : (removeDuplicates xs)

-- Replaces a substring in a string
strReplace :: String -> String -> String -> String
strReplace a b str =
  let at = TXT.pack a
      bt = TXT.pack b
      strt = TXT.pack str
  in TXT.unpack (TXT.replace at bt strt)

-- Replaces a string with an empty string
clearString :: String -> String
clearString str = strReplace str "" str

-- File IO (wrapper function for loading file)
-- loadFile :: String -> IO ByteString
-- loadFile fname = BS.readFile fname
loadFile :: String -> ByteString
loadFile fname = unsafePerformIO (BS.readFile fname)

-- File IO (given a directory name, remove its contents and ensure that it is empty)
removeDirectoryContents :: String -> ()
removeDirectoryContents dirname = (seq (unsafePerformIO (removeDirectoryRecursive dirname)) (ensureDirectoryExists dirname))

-- File IO (given a directory name, ensure that it exists)
ensureDirectoryExists :: String -> ()
ensureDirectoryExists dirname = unsafePerformIO (createDirectoryIfMissing True dirname)

-- File IO (given a file path, ensure that a path for it exists)
ensurePathExists :: String -> ()
ensurePathExists fname = unsafePerformIO (createDirectoryIfMissing True (takeDirectory fname))

-- File IO (wrapper function for writing a file, given a path
-- and contents)
dumpFile :: String -> ByteString -> ()
dumpFile fname fcontents = unsafePerformIO (BS.writeFile fname fcontents)

-- Check if file exists
fileExists :: String -> Bool
fileExists fname = unsafePerformIO (doesFileExist fname)

-- Wrapper trace function for debugging
debug_ :: String -> a -> a
debug_ = trace

-- Creates a separator string
createSep :: String
createSep = "---------------------------------\n"