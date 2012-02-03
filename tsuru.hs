--
-- This is a partial implementation of Tsuru's code sample from
-- http://www.tsurucapital.com/en/code-sample.html . 
--

module Main (
    main
) where

import System.IO hiding (putStrLn, readFile)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.List
import Data.Bits
import Control.Monad
import Numeric
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    putStr "Opening file: "
    putStrLn file
    words <- processFile file

    -- Dump out the packets.
    mapM (\a -> do
                    putStrLn "\r\n\r\nPACKET:"
                    mapM (\b -> putStr (showHex b " ")) a) words

    putStrLn "\r\nDone."

-- This needs to be expanded.
type Packet = [Word8]

-- Read packets from a file.
processFile file = do
    words <- fileToWordList file
    return $ getPackets words


-- dropWhile but with two elements at a time.
-- Used to find the frst marker.
dropWhile2 :: (a -> a -> Bool) -> [a] -> [a]
dropWhile2 p (x:y:xs) =
    if p x y then dropWhile2 p xs
    else x:y:xs
dropWhile2 p _ = []

getPacket :: [Word8] -> Packet
getPacket l = l


-- Process a list of bytes and find the sequences that are packets.
-- Could be made lots faster by combining the drop and take calls.
getPackets ([]) = []
getPackets l =
    let item = dropWhile2 (\a b -> a /= 0x42 && b /= 0x36) l in
    let by = takeWhile ((/=) 0xFF) item in
    (getPacket by) : (getPackets $ drop 190 item)

fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ BS.unpack contents

-- Same as above but uses an acumulator. Will run through everything
-- before returning, so it'll be slower.
--getPackets :: [Word8] -> [Packet]
--getPackets l =
--    getPacketsAcc l []
--    where
--        getPacketsAcc ([]) acc = acc
--        getPacketsAcc l acc =
--            let item = dropWhile2 (\a b -> a /= 0x42 && b /= 0x36) l in
--            let by = takeWhile ((/=) 0xFF) item in
--            getPacketsAcc (drop 190 item) ((getPacket by) : acc)
