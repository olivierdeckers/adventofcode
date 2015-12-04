import Crypto.Hash.MD5
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16

main = do
  let str = C.pack "yzbqklnj" :: C.ByteString
  print $ findSolution str 1

findSolution :: C.ByteString -> Int -> Int
findSolution secret start 
  | (C.take 5 $ computeHash secret start) == prefix = start
  | True = findSolution secret (start+1)
  where
    prefix = C.pack "00000"

computeHash :: C.ByteString -> Int -> C.ByteString
computeHash secret start =
  encode $ hash $ C.append secret (C.pack $ show start) :: C.ByteString