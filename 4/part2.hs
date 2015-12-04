import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Control.Parallel
import Control.Arrow

main = do
  let str = C.pack "yzbqklnj" :: C.ByteString
  let inputs = [(C.append str (C.pack (show x)), x) | x <- [1..100000000]]
  let hashes = map ((encode . hash) *** id) inputs
  let solutions = filter (\(x,_) -> C.take 6 x == C.pack "000000") hashes
  print $ head solutions