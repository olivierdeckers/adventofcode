import Crypto.Hash.MD5
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16
import Control.Parallel.Strategies
import Control.Arrow
import Data.List.Split

main = do
  let solution = head $ (filter f [1..] `using` parBuffer 4 rdeepseq)
  print solution
  where 
    str = C.pack "yzbqklnj" :: C.ByteString
    f :: Int -> Bool
    f x = prefix == (C.take 6 $ getHash x)
    getHash x = (encode . hash) $ C.append str (C.pack $ show x)
    prefix = C.pack "000000"