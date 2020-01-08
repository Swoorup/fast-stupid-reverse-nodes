{-# LANGUAGE NumDecimals #-}
import Control.Parallel
import Control.Monad
import Data.DList as D
import Data.Function
import System.TimeIt

swapNodesNoTR :: Int -> [a] -> [a]
swapNodesNoTR _ [] = []
swapNodesNoTR k x = 
  let 
    (a,b)     = splitAt k x 
    left = fromList $ if (length a < k) then a else reverse a 
    right = fromList $ swapNodes k b 
   in 
     toList $ D.append left right 

pswapNodesNoTR :: Int -> Int -> [a] -> [a]
pswapNodesNoTR 1 k l = swapNodesNoTR k l
pswapNodesNoTR p k [] = []
pswapNodesNoTR p k l = 
  let 
    (a,b)     = splitAt k l 
    left = fromList $ if (length a < k) then a else reverse a 
    right = pswapNodesNoTR (p-1) k b & fromList
   in 
     left `par` (right `par` (toList $ D.append left right))

swapNodes :: Int -> [a] -> [a]
swapNodes k z = swapNodes' z empty & toList
  where 
    swapNodes' :: [a] -> DList a -> DList a
    swapNodes' [] acc = acc
    swapNodes'  x acc = 
      let 
        (a,b)     = splitAt k x 
        left = fromList $ if (length a < k) then a else reverse a 
       in 
         swapNodes' b (append acc left)

pswapNodes :: Int -> Int -> [a] -> [a]
pswapNodes 1 k z = swapNodes k z
pswapNodes p k z = pswapNodes' p z empty & toList
  where 
    pswapNodes' :: Int -> [a] -> DList a -> DList a
    pswapNodes' p [] acc = acc
    pswapNodes' p x acc = 
      let 
        (a,b)     = splitAt k x 
        left = fromList $ if (length a < k) then a else reverse a 
        acc' = append acc left
       in 
         b `par` (acc `par` (pswapNodes' (p-1) b acc'))

main :: IO ()
main = do
  let algorithm = 3
      k = 1e6
      p = 128
      l = [1..1e8]
      f = case algorithm of 
            1 -> swapNodes k l
            2 -> swapNodesNoTR k l
            3 -> pswapNodesNoTR p k l
            4 -> pswapNodes p k l

  f & length & print & timeIt
