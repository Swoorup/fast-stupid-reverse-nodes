{-# LANGUAGE NumDecimals #-}
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import Data.DList as D hiding (concat)
import Data.Function
import System.TimeIt
import Data.List.Split

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

moreFuncSwap :: Int -> [a] -> [a]
moreFuncSwap k z = 
  toList $ append nl nr
    where
      len = length z
      (l,r) = splitAt (len - mod len k) z
      nl = fromList $ concat . fmap reverse . chunksOf k $ l
      nr = fromList r

-- Thank you -- https://www.reddit.com/user/StephenSwat/ --
kGroupReverse :: Int -> [a] -> [a]
kGroupReverse n = concat . fmap (\l -> if length l == n then reverse l else l) . chunksOf n

pkGroupReverse :: (NFData a) => Int -> [a] -> [a]
pkGroupReverse n = 
  concat . pmap . chunksOf n
    where
      pmap ll = withStrategy (parBuffer 4 rdeepseq) (fmap (\l -> if length l == n then reverse l else l) ll)

-- u/djletch --
djletch :: Int -> [a] -> [a]
djletch n = concat . go n []
  where
    go _ [] [] = []
    go 0 acc xs = acc : go n [] xs
    go _ acc [] = [reverse acc]
    go i acc (x:xs) = go (i-1) (x:acc) xs

-- u/tripa --
tripa :: Int -> [a] -> [a]
tripa k xs = go xs k [] where
  go xs 0 acc = acc ++ go xs k []
  go (x:xs) i acc = go xs (i-1) (x:acc)
  go [] _ acc = reverse acc

tripa' :: Int -> [a] -> [a]
tripa' k xs = go xs k where
  go xs k = fst (fix (f xs k . snd))
  f    xs  0 acc = (acc,go xs k)
  f (x:xs) i acc = f xs (i-1) (x:acc)
  f   []   _ acc = (reverse acc,[])
                -- that "reverse" is the next optimize
                -- target should k get too big
                --
main :: IO ()
main = do
  let algorithm = 10
      k = 1e6
      p = 16
      l = [1..1e8]
      f = case algorithm of 
            1 -> swapNodes k l
            2 -> swapNodesNoTR k l
            3 -> pswapNodesNoTR p k l
            4 -> pswapNodes p k l
            5 -> moreFuncSwap k l
            6 -> kGroupReverse k l
            7 -> pkGroupReverse k l :: [Int]
            8 -> djletch k l :: [Int]
            9 -> tripa k l
            10 -> tripa' k l

  f & length & print & timeIt
