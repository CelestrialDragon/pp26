-- Session 5, block 2: lists without end, in Haskell.       runghc Inf.hs
-- A cell of a list is built when it is read, and not before. So a list without
-- end is a value, and a program that reads a part of it ends.

nats :: [Integer]
nats = [0 ..]

-- The first number of the list is prime: we keep it, we take its multiples out
-- of the rest, and we do the same with what is left. take 10 primes reads ten cells.
primes :: [Integer]
primes = sieve [2 ..]
  where sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
  print (take 5 nats)
  print (nats !! 1000000)
  print (takeWhile (< 60) (map (^ 2) nats))
  print (take 10 primes)
  print (foldr (\x rest -> x > 2 || rest) False nats)
