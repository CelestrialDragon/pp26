-- Block 2: the list of Fibonacci numbers, in one line that refers to itself.
-- runghc demos/Fibs.hs
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  print (take 10 fibs)
  print (fibs !! 25)
