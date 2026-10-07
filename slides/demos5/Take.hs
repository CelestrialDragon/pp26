-- Block 2: the list of all the natural numbers, and take. The same two definitions are in
-- demos/take.ml, line for line.   runghc demos/Take.hs
import Prelude hiding (take)

from :: Integer -> [Integer]
from n = n : from (n + 1)

take :: Integer -> [a] -> [a]
take n l =
  if n == 0 then []
  else case l of
    [] -> []
    x : rest -> x : take (n - 1) rest

main :: IO ()
main = do
  print (take 5 [10, 20, 30, 40, 50, 60, 70])
  print (take 5 (from 0))
  print (take 5 [0 ..])
