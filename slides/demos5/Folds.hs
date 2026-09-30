-- Session 5, block 2: the two folds, in Haskell.       runghc Folds.hs
-- trace prints "visit" when a character is read, so the output shows what each
-- fold reads. Laziness delays what a function is given: with foldr our function
-- is given the rest, and at 3 it answers without reading it.
import Debug.Trace (trace)

text :: String
text = "hi\nyo"

seen :: Char -> Char
seen c = trace ("visit " ++ show c) c

main :: IO ()
main = do
  putStrLn "foldr:"
  print (foldr (\x r -> seen x == '\n' || r) False text)
  putStrLn "foldl:"
  print (foldl (\r x -> r || seen x == '\n') False text)
  -- foldl on [0 ..] does not return: it reaches the end of the list before it
  -- answers, and the list has no end. It is left out of this file.
  putStrLn "foldr on a list without end:"
  print (foldr (\x r -> x > 2 || r) False [0 ..])
  putStrLn "take 5 of a map of a filter on a list without end:"
  print (take 5 (map (\n -> n * n) (filter even [0 ..])))
  -- when is our function called? It says so. With foldr it is the call on top: it is
  -- called on 0, 1, 2, 3 and answers. With foldl the call on top is foldl again, and
  -- the calls of our function wait: on [0 .. 5] they run at the end, from the last
  -- element to the first; on [0 ..] the end never comes, and none of them runs.
  putStrLn "foldr on [0 ..], our function traced:"
  print (foldr fr False [0 ..])
  putStrLn "foldl on [0 .. 5], our function traced:"
  print (foldl fl False [0 .. 5])

fr :: Integer -> Bool -> Bool
fr x rest = trace ("  f called on " ++ show x) (x > 2 || rest)

fl :: Bool -> Integer -> Bool
fl found x = trace ("  f called on " ++ show x) (found || x > 2)
