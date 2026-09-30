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
