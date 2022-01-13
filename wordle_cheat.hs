import Data.List (transpose, foldl')




data GuessChar = Wrong Char | Misplaced Char | Correct Char
type Guess = [GuessChar]

guesses :: [Guess]
guesses = [ map Wrong "tears"
          , map Wrong "mound"
          , [Wrong 'w', Wrong 'h', Misplaced 'i', Wrong 'f', Wrong 'f']
          , [Wrong 'c', Misplaced 'y', Wrong 'l', Misplaced 'i', Wrong 'x']
          ]

requiredLetters :: Guess -> [Char]
requiredLetters [] = []
requiredLetters ((Wrong x):xs) = requiredLetters xs
requiredLetters ((Misplaced x):xs) = x : requiredLetters xs
requiredLetters ((Correct x):xs) = x : requiredLetters xs

excludedLetters :: Guess -> [Char]
excludedLetters [] = []
excludedLetters ((Wrong x):xs) = x : excludedLetters xs
excludedLetters (_:xs) = excludedLetters xs

known :: [Guess] -> [Maybe Char]
known guesses = map (foldl' collect Nothing) $ transpose guesses
  where
    collect (Just x) _  = Just x
    collect _ (Correct x) = Just x
    collect x _ = x

misplaced  :: [Guess] -> [[Char]]
misplaced guesses = map (foldl' collect []) $ transpose guesses
  where
    collect xs (Misplaced x) = x:xs
    collect xs _  = xs


hasRequired :: String -> Bool
hasRequired str = all (`elem` str) $ concatMap requiredLetters guesses

noExcluded :: String -> Bool
noExcluded str = all (not . (`elem` str)) $ concatMap excludedLetters guesses

knownCorrect :: String -> Bool
knownCorrect str = all pass $ zip str (known guesses)
  where
    pass (x, Nothing) = True
    pass (x, Just y) = x == y

noneMisplaced :: String -> Bool
noneMisplaced str = all pass $ zip str (misplaced guesses)
  where
    pass (x, []) = True
    pass (x, ms) = all (/= x) ms

lengthFive :: String -> Bool
lengthFive = (==5) . length


valid :: String -> Bool
valid x = lengthFive x && hasRequired x && noExcluded x && knownCorrect x && noneMisplaced x


main = do
  candidates <- filter valid <$> lines <$> getContents
  mapM putStrLn candidates
  return ()



