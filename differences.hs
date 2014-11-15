module Main where


import Data.String
import Data.List
import System.Console.Haskeline

letterDiff (a,b) = if a == b then 1 else 0

wordDiff a b = foldl (\count letters -> count + letterDiff letters) 0 $ zip a b

getValues word words =
  [ wordDiff word curr | curr <- words ]


masterList =
  [ "anticipating"
  , "civilization"
  , "seclusionist"
  , "replenishing"
  , "apprehensive"
  , "quarterstaff"
  , "purification"
  , "broadcasting"
  , "inaccessible"
  , "appreciation"
  , "encountering"
  , "recuperating"
  , "conversation"
  ]


printAll words = mapM_ print $ fullList words

fullList words = [listRow i words | i <- [0..((length words)-1)]]

listRowPrintable index words =
  let word = (words !! index)
      values = getValues word words
  in
  word ++ " " ++ (show values) ++ " " ++ (show $ largestDups values)

listRow index words =
  let word = (words !! index)
      values = getValues word words
  in
   (index, word, values, largestDups values)

leastDups wAnalyses =
  foldl (\acc@(_,_,_,prevDups) curr@(_,_,_,dups) -> if dups < prevDups then curr else acc) (wAnalyses !! 0) wAnalyses

iterateWordList :: Int -> Int -> [String] -> [String]
iterateWordList index matches words =
  let (_,_,values,_) = listRow index words
  in
   reverse $ foldl (\acc (match_count, word) -> if match_count == matches then
                                        word:acc
                                      else
                                        acc) [] $ zip values words

largestDups lst =
  foldl (\acc item ->
          let currCount = foldl
                          (\total curr ->
                            if item == curr
                            then total + 1
                            else total) 0 lst
          in
           if currCount > acc
           then currCount
           else acc) 0 lst

oneStep words =
  let wAnalyses = fullList words
      firstTry@(index,word,_,_) = leastDups wAnalyses
  in
   do
     mapM_ print wAnalyses
     putStrLn " "
     putStrLn $ "Please choose: " ++ word
     putStr "Matches> "
     newIdx <- fmap read getLine
     return $ iterateWordList index newIdx words

iterateMatch words =
  if (length words) <= 1
  then return words
  else do
    newWords <- oneStep words
    iterateMatch newWords


buildList :: IO [String]
buildList = runInputT defaultSettings $ loop [] where
  loop :: [String] -> InputT IO [String]
  loop acc = do
    rawLine <- getInputLine "> "
    case rawLine of
      Nothing -> return acc
      Just line ->
        if length line <= 1
        then return acc
        else loop $ line:acc


buildList_alt :: [String] -> IO [String]
buildList_alt acc = do
  { putStr ">"
  ; line <- getLine
  ; if length line <= 1
    then return acc
    else buildList_alt $ line:acc
  }
                    
doit = do
  putStrLn "Please list all the words below, 'q' ends"
  words <- buildList
  iterateMatch words
  
main = doit
