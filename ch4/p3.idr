module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
 where
  addToData : Vect old String -> Vect (S old) String
  addToData [] = [newitem]
  addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
      case integerToFin pos (size store) of
           Nothing => Just ("Out of range\n", store)
           Just id => Just (index id store_items ++ "\n", store)

searchEntries : (str : String) -> (store : DataStore) -> List (Integer, String)
searchEntries "" _ = []
searchEntries str store = let withIndexes = zipWith (\a,b => (toIntegerNat a,b)) [0.. size store] (toList (items store)) in
  filter (isInfixOf str . snd) withIndexes

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp =
  case parse inp of
       Nothing => Just ("Invalid command\n", store)
       Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
       Just (Get pos) => getEntry pos store
       Just (Search str) => let results = searchEntries str store in
                           Just (concatMap (\(ind, item) => show ind ++ ": " ++ item ++ "\n") results, store)
       Just Size => Just (show (size store) ++ "\n", store)
       Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
