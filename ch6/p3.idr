module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

total
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

total
addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
 where
  addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
  addToData [] = [newitem]
  addToData (x :: xs) = x :: addToData xs

total
display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
      case integerToFin pos (size store) of
           Nothing => Just ("Out of range\n", store)
           Just id => Just (display (index id (items store)) ++ "\n", store)

getAllEntries : (store : DataStore) -> Maybe (String, DataStore)
getAllEntries store = Just (entries, store)
  where
    entries = foldl (\a, i => a ++ display i ++ "\n") "" (items store)

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Maybe Integer -> Command schema
  Quit : Command schema

total
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
      = case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input
  = case span isDigit input of
         ("", rest) => Nothing
         (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input
  = case span isLower input of
         (chars, rest) => case unpack chars of
                              [char] => Just (char, ltrim rest)
                              _ => Nothing
         _ => Nothing
parsePrefix (schemal .+. schemar) input
  = case parsePrefix schemal input of
         Nothing => Nothing
         Just (l_val, input') =>
            case parsePrefix schemar input' of
                 Nothing => Nothing
                 Just (r_val, input'') => Just ((l_val, r_val), input'')

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
    = case xs of
           [] => Just SString
           _ => do xss <- parseSchema xs
                   Just (SString .+. xss)
parseSchema ("Int" :: xs)
    = case xs of
           [] => Just SInt
           _ => do xss <- parseSchema xs
                   Just (SInt .+. xss)
parseSchema ("Char" :: xs)
    = case xs of
           [] => Just SChar
           _ => do xss <- parseSchema xs
                   Just (SChar .+. xss)
parseSchema _ = Nothing

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  _ => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest
    = case parseBySchema schema rest of
           Nothing => Nothing
           Just restok => Just (Add restok)
parseCommand schema "get" rest
    = case unpack rest of
           [] => Just (Get Nothing)
           args => case all isDigit args of
                        False => Nothing
                        True => Just (Get (Just (cast rest)))
parseCommand schema "schema" rest
    = do schemaok <- parseSchema (words rest)
         Just (SetSchema schemaok)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              _ => Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                       (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
       Nothing => Just ("Invalid command\n", store)
       Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
       Just (Get pos) => case pos of
                              Just p => getEntry p store
                              Nothing => getAllEntries store
       Just (SetSchema schema') => case setSchema store schema' of
                                        Nothing => Just ("Can't update schema\n", store)
                                        Just store' => Just ("OK\n", store')
       Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
