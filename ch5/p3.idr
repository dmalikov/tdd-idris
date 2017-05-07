import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  if (x == "")
     then pure []
     else do xs <- readToBlank
             pure (x :: xs)

readVectToEOF : File -> IO (Either FileError (n ** Vect n String))
readVectToEOF f = do
  eof <- fEOF f
  if eof
     then pure (Right (_ ** []))
     else do Right line <- fGetLine f
               | Left err => pure (Left err)
             Right (_ ** lines) <- readVectToEOF f
               | Left err => pure (Left err)
             pure (Right (_ ** line :: lines))

readAndSave : IO ()
readAndSave = do
  putStrLn "Enter file content (blank line to end):"
  lines <- readToBlank
  putStr "Enter filename: "
  filename <- getLine
  Right _ <- writeFile filename (unlines lines)
    | Left e => putStrLn ("Error: " ++ show e)
  pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right f <- openFile filename Read
    | Left err => do printLn err
                     pure (_ ** [])
  Right v <- readVectToEOF f
    | Left err => do printLn err
                     pure (_ ** [])
  pure v
