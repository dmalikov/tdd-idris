longer : String -> String -> String
longer x y = if length x > length y
                then x
                else y

printLonger' : IO ()
printLonger' = do
  putStr "First string: "
  firstString <- getLine
  putStr "Second string: "
  secondString <- getLine
  putStrLn (show (length (longer firstString secondString)))

printLonger : IO ()
printLonger =
  putStr "First string: " >>= \_ =>
  getLine >>= \firstString =>
  putStr "Second string: " >>= \_ =>
  getLine >>= \secondString =>
  putStrLn (show (length (longer firstString secondString)))
