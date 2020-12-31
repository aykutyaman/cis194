module Main (main) where
import System.Environment
import System.Directory
import System.IO
import Data.List

-- todo add todo.txt "Find the magic sword of power"
-- todo view todo.text
-- todo remove todo.txt 2
-- todo bump todo.txt 2

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("view", view),
            ("remove", remove),
            ("bump", bump)
           ]

add :: [String] -> IO ()
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")
add _ = return ()

view :: [String] -> IO ()
view [filename] = do
  contents <- readFile filename
  let todos = lines contents
  let indexed = zipWith (\todo i -> show (i :: Integer) ++ " - " ++ todo) todos [0..]
  putStrLn $ "\n" ++ unlines indexed
view _ = return ()

remove :: [String] -> IO ()
remove [filename, n] = do
  handle <- openFile filename ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let num = read n
      todos = lines contents
      newTodos = delete (todos !! num) todos
  hPutStr tempHandle $ unlines newTodos
  hClose tempHandle
  hClose handle
  removeFile filename
  renameFile tempName filename
  return ()
remove _ = return ()

bump :: [String] -> IO ()
bump [filename, n] = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  (tempName, tempHandle) <- openTempFile "." "temp"
  let todos = lines contents
      num = read n
      item = todos !! num
      items = delete item todos
      newItems = item : items
  hPutStr tempHandle (unlines newItems)
  hClose tempHandle
  hClose handle
  removeFile filename
  renameFile tempName filename
  return ()
bump _ = return ()

errorExit :: IO ()
errorExit = do
  putStrLn "\nThere was an error"
  return ()

main :: IO ()
main = do
  (command:args) <- getArgs
  case lookup command dispatch of
    Just action -> action args
    Nothing -> errorExit
  
