module Main (main) where
import System.Environment
import qualified Data.ByteString as B
import System.IO.Error

main :: IO ()
main = catchIOError toTry handler

toTry :: IO ()
toTry = do
  (fileName1:fileName2:_) <- getArgs
  copy fileName1 fileName2

copy :: FilePath -> FilePath -> IO ()
copy fileName1 fileName2 = do
  contents <- B.readFile fileName1
  B.writeFile fileName2 contents
  
handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "The file doesn't exist at:" ++ path
      Nothing -> putStrLn "The file doesn't exist at unknown loc.!"
  | otherwise = ioError e
