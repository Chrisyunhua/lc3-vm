module Main where

import LC3VM
import System.Environment
  ( getArgs,
    getProgName,
  )

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  if length args /= 1
    then putStrLn $ "usage:\n  " ++ progName ++ " <obj_file>"
    else do
      let imageFile = head args
      readVM imageFile >>= runVM
