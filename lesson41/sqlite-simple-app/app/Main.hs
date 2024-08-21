{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified MyLib
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

printUsers :: IO ()
printUsers = mapM_ print =<< MyLib.getUsers

printTools :: IO ()
printTools = mapM_ print =<< MyLib.getTools

printAvailableTools :: IO ()
printAvailableTools = mapM_ print =<< MyLib.getAvailableTools

printUnavailableTools :: IO ()
printUnavailableTools = mapM_ print =<< MyLib.getUnavailableTools

addUser :: IO ()
addUser = do
  putStr "Enter a username: "
  username <- getLine
  MyLib.addUser username

checkout :: IO ()
checkout = do
  putStr "Enter a user ID: "
  userId <- read <$> getLine
  putStr "Enter a tool ID: "
  toolId <- read <$> getLine
  MyLib.checkout userId toolId

checkin :: IO ()
checkin = do
  putStr "Enter a tool ID: "
  toolId <- read <$> getLine
  MyLib.doCheckin toolId

performCmd :: String -> IO ()
performCmd = \case
  "users" -> printUsers >> main
  "tools" -> printTools >> main
  "add-user" -> addUser >> main
  "checkout" -> checkout >> main
  "checkin" -> checkin >> main
  "in" -> printAvailableTools >> main
  "out" -> printUnavailableTools >> main
  "quit" -> putStrLn "bye!"
  _ -> putStrLn "Sorry, command not found!" >> main

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a command: "
  performCmd =<< getLine
