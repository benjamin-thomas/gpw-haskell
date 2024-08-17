{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module SimulateHttp (main) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Relude

data Action
    = IncFoo
    | TriggerHttpRequest
    deriving (Show, Eq)

type App a = StateT AppState (Writer [Action]) a

data AppState = AppState
    { foo :: Int
    , bar :: Int
    , msg :: String
    }
    deriving (Show)

triggerHttp :: App ()
triggerHttp = lift $ tell [TriggerHttpRequest]

incFoo :: App ()
incFoo = do
    modify' $ \st -> st{foo = foo st + 1}
    lift $ tell [IncFoo]

program :: App ()
program = do
    incFoo
    incFoo
    st <- get
    when (foo st > 1) triggerHttp

interpretActions :: [Action] -> IO ()
interpretActions =
    mapM_
        ( \case
            TriggerHttpRequest -> putStrLn "HTTP request would actually happen here..."
            IncFoo -> putStrLn "Foo has been incremented"
        )

main :: IO ()
main = do
    putStrLn ">>> Launch tests <<<"
    testIncrementingOnceDoesNotTriggerAnHttpRequest
    testIncrementingTwiceTriggersAnHttpRequest

    putStrLn "\n\n>>> Launch the app <<<"
    putStrLn $ mconcat ["=========", yellow <> " BEG [Run app] " <> reset, "========="]
    let initialState = AppState 0 0 "Hello!"
        (_, actions) = runWriter (execStateT program initialState)
    interpretActions actions
    putStrLn $ mconcat ["=========", yellow <> " END [Run app] " <> reset, "========="]

{-

==== TESTING ====

 -}

green :: String
green = "\x1b[32m"

red :: String
red = "\x1b[31m"

yellow :: String
yellow = "\x1b[33m"

reset :: String
reset = "\x1b[0m"

testIncrementingOnceDoesNotTriggerAnHttpRequest :: IO ()
testIncrementingOnceDoesNotTriggerAnHttpRequest = do
    putStrLn $ mconcat ["=========", yellow <> " BEG [Run test] " <> reset, "========="]
    let initialState = AppState 0 0 "Hello!"
        (_, actions) = runWriter (execStateT program' initialState)
    mapM_ print actions
    let expectedActions =
            [ IncFoo
            ]
    if actions == expectedActions
        then putStrLn $ mconcat [green, "Test passed", reset]
        else putStrLn $ mconcat [red, "Test failed", reset]
    putStrLn $ mconcat ["=========", yellow <> " END [Run test] " <> reset, "========="]
  where
    program' :: App ()
    program' = do
        incFoo
        st <- get
        when (foo st > 1) triggerHttp

testIncrementingTwiceTriggersAnHttpRequest :: IO ()
testIncrementingTwiceTriggersAnHttpRequest = do
    putStrLn $ mconcat ["=========", yellow <> " BEG [Run test] " <> reset, "========="]
    let initialState = AppState 0 0 "Hello!"
        (_, actions) = runWriter (execStateT program' initialState)
    mapM_ print actions
    let expectedActions =
            [ IncFoo
            , IncFoo
            , TriggerHttpRequest
            ]
    if actions == expectedActions
        then putStrLn $ mconcat [green, "Test passed", reset]
        else putStrLn $ mconcat [red, "Test failed", reset]
    putStrLn $ mconcat ["=========", yellow <> " END [Run test] " <> reset, "========="]
  where
    program' :: App ()
    program' = do
        incFoo
        incFoo
        st <- get
        when (foo st > 1) triggerHttp
