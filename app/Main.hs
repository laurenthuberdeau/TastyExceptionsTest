module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import System.Exit (exitSuccess)

delay :: IO ()
delay = interruptibleDelay

main :: IO ()
main = do
  -- runTestsBracket
  -- runTestsTastyBracket
  runTestsWithResource
  where
    runTestsBracket = defaultMain $ testGroup "Tests"
      [ testCase "3+3=6" $
          withKeyBracket "Key1" $ \key -> do
            putStrLn $ "Key: " <> key
            delay
            3+3 @?= 6
      ]

    runTestsTastyBracket = defaultMainWithExceptionHandling $ testGroup "Tests"
      [ testCase "3+3=6" $
          withKeyTastyBracket "Key1" $ \key1 -> do
            withKeyTastyBracket "Key2" $ \key2 -> do
              putStrLn $ "Key1: " <> key1
              putStrLn $ "Key2: " <> key2
              delay
              3+3 @?= 6
      ]

    runTestsWithResource = defaultMain $ testGroup "Tests"
      [ withKeyWithResource "Key1" $ \createKey1 -> do
          withKeyWithResource "Key2" $ \createKey2 -> do
            testCase "3+3=6" $ do
              key2 <- createKey2
              key1 <- createKey1
              putStrLn $ "Key: " <> key1
              putStrLn $ "Key: " <> key2
              delay
              3+3 @?= 6
      ]

withKeyBracket :: String -> (String -> IO ()) -> IO ()
withKeyBracket name action = do
  let acquire = do
        putStrLn $ "withKeyBracket: Starting acquire " <> name
        delay
        putStrLn $ "withKeyBracket: Finished acquire " <> name
        pure "KEY"

      release _ = do
        putStrLn $ "withKeyBracket: Starting release " <> name
        delay
        putStrLn $ "withKeyBracket: Finished release " <> name

  bracket
    acquire
    release
    action

withKeyTastyBracket :: String -> (String -> IO ()) -> IO ()
withKeyTastyBracket name action = do
  let acquire = do
        putStrLn $ "withKeyTastyBracket: Starting acquire " <> name
        delay
        putStrLn $ "withKeyTastyBracket: Finished acquire " <> name
        pure "KEY"

      release _ = do
        putStrLn $ "withKeyTastyBracket: Starting release " <> name
        delay
        putStrLn $ "withKeyTastyBracket: Finished release " <> name

  tastyBracket
    acquire
    release
    action

withKeyWithResource :: String -> (IO String -> TestTree) -> TestTree
withKeyWithResource name action = do
  let acquire = do
        putStrLn $ "withKeyWithResource: Starting acquire " <> name
        delay
        putStrLn $ "withKeyWithResource: Finished acquire " <> name
        pure "KEY"

      release _ = do
        putStrLn $ "withKeyWithResource: Starting release " <> name
        delay
        putStrLn $ "withKeyWithResource: Finished release " <> name

  withResource
    acquire
    release
    action

interruptibleDelay :: IO ()
interruptibleDelay = threadDelay (3 * 10^6)

nonInterruptibleDelay :: IO ()
nonInterruptibleDelay = fib 36 >>= print
  where
    fib :: Int -> IO Int
    fib 0 = pure 1
    fib 1 = pure 1
    fib n = do
      x <- fib (n-1)
      y <- fib (n-2)
      pure (x+y)

resourcesAllocatedCount :: TVar Int
resourcesAllocatedCount = unsafePerformIO (newTVarIO 0)

tastyBracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
tastyBracket acquire release =
  bracket
    ((do
      a <- acquire
      modifyTVarIO succ resourcesAllocatedCount
      pure a
    ) `onException` (putStrLn "bracket acquire function had an exception. Cleanup may fail." >> modifyTVarIO pred resourcesAllocatedCount))
    (\a -> do
      void $ release a
      modifyTVarIO pred resourcesAllocatedCount
    )
  where
    modifyTVarIO f tvar = atomically (modifyTVar tvar f)

-- Using `bracket` in Tasty doesn't work when an Exception is thrown (ie. CTRL+C).
-- This is because Tasty runs the test in separate threads and has no way to know
-- when the release functions of bracket finish, and exits before they have
-- time to finish. The solution to that problem is to use Tasty's withResource
-- function, but this requires many changes to the tests and chains us to Tasty.
--
-- This custom "defaultMain" function is a way to go around this issue.
-- It runs Tasty in its own thread and keeps track of resources allocated with
-- `tastyBracket`. When an exception happens, it waits until all `tastyBracket`
-- release action finish.
defaultMainWithExceptionHandling :: TestTree -> IO ()
defaultMainWithExceptionHandling tests = do
  tastyThread <- Async.async (defaultMain tests)
  Async.wait tastyThread `onException` do
    resourcesRemaining <- readTVarIO resourcesAllocatedCount
    waitForResources tastyThread
  where
    waitForResources tastyThread = do
      putStrLn "\nCleaning up the resources."
      putStrLn "Press CTRL+C to exit immediately."
      Async.cancel tastyThread
      -- Waits until all the resources are released
      forever $ do
        resourcesRemaining <- readTVarIO resourcesAllocatedCount
        print resourcesRemaining
        when (resourcesRemaining <= 0) $ do
          putStrLn "Done!"
          exitSuccess
        threadDelay $ (1 * 1000 * 1000)
