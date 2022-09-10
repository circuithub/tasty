module Timeouts where

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Concurrent
import Control.Exception (SomeException(SomeException), try)
import Utils

-- this is a dummy tree we use for testing
testTree :: TestTree
testTree =
  localOption (mkTimeout $ 2 * ds) $
  testGroup "timeout test"
    [ testCase "fast" $ threadDelay ds
    , testCase "slow" $ threadDelay (3 * ds)
    , testCase "infinite loop" $ do -- see #280
        let x :: Int; x = x
        [1, 2, 3, x] @?= [3, 2, 1]
    , testCase "catch async exceptions" $ do -- see #349
        res <- try (length [1..] @?= 0)
        case res of
          Left e@SomeException{} -> do
            print e
            length [1..] @?= 0
          Right _ -> fail "Should have timed out"
    ]
  where
    ds :: Integral a => a
    ds = 10^5

testTimeouts :: TestTree
testTimeouts = testCase "Timeouts" $ do
  launchTestTree mempty testTree $ \smap -> do
    [fast, slow, inf_loop, catchAsync] <- runSMap smap
    case fast of
      Result { resultOutcome = Success } -> return ()
      _ -> assertFailure $ "Fast test failed: " ++ resultDescription fast

    assertTimeoutFailure "slow" slow
    assertTimeoutFailure "infinite loop" inf_loop
    assertTimeoutFailure "catch async excepions" catchAsync

    return $ const $ return ()

assertTimeoutFailure :: TestName -> Result -> Assertion
assertTimeoutFailure name r =
  case r of
    Result { resultOutcome = Success } -> assertFailure $ "Test " ++ name ++ " passed"
    Result { resultOutcome = Failure (TestTimedOut _) } -> return ()
    _ -> assertFailure $ "Test " ++ name ++
      " failed for the wrong reason: " ++ resultDescription r
