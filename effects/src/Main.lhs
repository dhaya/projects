In this series of posts, we will consider how to layer effects in a non trivial
application. I will try to start by solving smaller problems and make things
complicated as I go along.

> {-# LANGUAGE ScopedTypeVariables #-}
> module Main where
> import Control.Concurrent
> import Control.Monad

First, lets write a function that repeatedly does some IO action. Everytime
it does it will read some value and the new value read will be override previous
values. Here we are essentially trying to simulate applications that need to
continuosly refresh some reference data that it needs to use.

> type Value1 = IO Int
> type Value2 = IO Float

Say the value we are interested in getting two values (an Int and a float). The main
program will just periodically print out the sum of these two values.

> getValue1 :: Value1
> getValue1 = print "Enter Int Val: " >> getLine >>= return . read
>
> getValue2 :: Value2
> getValue2 = print "Enter Float Val: " >> getLine >>= return . read
>
> input1 :: MVar Int -> IO ()
> input1 m1 = do
>   v <- getValue1
>   putMVar m1 v
>
> input2 :: MVar Float -> IO ()
> input2 m2 = do
>   v <- getValue2
>   putMVar m2 v
>

We will now fork these two input actions in two separate threads. The main thread
should just consume from the two mvars to do its addition.

>
> main :: IO ()
> main = do
>   m1 <- newEmptyMVar
>   m2 <- newEmptyMVar
>   forkIO $ do
>     replicateM_ 10 $ input1 m1
>   forkIO $ do
>     replicateM_ 10 $ input2 m2
>   replicateM_ 10 $ add m1 m2
>

Now the consuming function just adds the values taken
out of the mvars as shown.

> add :: MVar Int -> MVar Float -> IO ()
> add m1 m2 = do
>   v1 <- takeMVar m1
>   v2 <- takeMVar m2
>   print $ (fromIntegral v1 :: Float) + v2

This is still a bit clumpsy since we hardcode the 10 in the main function as well.
We want to have the input processes be independent from the consumers and the
consumer should function as long as there are new inputs available. In the future we
will explore if there are alternative approaches to the same problem that are more
elegant than this approach.
