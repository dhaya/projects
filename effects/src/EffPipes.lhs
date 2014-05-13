In the last post, we saw some inter thread coordination using forkIO and MVars.
We will try to solve the exercise we did in the last post using pipes. This doesn't
involve concurrency so we lose the concurrency feature of the forkIO solution but
we will slowly work our way to a solution involving concurrency with pipes.

> module EffPipes where
>

> import qualified Pipes as P
> import Pipes.Prelude as PP hiding (read, print)

We would like to model the two sources of input as two Producers in pipes.

> produceInt :: P.Producer Int IO ()
> produceInt = do
>   P.lift $ print "Int:"
>   str <- P.lift getLine
>   let i = read str
>   P.yield i
>   produceInt
>

The float producer is going to be similar.

> produceFloat :: P.Producer Float IO ()
> produceFloat = do
>   P.lift $ print "Float: "
>   str <- P.lift getLine
>   let f = read str
>   P.yield f
>   produceFloat

Now we just have to combine these two producers into a single producer. Since
a producer is a stream of values, we can think of combining two producers as
zipping two streams.

> combinedProducer :: P.Producer Int IO ()
>                     -> P.Producer Float IO ()
>                     -> P.Producer (Int, Float) IO ()
> combinedProducer ip fp = PP.zip ip fp

Thats it! Lets write a consumer that adds these two stream values.

> adder :: P.Consumer (Int,Float) IO ()
> adder = do
>   (i, f) <- P.await
>   P.lift $ print $ f + fromIntegral i
>   adder
>

Pull based composition of the producer and consumer is given by.

> eff :: P.Effect IO ()
> eff = (combinedProducer produceInt produceFloat) P.>-> adder
>

And finally run it!

> main :: IO ()
> main = do
>   P.runEffect eff
>

Later we will see how to get concurrency back. We will also change the effects
to something that is more convenient instead of the manual entering of int and
float values, we will either read them from files containing these values or
randomly generate them from the producers.
