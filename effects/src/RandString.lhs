How to generate a random string containing only ascii characters. This is
quite easy to do given that System.Random has a way to get any random type by
the Random typeclass. But I want to make it a bit interesting by asking
for random strings that only contain ascii letters.

Ascii has both lower case and upper case english characters. So to start
with lets model them as two different types.

> newtype LCAscii = LC Char deriving (Show)
> newtype UCAscii = UC Char deriving (Show)

When generating a random char, we will generate a random number and
Next we will define a typeclass that has a method which returns the code
for the character.

> class CodePoint a where
>   cp :: a -> Int

The code point for Char is given by the ord function.

> instance CodePoint LCAscii where
>   cp (LC c) = ord c

> instance CodePoint UCAscii where
>   cp (UC c) = ord c


We also need to define Bound instances for these two types.

> instance Bounded UCAscii where
>   minBound = UC 'A'
>   maxBound = UC 'Z'
> 
> instance Bounded LCAscii where
>   minBound = LC 'a'
>   maxBound = LC 'z'

Next we will define instances for the Random typeclass present in System.Random.
We just convert the character bounds to Ints using the CodePoint typeclass and 
and generate a random integer between these two bounds. Once we have an Int, we 
again get a character back using chr function.

> instance Random LCAscii where
>   randomR ival g = (LC . chr $ r, g')
>                    where leftBound = cp $ fst ival
>                          rightBound = cp $ snd ival
>                          (r :: Int, g') = randomR (leftBound, rightBound) g
>   random = randomBounded
> 
> instance Random UCAscii where
>   randomR ival g = (UC . chr $ r, g')
>                    where leftBound = cp $ fst ival
>                          rightBound = cp $ snd ival
>                          (r :: Int, g') = randomR (leftBound, rightBound) g
>   random = randomBounded
> 
> randomBounded :: (RandomGen g, Random a, Bounded a) => g -> (a, g)
> randomBounded = randomR (minBound, maxBound)

We will model Ascii by the sum type of both lower case and upper case ascii. 

> newtype Ascii = Ascii (Either LCAscii UCAscii) deriving (Show, CodePoint, Random)

Deriving CodePoint and Random typeclass for this needs GeneralizedNewtypeDeriving extension.
In addition to that we also need to define the instances for the sum type for this to work.

> instance (CodePoint a, CodePoint b) => CodePoint (Either a b) where
>   cp (Left x) = cp x
>   cp (Right b) = cp b

> instance (Random a, Random b, Bounded a, Bounded b) => Random (Either a b) where
>   random g = (e, g'')
>     where
>       (toss, g') = randomR (0 :: Int, 1) g
>       left = toss == 0
>       (e, g'') = if left then
>                    let (rand, newg) = randomR (minBound :: a, maxBound) g'
>                    in (Left rand :: Either a b, newg)
>                  else
>                    let (rand, newg) = randomR (minBound :: b, maxBound) g'
>                    in (Right rand :: Either a b, newg)
> 
>   randomR ival g = case (fst ival) of
>     Left x -> let (rand, newg) = randomR (minBound :: a, maxBound) g
>               in (Left rand :: Either a b, newg)
>     Right y -> let (rand, newg) = randomR (minBound :: b, maxBound) g
>                in (Right rand :: Either a b, newg)

The above code is kind of ugly and I am sure there is a better way to do this. But 
essentially for trhe random function we toss a coin and determine if we want to
generate a lower case random or an upper case random. For the randomR method that
takes an interval, we look at the minBound and decide if we need lower case or not.

This instance assumes that a and b types have the same sizes. Otherwise, the distribution
of randoms won't be preserved with a coin toss.

Now we have that, a few more functions to tie everything together.

> ascii :: Ascii -> Char
> ascii (Ascii (Left (LC c))) = c
> ascii (Ascii (Right (UC c))) = c
> 
> randomAscii :: IO Char
> randomAscii = ascii <$> (randomIO :: IO Ascii)

Finally this function takes an int and returns a random string of that size.

> randomString :: Int -> IO String
> randomString k = replicateM k randomAscii


