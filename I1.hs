import           Control.Applicative
import           Control.Exception
import           Data.Maybe
import           Prelude hiding (drop, head, length, tail)
import qualified Prelude as P (drop, head, length, tail)
import           System.IO

inc, dec :: Int -> Int
inc x = x + 1
dec x = x - 1

fix :: (a -> a) -> a
fix f = f (fix f)

-- The stream.
data SG e = Empty
          | El e
          | End

-- The iteratee.
data IV e a = Done a (SG e)
            | Cont (SG e -> IV e a)

-- We need a way to feed data to an iteratee. It's called enum and it's kind
-- of a specialized foldl, in that it can signal when it's done since it
-- returns another iteratee.
enum :: IV e a -> [e] -> IV e a
enum i              []      = i
enum i@(Done _ _)   _       = i
enum (Cont f)       (x:xs)  = enum (f $ El x) xs

-- We need a way to get a final value out of an iteratee. It's called run.
run :: IV e a -> Maybe a
-- Handle the finite case, sort of like head or take.
run (Done x _)  = Just x
run (Cont f)    = iter $ f End
  where
    iter :: IV e a -> Maybe a
    -- Handle the running total case, like sum.
    iter (Done x _) = Just x
    -- Handle the divergent case.
    iter _          = Nothing

----------------------------------------------------------------------------
-- | LIST ANALOGS

head :: IV e (Maybe e)
-- Wrap the result into a new iteratee.
head = Cont iter
  where
    iter :: SG e -> IV e (Maybe e)
    -- Take the first element and then we're done.
    iter (El x) = Done (Just x) Empty
    -- If the stream is empty, return a continuation to the same function, since
    -- we're waiting for a new element or the end of the stream.
    iter Empty = Cont iter
    -- If we're at the end, we're done but we don't have an answer.
    iter End = Done Nothing End

-- Get the first element without removing it from the stream.
peek :: IV e (Maybe e)
peek = Cont iter
  where
    iter :: SG e -> IV e (Maybe e)
    iter x@(El y)   = Done (Just y) x
    iter Empty      = Cont iter
    iter End        = Done Nothing End

-- This one can be done before even getting a value.
drop :: Int -> IV e ()
drop 0  = Done () Empty
drop x  = Cont iter
  where
    iter :: SG e -> IV e ()
    iter (El _) = drop $ dec x
    iter Empty  = Cont iter
    iter End    = Done () End

-- This one keeps an accumulator.
length :: IV e Int
length = Cont (iter 0)
  where
    iter :: Int -> SG e -> IV e Int
    iter r (El _)   = Cont (iter $ inc r)
    iter r Empty    = Cont (iter r)
    iter r End      = Done r End

----------------------------------------------------------------------------
-- | The iteratee is a monad.
instance Functor (IV e) where
    fmap f (Done x s)   = Done (f x) s
    fmap f (Cont next)  = Cont (fmap f . next)

instance Applicative (IV e) where
    pure x      = Done x End
    (Done f s)  <*> i   = fmap f i
    (Cont next) <*> i   = Cont (\s -> next s <*> i)

instance Monad (IV e) where
    return              = pure
    (Done x s)  >>= f   = case f x of
        (Done y _)  -> Done y s
        (Cont next) -> next s
    (Cont next) >>= f   = Cont (\s -> next s >>= f)

----------------------------------------------------------------------------
-- | Examples using the functor, applicative functor, and monad instances.

tail :: IV e ()
tail = drop 1

-- Skip an element
drop1keep1 :: IV e (Maybe e)
drop1keep1 = tail >> head

-- Keep every other element.
osc :: IV a [a]
osc = fmap catMaybes . sequence . replicate 5 $ drop1keep1

----------------------------------------------------------------------------
-- | Monadic enumerator.

type EM e m a = IV e a -> m (IV e a)

enumHandle :: Handle -> EM Char IO a
enumHandle x i = iter i
  where
    iter i@(Done _ _)   = return i
    iter i@(Cont next)  = hIsEOF x >>= (\b ->
        if b
            then return i
            else hGetChar x >>= iter . next . El)

enumFile :: FilePath -> IV Char a -> IO (IV Char a)
enumFile x i = bracket (openFile x ReadMode) (hClose) (flip enumHandle i)

len2files :: FilePath -> FilePath -> IO (Maybe Int)
len2files x y = fmap run $ ((enumFile x) >=> (enumFile y)) length
