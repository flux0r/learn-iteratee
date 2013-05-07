module Iteratee.Types where

------------------------------------------------------------------------------
import qualified Control.Applicative as A
import           Control.Exception (SomeException)

------------------------------------------------------------------------------
-- | Represent a message as an Exception.
type Msg = SomeException


------------------------------------------------------------------------------
-- | A stream is a sequence of elements broken into Chunks. It can either be
-- in the End state, which means the stream has no more data, or it can be in
-- the Chunk state, which means the stream is continuing.
data S e = End (Maybe Msg)
         | Chunk [e]
  deriving (Show)


------------------------------------------------------------------------------
-- | Type alias for a step function that can process new data and make a new
-- state. 
type Step e m a = S e -> m (I e m a, S e)


------------------------------------------------------------------------------
-- | An iteratee is a stream processor. It can be in the Done state or the
-- Cont state. The Cont state can have a message to the stream producer or an
-- error and it also has the step function that can process new data and make
-- a new state.
--
-- It's assumed that iteratees, if given bounded input, do bounded computation
-- and use bounded resources. It's also assumed that an iteratee which is
-- given a stream in the End state will move to the Done state.
--
-- The monad m can be the identity monad for pure computations.
data I e m a = Done !a
             | Cont !(Maybe Msg) (Step e m a)


------------------------------------------------------------------------------
-- | An iteratee is a functor.
(<$>) :: Functor m => (a -> b) -> I e m a -> I e m b
f <$> (Done x)          = Done (f x)
f <$> (Cont msg step)   = Cont msg (fmap next . step)
  where
    next (Done x, s)    = (Done (f x), s)
    next (i, s)         = (f <$> i, s)

instance Functor m => Functor (I e m) where
    fmap = (<$>)


------------------------------------------------------------------------------
-- | An iteratee is an applicative functor.
pureI :: a -> I e m a
pureI x = Done x

(<*>) :: Functor m => I e m (a -> b) -> I e m a -> I e m b
(Done f) <*> i          = f <$> i
(Cont msg step) <*> i   = Cont msg (fmap next . step)
  where
    next (Done f, s)    = (f <$> i, s)
    next (i', s)        = (i' <*> i, s)

instance Functor m => A.Applicative (I e m) where
    pure    = pureI
    (<*>)   = (<*>)


------------------------------------------------------------------------------
-- | An iteratee is a monad.

