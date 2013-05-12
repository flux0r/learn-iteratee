module Iteratee.Types where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Exception
import Data.Monoid


------------------------------------------------------------------------------
-- | Represent a message as an Exception.
type Msg = SomeException


------------------------------------------------------------------------------
-- | The input is a sequence of elements broken into chunks. It can either be
-- in the End state, which means the stream has no more data, or it can be in
-- the More state, which means the stream has more data.
data In e = End (Maybe Msg) | More [e]
  deriving (Show)

instance Eq i => Eq (In i) where
    (End _)     == (End _)  = True
    (More x)    == (More y) = x == y
    _           == _        = False

instance (Eq i, Monoid i) => Monoid (In i) where
    mempty                          = More mempty  
    x@(End _)   `mappend` _         = x
    x           `mappend` y@(End _) = if x == mempty
                                          then y
                                          else x
    (More x)    `mappend` (More y)  = More (x `mappend` y)

instance Functor In where
    fmap _ (End xs)     = End xs
    fmap f (More xs)    = More $ fmap f xs


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
data I e m a = Done a
             | Cont (Maybe Msg) (In e -> m (I e m a, In e))

fmapI :: Functor m => (a -> b) -> I e m a -> I e m b
fmapI f (Done xs)       = Done $ f xs
fmapI f (Cont msg go)   = Cont msg $ fmap after . go
  where
    after (Done xs, s)  = (Done (f xs), s)
    after (i, s)        = (fmapI f i, s)

pureI :: a -> I e m a
pureI = Done

appI (Done f)       i     = fmap f i
appI (Cont msg go)  i   = Cont msg (fmap after . go)
  where
    after (Done f, s)   = (fmap f i, s)
    after (i', s)       = (i' <*> i, s) 

bindI :: (Applicative m, Monad m) => I e m a -> (a -> I e m b) -> I e m b
bindI (Done xs)     f   = f xs
bindI (Cont msg go) f   = Cont msg (\s -> go s >>= after)
  where
    after (Done xs, s') = case f xs of
            Cont Nothing go'    -> go' s'
            i                   -> pure (i, s')

instance Functor m => Functor (I e m) where
    fmap = fmapI

instance Functor m => Applicative (I e m) where
    pure    = pureI
    (<*>)   = appI

instance (Applicative m , Monad m) => Monad (I e m) where
    return  = pureI
    (>>=)   = bindI


------------------------------------------------------------------------------
