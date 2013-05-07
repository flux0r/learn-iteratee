{-# LANGUAGE RankNTypes #-}

import           Control.Exception (ErrorCall(ErrorCall), SomeException,
                                    toException)
import           Data.Monoid (Monoid(mappend, mempty))
import           Prelude hiding (break, drop, head, span)
import qualified Prelude (break, drop, span)

----------------------------------------------------------------------------
type Message = SomeException

----------------------------------------------------------------------------
-- | The Carrier is the stream or pipe or whatever that brings in data in
-- chunks, each of which is called a package. It can signal that no more
-- data is coming with Void and that more data might come but there isn't
-- any available right now with Package [].
data Carrier e = Package [e]
               | Empty (Maybe Message)
  deriving
    (Show)

emptyCarrier :: Carrier e
emptyCarrier = Package []

----------------------------------------------------------------------------
-- | The Iteratee is a consumer or stream processor. It does stuff with the
-- Packages on the Carrier. It signals it is finished with Shut and that it
-- is waiting for more with Open. In short, the Iteratee knows what to do
-- with the next element.
-- 
-- The step function consumes part of the carrier stream, computes the new
-- state, and then returns the unconsumed part of the carrier stream.
--
-- The Iteratee looks like a state and continuation monad with the ability
-- to backtrack. The Open state is like an operating system call to consume
-- another chunk of data.
data Iteratee e a = Open (Maybe Message)
                         (Carrier e -> (Iteratee e a, Carrier e))
                  | Shut a

instance Monad (Iteratee e) where
    return              = Shut
    Shut a >>= f        = f a
    Open msg next >>= f = Open msg (docase . next)
      where
        docase (Shut x, c') = case f x of
            Open Nothing next'  -> next' c'
            i                   -> (i, c')
        docase (i, c')      = (i >>= f, c')

-- Throw an unrecoverable error.
throwError :: Message -> Iteratee e a
throwError x = let next = (\s -> (throwError x, s)) in
    Open (Just x) next

-- Throw a recoverable error.
throwRecoverableError ::Message
                      -> (Carrier e -> (Iteratee e a, Carrier e))
                      -> Iteratee e a
throwRecoverableError msg next = Open (Just msg) next

-- Send Empty to the iteratee and ignore the unconsumed part of the carrier.
run :: Iteratee e a -> a
run (Shut x)            = x
run (Open Nothing next) = check . fst $ next (Empty Nothing)
  where
    check :: Iteratee e a -> a
    check (Shut x)      = x
    check (Open m _)    = error $ "possibly divergent, control message: " ++ show m
run (Open (Just m) _)   = error $ "control message: " ++ show m

-- Create the Empty message to send to throwError. If the Carrier is already
-- Empty, just send the original message.
setEmpty :: Carrier e -> Message
setEmpty (Empty (Just x))    = x
setEmpty _                  = emptyException

emptyException :: Message
emptyException = toException $ ErrorCall "Empty"

divergenceException :: Message
divergenceException = toException $ ErrorCall "divergent iteratee"

----------------------------------------------------------------------------
-- | Primitive iteratees.
continue :: (Carrier e -> (Iteratee e a, Carrier e)) -> Iteratee e a
continue = Open Nothing

continueM :: (Carrier e -> (Iteratee e a, Carrier e))
          -> (Iteratee e a, Carrier e)
continueM s = (continue s, emptyCarrier)

-- Like List.span. Return the longest prefix, which might be empty, in which
-- all the elements satisfy a given predicate.
span :: (e -> Bool) -> Iteratee e [e]
span f = continue (step [])
  where
    step r (Package []) = continueM (step r)
    step r (Package xs) =
        case Prelude.span f xs of
            (_, [])     -> continueM (step $ r ++ xs)
            (r', tail)  -> (Shut (r ++ r'), Package tail)
    step r s            = (Shut r, s)

-- Like List.break. Return the longest prefix, which might be empty, in
-- which none of the elements satisfy a given predicate.
break :: (e -> Bool) -> Iteratee e [e]
break f = span (not . f)

-- Get the next element of the stream without removing it from the stream.
peek :: Iteratee e (Maybe e)
peek = continue step
  where
    step (Package [])       = (peek, emptyCarrier)
    step s@(Package (x:_))  = (Shut (Just x), s)
    step s                  = (Shut Nothing, s)

-- Return the next element of the carrier. If it is Empty, raise a
-- recoverable error.
head :: Iteratee e e
head = continue step
  where
    step :: Carrier e -> (Iteratee e e, Carrier e)
    step (Package [])       = (head, emptyCarrier)
    step (Package (x:xs))   = (Shut x, Package xs)
    step s                  = (Open (Just $ setEmpty s) step, s)

-- Take a sequence of elements and a stream and return the count the count
-- of how many elements of the sequence matched elements of the stream.
-- Remove the matched elements from the stream.
heads :: Eq e => [e] -> Iteratee e Int
heads s = iter 0 s
  where
    iter :: Eq e => Int -> [e] -> Iteratee e Int
    iter x []       = return x
    iter x ys       = continue (next x ys)

    next :: Eq e => Int -> [e] -> Carrier e -> (Iteratee e Int, Carrier e)
    next i xs       (Package [])            = (iter i xs, emptyCarrier)
    next i (x:xs)   s@(Package (x':xs'))    =
        if x == x'
            then next (succ i) xs (Package xs')
            else (Shut i, s)
    next i _        s                       = (Shut i, s)

skipToEmpty :: Iteratee e ()
skipToEmpty = continue step
  where
    step :: Carrier e -> (Iteratee e (), Carrier e)
    step (Package _)    = (skipToEmpty, emptyCarrier)
    step s              = (Shut (), s)

-- Like List.drop. Skip n elements of the carrier, if there are that many.
drop :: Int -> Iteratee e ()
drop 0 = return ()
drop n = continue step
  where
    step :: Carrier e -> (Iteratee e (), Carrier e)
    step (Package xs) = let l = length xs in
        if l <= n
            then (drop $ n - l, emptyCarrier)
            else (Shut (), Package $ Prelude.drop n xs)

isEmpty :: Iteratee e (Maybe Message)
isEmpty = continue next
  where
    next :: Carrier e -> (Iteratee e (Maybe Message), Carrier e)
    next (Package [])   = (isEmpty, emptyCarrier)
    next s@(Empty xs)   = (Shut (Just $ setEmpty s), s)
    next s              = (Shut Nothing, s)

----------------------------------------------------------------------------
-- | The Enumerator applies the Iteratee to the Carrier until the Carrier
-- signals Void or the Iteratee signals Shut. After resource cleanup, it
-- returns the final value of the Iteratee. It is an iteratee transformer.
-- It knows how to get to the next element.
-- type Enumerator e m a = Iteratee e m a -> m (Iteratee e m a)

----------------------------------------------------------------------------
-- | The Enumeratee is a generic stream decoder. It is an Enumerator with a
-- base monad of Iteratee i m.
-- type Enumeratee i o m a = Iteratee o m a -> Iteratee i m (Iteratee o m a)

----------------------------------------------------------------------------
-- type R e m = Iteratee e m

-- A general stream producer. Enumerator is a special case of L where mi and
-- mo are the same.
-- newtype L e mi mo = L {
--     unL :: forall a. R e mi a -> mo (R e mi a)
-- }

-- instance (Monad mi, Monad mo) => Monoid (L e mi mo) where
--     mempty  = L return
--     mappend (L x) (L y) = L $ \i -> (x i) >>= y
