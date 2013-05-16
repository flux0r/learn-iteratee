module I.In where


------------------------------------------------------------------------------
import Control.Exception (SomeException)
import Data.Function (fix)


------------------------------------------------------------------------------
type Msg = SomeException


------------------------------------------------------------------------------
data In e = End (Maybe Msg) | More [e]

data I e a = Done a
           | Cont (Maybe Msg) (In e -> (I e a, In e))

emptyStream :: In e
emptyStream = More []

instance Functor (I e) where
    fmap f (Done x)         = Done $ f x
    fmap f (Cont msg go)    = Cont msg $ after . go
      where
        after (Done x, i)       = (Done $ f x, i)
        after (Cont
