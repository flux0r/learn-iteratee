module I.In where


------------------------------------------------------------------------------
import Control.Exception (SomeException)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))


------------------------------------------------------------------------------
type Msg = SomeException


------------------------------------------------------------------------------
data In i = End (Maybe Msg) | More i


------------------------------------------------------------------------------
combineIn :: Semigroup i => In i -> In i -> In i
(End msg)   `combineIn` _           = End msg
_           `combineIn` (End msg)   = End msg
(More x)    `combineIn` (More y)    = More $ x <> y

idIn :: In i
idIn = End Nothing

bindIn :: In i -> (i -> In o) -> In o
bindIn (End msg)    _   = End msg
bindIn (More x)     y   = y x   
