------------------------------------------------------------------------------
import Control.Monad (liftM2, liftM)
import Prelude hiding (getLine)
import System.IO.Error (catchIOError)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | ITERATEES


-----------------------------------------------------------------------------
-- | Normal IO
--
-- I want to read lines from stdin until an empty line. One way to do it is
-- to use standard Haskell IO with getChar.

type LiftedChar = Maybe Char

-- getChar0 knows what to do if there is an error: return Nothing.
getChar0 :: IO LiftedChar
getChar0 = (getChar >>= return . Just) `catchIOError` (\_ -> return Nothing)

-- Read one line...
getLine0 :: IO String
getLine0 = iter ""
  where
    iter cs = getChar0 >>= \lc ->
        case lc of
            (Just c) | c /= '\n'    -> iter (c:cs)
            _                       -> return $ reverse cs

-- ...Now read a bunch of lines.
getLines0 :: IO [String]
getLines0 = iter []
  where
    iter ls = getLine0 >>= \l ->
        case l of
            []  -> return $ reverse ls
            l   -> iter (l:ls)


------------------------------------------------------------------------------
-- | IO as a process
--
-- The process is either Done, or it can take more data.
data I a = Done a
         | More (LiftedChar -> I a)

getLine :: I String
getLine = iter ""
  where
    iter = \cs -> More $ \lc ->
        case lc of
            (Just c) | c /= '\n'    -> iter (c:cs)
            _                       -> Done $ reverse cs

-- I need a way to observe the Iteratee. eval takes a String and feeds it to
-- the process.
eval :: String -> I a -> a
eval ""     (More go)   = eval "" $ go Nothing
eval (c:cs) (More go)   = eval cs $ go (Just c)
eval cs     (Done x)    = x

-- To combine getLine into a process that reads a bunch of lines, I can make I
-- a Monad.

-- (>>>) is function composition for monadic functions.
(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = \x -> f x >>= g

iReturn :: a -> I a
iReturn = Done

iBind :: I a -> (a -> I b) -> I b
iBind (More go) f   = More $ go >>> f
iBind (Done x)  f   = f x

instance (Monad I) where
    return  = iReturn
    (>>=)   = iBind

-- So now, getLines will look a lot like getLines0.
getLines :: I [String]
getLines = iter []
  where
    iter = \ls -> getLine >>= \l ->
        case l of
            []  -> return $ reverse ls
            l   -> iter (l:ls)

-- Tests
t110 :: String
t110 = eval "abd\nxxx\nf" getLine

t111 :: [String]
t111 = eval "abd\nxxx\nf" getLines


------------------------------------------------------------------------------
-- | ENUMERATORS

-- If I factor eval above (eval :: String -> I a -> a), I get the composition
-- of run (run :: I a -> a) and enumString (enumString :: String -> I a -> I
-- a).

enumString :: String -> I a -> I a
enumString ""       i           = i
enumString (c:cs)   (More go)   = enumString cs $ go (Just c)
enumString _        (Done x)    = Done x 

run :: I a -> a
run (More go)   = run $ go Nothing
run (Done x)    = x

-- This version of run will report an error if the process tries to get more
-- data after receiving Nothing.
run' :: I a -> a
run' (More go)  = case go Nothing of
        Done x  -> x
        _       -> error "Divergent iteratee"


------------------------------------------------------------------------------
-- | Parsing combinators

-- This iteratee will never receive Done.
failure :: I a
failure = More (const failure)

-- Recognize the empty string.
empty :: a -> I a
empty = \x -> Done x

-- One lifted character
oneChar :: I LiftedChar
oneChar = More Done

-- Left-biased alternation
(<!) :: I a -> I a -> I a
(Done x)    <!  _           = Done x
_           <!  (Done x)    = Done x
(More go)   <!  (More go')  = More $ \c -> go c <! go' c

-- Recognizes one character string that satisfies the given predicate.
pSat :: (LiftedChar -> Bool) -> I LiftedChar
pSat = \pred ->
        oneChar >>= \c ->
            if pred c
                then return c
                else failure

-- The same as pSat except using regular Char.
pSat' :: (Char -> Bool) -> I Char
pSat' = \pred ->
    oneChar >>= \c ->
        case c of
            Just c | pred c -> return c
            _               -> failure

-- Recognize one character string.
one :: I Char
one = oneChar >>= maybe failure return

-- getLine with combinators
pGetLine :: I String
pGetLine = l <! liftM2 (:) one pGetLine
  where
    l = pSat (\c -> c == Just '\n' || c == Nothing) >> return ""

tpl :: String
tpl = run $ enumString "abd\nxxx\nf" pGetLine

tpl2 :: String
tpl2 = run $ enumString "abd" pGetLine

-- More efficient version of pGetLine. First, inline pSat and one.
pGetLine1 :: I String
pGetLine1 = l <! c
  where
    l = (oneChar >>= \lc -> case lc of
            Just c | c /= '\n'  -> failure
            _                   -> return "")
    c = (oneChar >>= maybe failure return) >>= \c' ->
            liftM (c':) pGetLine1

tpl1 :: String
tpl1 = run $ enumString "abd\nxxx\nf" pGetLine1

-- Now use the fact that bind is associative.
pGetLine2 :: I String
pGetLine2 = l <! c
  where
    l = oneChar >>= (\lc -> case lc of
            Just c | c /= '\n'  -> failure >> return ""
            _                   -> return lc >> return "")
    c = oneChar >>= (\lc -> maybe failure return lc >>= \c' ->
            liftM (c':) pGetLine2)

-- By the monad laws, failure >>= k === failure.
pGetLine3 :: I String
pGetLine3 = l <! c
  where
    l = oneChar >>= (\lc ->
        case lc of
            Just c | c /= '\n'  -> failure
            _                   -> return "")
    c = oneChar >>= (\lc ->
        case lc of
            Just c' -> liftM (c':) pGetLine3
            _       -> failure)

tpl3 :: String
tpl3 = run $ enumString "abd\nxxx\nf" pGetLine3

-- Use right distributivity.
pGetLine4 :: I String
pGetLine4 = oneChar >>= \lc -> l lc <! c lc
  where
    l = \lc' -> case lc' of
        Just c | c /= '\n'  -> failure
        _                   -> return ""
    c = \lc' -> case lc' of
        Just c'             -> liftM (c':) pGetLine4
        _                   -> failure

tpl4 :: String
tpl4 = run $ enumString "abd\nxxx\nf" pGetLine4

-- Pull out the case analysis.
pGetLine5 :: I String
pGetLine5 = oneChar >>= iter
  where
    iter (Just '\n')    = return "" <! liftM ('\n':) pGetLine5
    iter Nothing        = return "" <! failure
    iter (Just c)       = failure   <! liftM (c:) pGetLine5

tpl5 :: String
tpl5 = run $ enumString "abd\nxxx\nf" pGetLine5

-- Finally, use the definition of <!.
pGetLine' :: I String
pGetLine' = oneChar >>= iter
  where
    iter (Just '\n')    = return ""
    iter Nothing        = return ""
    iter (Just c)       = liftM (c:) pGetLine'

tp1' :: String
tp1' = run $ enumString "abd\nxxx\nf" pGetLine'

tp2' :: String
tp2' = run $ enumString "ab" pGetLine'
