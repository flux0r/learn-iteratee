import Control.Monad ((>=>), liftM2, liftM)
import Data.Maybe (isJust, fromJust)

type LChar = Maybe Char

getchar0 :: IO LChar
getchar0 = (getChar >>= return . Just) `catch` \_ -> return Nothing

getline0 :: IO String
getline0 = loop ""
  where
    loop x = getchar0 >>= check x
    check acc (Just c) | c /= '\n'  = loop (c:acc)
    check acc _                     = return (reverse acc)

getlines0 :: IO [String]
getlines0 = loop []
  where
    loop :: [String] -> IO [String]
    loop x = getline0 >>= check x
    check :: [String] -> String -> IO [String]
    check x ""  = return (reverse x)
    check x xs  = loop (xs:x)

-- getline0 and getlines0 can be viewed as processes receiving lifted
-- characters on a dedicated channel stdin and terminating with a value,
-- which is either a line or a list of lines.
--
-- The simplest model to represent these is a data type with a constructor
-- for each process operation, either finished or inputting a character.
-- This data type is called an iteratee.
data I a = Done a
         | GetC (LChar -> I a)

-- The line reader.
getline :: I String
getline = loop ""
  where
    loop :: String -> I String
    loop = \x -> GetC (check x)
    check :: String -> LChar -> I String
    check = \x xs ->
        if (isJust xs && fromJust xs /= '\n')
            then loop $ (:) (fromJust xs) x
            else Done (reverse x)

-- Things like getline don't actually do anything; they have to be
-- interpreted. The interpreter takes a finite source and a process to send
-- it to (like getline).
--
-- When the string is exhausted, the iteratee is sent EOF, which is Nothing
-- in this particular case. So eval somestring someiteratee is like a Unix
-- pipeline cat somestring | someiteratee .
eval :: String -> I a -> a
eval ""     (GetC x)    = eval "" $ x Nothing
eval (c:cs) (GetC x)    = eval cs $ x (Just c)
eval cs     (Done x)    = x

instance Monad I where
    return = Done
    (Done x) >>= f = f x
    (GetC x) >>= f = GetC (x >=> f)

-- Chain simple iteratees to make larger ones by using the monadic bind
-- operation. To make a line reader, chain getline.
getlines :: I [String]
getlines = loop []
  where
    loop :: [String] -> I [String]
    loop x = getline >>= check x
    check :: [String] -> String -> I [String]
    check xs "" = return (reverse xs)
    check xs x  = loop (x:xs)

-- Instead of eval, we can separate the sending of data from the sending of
-- EOF. The first feeds characters until finished and the second says when
-- there are no more data and extracts the result.
en_str :: String -> I a -> I a
en_str ""       i           = i
en_str (c:t)    (GetC k)    = en_str t $ k (Just c)
en_str _        (Done x)    = return x

run :: I a -> a
run (GetC k) = run $ k Nothing
run (Done x) = x

-- Clearly, eval str == run . en_str str.  The function en_str is an
-- enumerator that enumerates a source (str in this case).
--
-- Composing enumerators corresponds to catenating their sources. So, en_str
-- (s1 ++ s2) == (en_str s2) . (en_str s1) .
--
-- Iteratees are also composable and act as parsers. So, en_str (s1 ++ s2)
-- (i >>= f) == (en_str s1 i) >>= (en_str s2) . f.

-- The left-biased alternation combinator gives choice.
(<|) :: I a -> I a -> I a
Done x  <| _        = Done x
_       <| Done x   = Done x
GetC k1 <| GetC k2  = GetC (\c -> k1 c <| k2 c)

-- Failure is the left zero of bind. So, failure >>= f == failure .

-- Right distributivity. i >>= \x -> (k1 x <| k2 x) == (i >>= k1) <| (i >>=
-- k2) . Left distributivity doesn't hold since <| commits to whatever a
-- parser recognizes first.


-- These are the primitive iteratees (parsers).
failure :: I a      -- The parser of nothing.
failure = GetC (const failure)

empty :: a -> I a   -- The parser of an empty string.
empty x = Done x

oneL :: I LChar     -- The parser of a single lifted character.
oneL = GetC Done

one :: I Char       -- The parser of a one-character string.
one = oneL >>= maybe failure return

pSat :: (LChar -> Bool) -> I LChar
pSat f = oneL >>= \c -> if f c then return c else failure

-- We can now derive getline and getlines in terms of the primitive
-- iteratees.
-- pGetline = \x -> x <| liftM2 (:) one pGetline
pGetline :: I String
pGetline = filter <| liftM2 (:) one pGetline
  where
    filter = pSat (\c -> c == Just '\n' || c == Nothing) >> return ""

-- Factor out oneL from pSat and one.
pGetline' :: I String
pGetline' = oneL >>= check
  where
    check (Just '\n')   = return ""
    check Nothing       = return ""
    check (Just c)      = liftM (c:) pGetline'
