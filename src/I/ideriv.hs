------------------------------------------------------------------------------
import Prelude hiding (getLine)
import System.IO.Error (catchIOError)
------------------------------------------------------------------------------


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
