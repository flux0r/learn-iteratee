import qualified Control.Applicative as A
import           Control.Exception (ErrorCall(..), SomeException,
                                    toException)
import           Data.Char (isHexDigit, digitToInt)
import           Data.Iteratee.IO.Posix (myfdRead)
import           Foreign.C (peekCAStringLen)
import           Foreign.Marshal.Alloc (allocaBytes)
import           Prelude hiding ((>>=), break, drop, head, take)
import qualified Prelude as P ((>>=), break, drop, head, take)
import           System.Posix (Fd, closeFd, openFd)

type Msg = SomeException

data S es   = Gob [es]
            | End (Maybe Msg)
  deriving (Show)

emptyS :: S es
emptyS = Gob []

data I es a = Done ! a
            | Cont ! (Maybe Msg) (S es -> (I es a, S es))

(<$>) :: (a -> b) -> I es a -> I es b
(<$>) f (Done x)         = Done $ f x
(<$>) f (Cont msg next)  = Cont msg (step . next)
  where
    step ((Done x), s)  = (Done $ f x, s)
    step (i, s)         = (f <$> i, s)

pureI :: a -> I es a
pureI x = Done x

(<*>) :: I es (a -> b) -> I es a -> I es b
(Done f) <*> i          = f <$> i
(Cont msg next) <*> i   = Cont msg (step . next)
  where
    step ((Done f), s)  = (f <$> i, s)
    step (j, s)         = (j <*> i, s)

(>>=) :: I es a -> (a -> I es b) -> I es b
(Done x) >>= f          = f x
(Cont msg next) >>= f   = Cont msg (step . next)
  where
    step (Done y, s)        = case f y of
        Cont Nothing next'  -> next' s
        i                   -> (i, s)
    step (i, s)             = (i >>= f, s)

instance Functor (I es) where
    fmap = (<$>)

instance A.Applicative (I es) where
    pure    = pureI
    (<*>)   = (<*>)

instance Monad (I es) where
    return  = pureI
    (>>=)   = (>>=)


----------------------------------------------------------------------------
-- | Throw an irrecoverable error.
throwMsg :: Msg -> I es a
throwMsg x = Cont (Just x) (\s -> (throwMsg x, s))


----------------------------------------------------------------------------
-- | Throw a recoverable error.
recoverMsg :: Msg -> (S es -> (I es a, S es)) -> I es a
recoverMsg x next = Cont (Just x) next


----------------------------------------------------------------------------
-- | Produce the control message to give to throwErr. If the stream is
-- already terminated, keep the original message.
setEnd :: S es -> Msg
setEnd (End (Just x))   = x
setEnd _                = signalEnd

signalEnd :: Msg
signalEnd = toException $ ErrorCall "End"

signalDiverge :: Msg
signalDiverge = toException $ ErrorCall "Divergent iteratee"


----------------------------------------------------------------------------
-- | Get the final result from an iteratee.
run :: I es a -> a
run (Done x)            = x
run (Cont Nothing next) = step . fst $ next (End Nothing)
  where
    step (Done y)       = y
    step (Cont msg _)   = error $ "possibly divergent, control message: " ++ show msg
run (Cont (Just x) _)   = error $ "control message: " ++ show x


----------------------------------------------------------------------------
-- | Some primitive iteratees.
continue :: (S es -> (I es a, S es)) -> I es a
continue = Cont Nothing

continueM :: (S es -> (I es a, S es)) -> (I es a, S es)
continueM next = (Cont Nothing next, emptyS)

-- Break the stream into a tuple. The first item in the tuple contains the
-- prefix and second item contains the stream starting at the first element
-- that matched the given predicate.
break :: (es -> Bool) -> I es [es]
break p = continue (step [])
  where
    step r (Gob []) = continueM (step r)
    step r (Gob es) = case P.break p es of
        (_, [])         -> continueM (step (r ++ es))
        (stream, tail)  -> (Done (r ++ stream), Gob tail)
    step r s        = (Done r, s)

-- Look at the next item in the stream without removing it.
peek :: I es (Maybe es)
peek = continue step
  where
    step (Gob [])       = (peek, emptyS)
    step s@(Gob (x:_))  = (Done (Just x), s)
    step s              = (Done Nothing, s)

-- Get the next element of the stream and throw a recoverable error if the
-- stream is ended.
head :: I es es
head = continue step
  where
    step (Gob [])       = (head, emptyS)
    step (Gob (x:xs))   = (Done x, Gob xs)
    step s              = (Cont (Just (setEnd s)) step, s)

-- Match the given sequence against the stream and keep a tally of the
-- matches, removing them from the stream.
-- heads :: [a] -> I es a
heads :: (Enum a, Eq es, Num a) => [es] -> I es a
heads xs = iter 0 xs
  where
    iter r []                       = return r
    iter r xs                       = continue (step r xs)
    step r xs (Gob [])              = (iter r xs, emptyS)
    step r (x:xs) s@(Gob (y:ys))    =
        if x == y
            then step (succ r) xs (Gob ys)
            else (Done r, s)
    step r _ s                      = (Done r, s)

-- Skip to the end of the stream.
skipToEnd :: I es ()
skipToEnd = continue step
  where
    step (Gob _)    = (skipToEnd, emptyS)
    step s          = (Done (), s)

-- Skip n elements of the stream.
drop :: Int -> I es ()
drop 0 = return ()
drop n = continue step
  where
    step (Gob xs) | length xs <= n  = (drop (n - length xs), emptyS)
    step (Gob xs)                   = (Done (), Gob $ P.drop n xs)
    step s                          = (Done (), s)

-- Check whether the stream is finished.
isFinished :: I es (Maybe Msg)
isFinished = continue step
  where
    step (Gob [])       = (isFinished, emptyS)
    step s@(End _)      = (Done (Just $ setEnd s), s)
    step s              = (Done Nothing, s)


----------------------------------------------------------------------------
-- | Reading headers and content from an HTTP-like stream.
type Line = String

terminators :: I Char Int
terminators = heads "\r\n" >>= (\n ->
    if n == 0
        then heads "\n"
        else return n)

isLineControlChar :: Char -> Bool
isLineControlChar = (\x -> x == '\r' || x == '\n')

findLineControlChars :: I Char Line
findLineControlChars = break isLineControlChar

readLine :: I Char Int
readLine = findLineControlChars >>= \l -> terminators

readLines :: I Char (Either [Line] [Line])
readLines = ls []
  where
    ls r        = findLineControlChars >>= \l -> terminators >>= step r l
    step r _ 0  = return . Left . reverse $ r
    step r [] _ = return . Right . reverse $ r
    step r x _  = ls (x:r)


----------------------------------------------------------------------------
-- | Enumerators
type E es a = I es a -> I es a
type EM es m a = I es a -> m (I es a)


----------------------------------------------------------------------------
-- | Apply the iteratee to the terminated stream. This is almost the same as
-- run except that it has error handling.
enumEnd :: E es a
enumEnd (Cont Nothing next) = iter . fst $ next (End Nothing)
  where
    iter i@(Done _)             = i
    iter i@(Cont (Just _) _)    = i
    iter _                      = throwMsg signalDiverge
enumEnd i                       = i


----------------------------------------------------------------------------
-- | Tell the iteratee the stream finished with an error.
enumErr :: Msg -> E es a
enumErr x (Cont Nothing next)   = iter . fst $ next (End (Just x))
  where
    iter i@(Done _)             = i
    iter i@(Cont (Just _) _)    = i
    iter _                      = throwMsg signalDiverge
enumErr _ i                     = i


----------------------------------------------------------------------------
-- | Composition of enumerators. It's flipped because normal composition is
-- right-associative.
(|->) :: E es a -> E es a -> E es a
(|->) = flip (.)

----------------------------------------------------------------------------
-- | Pass a list as a single gob to an iteratee.
enumSingle :: [es] -> E es a
enumSingle xs (Cont Nothing next)   = fst $ next (Gob xs)
enumSingle _ i                      = i

----------------------------------------------------------------------------
-- | Pass a list in gobs no larger than n to an iteratee.
enumGobs :: [es] -> Int -> E es a
enumGobs xs@(_:_) n (Cont Nothing next) =
    let (y1, y2) = splitAt n xs in
        enumGobs y2 n . fst $ next (Gob y1)
enumGobs _ _ i                          = i

----------------------------------------------------------------------------
-- | Enumerator for a POSIX Fd. Only uses one buffer.
-- 
-- Data.Iteratee.IO.Posix (myfdRead)
-- Foreign (Ptr)
-- Foreign.C (CChar, CStringLen, Errno, peekCAStringLen)
-- Foreign.Marshal.Alloc (allocaBytes)
-- System.Posix (ByteCount, Fd)
--
-- allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
-- myfdRead :: Fd -> Ptr CChar -> ByteCount -> IO (Either Errno ByteCount)
-- peekCAStringLen :: CStringLen -> IO String
--
enumFd :: Fd -> I Char a -> IO (I Char a)
enumFd fd i = allocaBytes (fromIntegral buf) (iter i)
  where
    buf = 5 -- Should be about 1024 for real code
    iter (Cont Nothing next) = readIt next
    iter i                   = \p -> return i
    readIt next p = do
        n <- myfdRead fd p buf
        case n of
          Left errno    -> return . fst $
            next (End (Just (toException (ErrorCall "IO Error"))))
          Right 0       -> return $ continue next
          Right n       -> do 
            str <- peekCAStringLen (p, fromIntegral n)
            iter (fst $ next (Gob str)) p

----------------------------------------------------------------------------
-- | Enumeratees
type Ee es a = I es a -> I es (I es a)


----------------------------------------------------------------------------
-- | The nested stream is a prefix, exactly n elements long, of the outer
-- stream. Read the n elements and apply the given nested iteratee to the
-- stream of the read elements.
take :: Int -> Ee es a
take 0 i@(Cont _ _)         = return i
take n (Cont Nothing next)  = continue (iter n next)
  where
    iter n next (Gob [])    = continueM (iter n next)
    iter n next s@(Gob xs)
      | length xs < n       = (take (n - length xs) . fst $ next s, emptyS)
    iter n next (Gob xs)    = let (x1, x2) = splitAt n xs in
                                (Done (fst $ next (Gob x1)), (Gob x2))
    iter n next s           = (Done (fst $ next s), s)
take n i                    = drop n >> return i

enumChunkDecoded :: Ee Char a
enumChunkDecoded i = readSize
  where
    readSize = break (== '\r') >>= (checkCrlf i . checkSize)
    checkCrlf i m = do
        n <- heads "\r\n"
        if n == 2
            then m
            else frameErr (exc "Bad Chunk: no CRLF") i
    checkSize "0" = checkCrlf i (return i)
    checkSize xs@(_:_) = maybe (frameErr (exc ("Bad chunk size: " ++ xs)) i)
                               readChunk $ readHex 0 xs
    readChunk x = take x i >>= \r -> checkCrlf r $ enumChunkDecoded r
    readHex r ""        = Just r
    readHex r (x:xs)
      | isHexDigit x    = readHex (16*r + digitToInt x) xs
    readHex r _         = Nothing
    frameErr e i = recoverMsg (exc "Frame error")
                              (\s -> (return (enumErr e i), s))
    exc m = toException (ErrorCall $ "Chunk decoding exc: " ++ m)


----------------------------------------------------------------------------
-- | Tests

-- Pure tests
testStr1 :: String
testStr1 = "header1: v1\rheader2: v2\r\nheader3: v3\nheader4: v4\n" ++
           "header5: v5\r\nheader6: v6\r\nheader7: v7\r\n\nrest\n"

readLinesRest :: I Char (Either [Line] [Line], String) 
readLinesRest = readLines >>= (\ls ->
                  break (const False) >>= (\rest ->
                  return (ls, rest)))

testp1 :: Bool
testp1 =
    let (Right lines, rest) = run $ enumSingle testStr1 readLinesRest in
    lines == ["header1: v1","header2: v2","header3: v3","header4: v4",
              "header5: v5","header6: v6","header7: v7"] &&
        rest == "rest\n"
