import qualified Pipes.Prelude as P
import Data.List.Split
import Control.Monad.Trans.State.Strict
import Pipes
import Control.Exception
import Control.Monad (forever,replicateM, when, unless)
import System.IO
import  System.Random
import Data.Map.Strict
a' :: Pipe a a IO ()
a' = do
        x <- await
        y <- await
        yield y
        yield x
a :: Pipe a a IO ()
a = do
        x <- await
        y <- await
        yield x
        yield y
        a
p = runEffect $ P.stdinLn >-> a >-> P.tee P.stdoutLn >-> a' >-> P.stdoutLn
b = runEffect $ P.stdinLn >-> a >-> a' >-> P.stdoutLn
getNums = P.stdinLn >-> P.map read 
printEverything =  P.map show >-> P.stdoutLn
-- signature has to be declared otherwise runEffect gets confused and ghc goes berserk
nums' :: IO ()
nums' = runEffect $ getNums >-> 
                    P.map (>0) >-> P.takeWhile not >-> 
                    printEverything

mean x = do
            y <- P.fold (+) 0 id $ each [0..x]
            return $ y/x

sum' = forever $  do
        a <- await
        n <- lift get
        (lift . put) (n + a)
        yield (n+a)

-- fvr = forever $ yield 1

m :: IO ()
m= flip evalStateT 0 $ 
        runEffect $ 
        each [1..100] >-> sum' >-> P.map show >-> P.stdoutLn

-- util avg' function
avg' :: Fractional p => [p] -> p
avg' [] = 0
avg' l=  sum l / fromIntegral (length l)

movingAvgPipe n = forever $ do
        a <- await
        l <- lift get
        if length l == n
                    then lift $ put $ tail l ++ [a]
                    else lift $ put $ l ++ [a]
        yield $ avg' l

-- https://www.reddit.com/r/haskell/comments/5axh2j/two_methods_of_using_statet_with_pipes_one_of/
--
-- StateT of Effect of IO, no multiple state per stage, single state for the whole effect,
-- ... >-> avg n >-> avg n >-> .. doesn't work as intende as intended
--
movingAvg n l = flip evalStateT [] $
                runEffect $
                each (l :: [Float]) >-> movingAvgPipe 6 >->
                -- avg 6  -- doesn't work
                P.take n >-> P.map show >-> P.stdoutLn
-- movingAvg 100 $ cycle [1,2,3,4]
--

--
-- Effect of StateT of IO, allows multiple stages with separated states
-- 
avgMulti :: Int -> Pipe Float Float IO () 
avgMulti n = flip evalStateT [] $ 
        forever $ 
            do
                a <- lift await
                l <- get
                if length l == n
                    then put $ tail l ++ [a]
                    else put $ l ++ [a]
                lift $ yield $ avg' l
-- lift are reversed wrt movi


movingMult n l = runEffect $ 
                each (l ::[Float]) >-> 
                avgMulti 30 >-> avgMulti 30 >->
                P.take n >-> P.map show >-> P.stdoutLn

-- prints the state of a cyclic buffer of length n
printState n = flip evalStateT [] $ 
        forever $ 
            do
                a <- lift await
                l <- get
                if length l == n
                    then put $ tail l ++ [a]
                    else put $ l ++ [a]
                liftIO $ print l

-- cyclic buffer with stateT (check)
check :: (Show a) => Int -> Int -> Producer a IO () -> IO ()
check n len l= runEffect $ l >-> P.take len  >-> printState n
-- check 10 30 $ each $ [1..]
--  

--
-- factorized out the last action in printState 
--
doStuff window stuff = flip evalStateT [] $ 
        forever $ 
            do
                a <- lift await
                l <- get
                if length l == window
                    then put $ tail l ++ [a]
                    else put $ l ++ [a]
                stuff l

--
-- here doStuff is used to create a consumer
--
-- checks if check' is the same of check
check' :: (Show a) => Int -> Int -> Producer a IO () -> IO ()
check' n len l= runEffect $ l >-> P.take len  >-> doStuff n (liftIO . print)
-- check' 10 30 $ each $ [1..]
-- 
-- use doStuff to create a pipe (stuff should contain some yield)
pipe window len producer = runEffect $ producer >-> P.take len >-> doStuff window (lift . yield) >-> P.map (show . avg') >-> P.stdoutLn
--
-- pipe 10 30 $ each $ cycle [1,2,3]
--

tokenize = go []
    where 
        go x = do
            c <- await
            case c of
                Nothing -> yield Nothing
                Just a ->  if a == ' ' || a == '\n'
                        then do
                            yield $ Just x
                            go []
                    else
                        go $ x ++ [a]

wordCount = go empty
    where
        go m = do
            x <- await
            case x of 
                Nothing -> yield m
                Just x' -> if member x' m
                                then go $ adjust (+1) x' m
                                else go $ insert x' 1 m



readChars handle = do
                    y <- (lift . hIsEOF) handle 
                    case y of
                        False -> do
                            x <- (lift . hGetChar) handle
                            yield $ Just x
                            readChars handle
                        True -> yield Nothing

main = withFile "dmesg.txt" ReadMode $  (\file ->  
                    runEffect $
                    readChars file >-> 
                    tokenize >->
                    wordCount >->
                    P.map show >->
                    P.stdoutLn)

