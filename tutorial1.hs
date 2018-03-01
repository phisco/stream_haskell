import Control.Monad (unless)
import Pipes
import System.IO (isEOF)

--         +--------+-- A 'Producer' that yields 'String's
--         |        |
--         |        |      +-- Every monad transformer has a base monad.
--         |        |      |   This time the base monad is 'IO'.
--         |        |      |  
--         |        |      |  +-- Every monadic action has a return value.
--         |        |      |  |   This action returns '()' when finished
--         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop

{-loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"-}
    {-lift $ putStrLn str          -- The body of the 'for' loop-}

main = runEffect $ for stdinLn (lift . putStrLn)
foreach = runEffect $ for (each (Just 1)) (lift . print)
