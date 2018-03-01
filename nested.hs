import Pipes
import qualified Pipes.Prelude as P  -- Pipes.Prelude already has 'stdinLn'

triple :: Monad m => a -> Producer a m ()
triple x = do
    yield x
    yield x
    yield x

loop :: Producer String IO ()
loop = for P.stdinLn triple

-- This is the exact same as:
--
-- loop = for P.stdinLn $ \x -> do
--     yield x
--     yield x
--     yield x

main = runEffect $ for loop (lift . putStrLn)
main' = runEffect $
    for P.stdinLn $ \str1 ->
        for (triple str1) $ \str2 ->
            lift $ putStrLn str2

main'' = runEffect $ for P.stdinLn (triple ~> lift . putStrLn)
