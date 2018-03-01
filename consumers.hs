import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Pipes
doubleUp :: Monad m => Consumer String m String
doubleUp = do
    str1 <- await
    str2 <- await
    return (str1 ++ str2)

-- more concise: doubleUp = (++) <$> await <*> await

main = runEffect $ lift getLine >~ doubleUp >~ stdoutLn
