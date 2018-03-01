import Pipes.Concurrent
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Pipes
import qualified Pipes.Prelude as P

main = do
        (output, input) <- spawn Unbounded

        forkIO $ do runEffect $ 
