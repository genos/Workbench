module Main where

-- https://gist.github.com/queertypes/8cb225ee1df57bd8b339585a3365ef84 &
-- https://vimeo.com/146374255
import           Control.Monad             (when)
import           Control.Monad.Free
import qualified Control.Monad.Free.Church as C
import           Data.Text                 (Text)
import qualified Data.Text.IO              as T
import           System.Exit               hiding (ExitSuccess)

data TeletypeF x = PutStrLn Text x
                 | GetLine (Text -> x)
                 | ExitSuccess
  deriving (Functor)

type Teletype = Free TeletypeF

putStrLn' :: MonadFree TeletypeF m => Text -> m ()
putStrLn' t = liftF $ PutStrLn t ()

getLine' :: MonadFree TeletypeF m => m Text
getLine' = liftF $ GetLine id

exitSuccess' :: MonadFree TeletypeF m => m ()
exitSuccess' = liftF ExitSuccess

run :: Teletype a -> IO a
run = iterM go
 where
  go (PutStrLn x1 x2) = T.putStrLn x1 >> x2
  go (GetLine x     ) = T.getLine >>= x
  go ExitSuccess      = exitSuccess

runPure :: Teletype a -> [Text] -> [Text]
runPure (Pure _             ) _      = []
runPure (Free (PutStrLn y t)) xs     = y : runPure t xs
runPure (Free (GetLine k   )) (x:xs) = runPure (k x) xs
runPure (Free (GetLine _   )) []     = []
runPure (Free ExitSuccess   ) _      = []

echo :: MonadFree TeletypeF m => m ()
echo = do
  c <- getLine'
  when (c /= "") $ do
    putStrLn' c
    echo

mkMain :: Teletype a -> IO ()
mkMain f = mapM_ T.putStrLn . runPure f $ replicate 100 "cat"

regMain :: IO ()
regMain = mkMain echo

mainImproved :: IO ()
mainImproved = mkMain $ C.improve echo

main :: IO ()
main = mainImproved
