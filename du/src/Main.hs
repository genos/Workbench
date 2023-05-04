module Main where

import Control.Monad ((<=<))
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import qualified System.Posix.Files as P

fileSize :: FilePath -> IO Integer
fileSize path = (toInteger . P.fileSize) <$> P.getFileStatus path

dirSize :: FilePath -> IO Integer
dirSize = fileSize

isDir :: FilePath -> IO Bool
isDir path = P.isDirectory <$> P.getFileStatus path

dirEntries :: FilePath -> IO [FilePath]
dirEntries path = f <$> getDirectoryContents path
  where
    f = map ((path ++ "/") ++) . drop 2

data FS r
    = File FilePath
    | Dir
        FilePath
        [r]

instance Functor FS where
    fmap _ (File x) = File x
    fmap f (Dir x ys) = Dir x (map f ys)

instance F.Foldable FS where
    foldMap = T.foldMapDefault

instance T.Traversable FS where
    sequenceA (File x) = pure $ File x
    sequenceA (Dir x ys) = Dir x <$> T.sequenceA ys

{- | Generic pure hylomorphism, slightly rewritten to match Edward Kmett's
| version in @recursion-schemes@ (apparently improves sharing)
-}
hylo :: (Functor f) => (a -> f a) -> (f b -> b) -> a -> b
hylo g f = h where h = f . fmap h . g

{- | Generic monadic hylomorphism, slightly rewritten to match Edward Kmett's
| version in @recursion-schemes@ (apparently improves sharing)
-}
hyloM :: (Traversable f, Monad m) => (a -> m (f a)) -> (f b -> m b) -> a -> m b
hyloM g f = hM where hM = f <=< T.mapM hM <=< g

du :: FilePath -> IO Integer
du = hyloM getFiles sumFiles
  where
    getFiles path =
        isDir path
            >>= \b -> if b then Dir path <$> dirEntries path else return $! File path
    sumFiles (File x) = fileSize x
    sumFiles (Dir x ys) = (sum ys +) <$> dirSize x

main :: IO ()
main = do
    args <- getArgs
    size <- du $ if null args then "." else head args
    print size
