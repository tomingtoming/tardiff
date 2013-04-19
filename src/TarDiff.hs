{-# LANGUAGE CPP #-}

import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip as Gz
import qualified Data.ByteString.Lazy as Lbs

import Diff
import TarInfo

main :: IO ()
main = do
  (bef:aft:_) <- getArgs
  befTar <- Lbs.readFile bef >>= extract >>= evaluate >>= return . fmap identify
  aftTar <- Lbs.readFile aft >>= extract >>= evaluate >>= return . fmap identify
  diffData <- evaluate $ diff getPath befTar aftTar :: IO [Diff EntryIdentifier]
  mapM_ (putStrLn . show) diffData
  return ()

extract :: Lbs.ByteString -> IO [Tar.Entry]
extract lbs =
  fmap (listEntries . Tar.read) $ (evaluate $ Gz.decompress lbs) `catch` (\e -> return (e::SomeException) >> evaluate lbs)
