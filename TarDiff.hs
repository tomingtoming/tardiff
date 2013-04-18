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
  befTar <- Lbs.readFile bef >>= extract >>= evaluate
  aftTar <- Lbs.readFile aft >>= extract >>= evaluate
--mapM_ (Lbs.putStrLn . showEntry) befTar
--mapM_ (Lbs.putStrLn . showEntry) aftTar
  mapM_ (putStrLn . show . fmap (show . showEntry)) $ diff identify befTar aftTar
  return ()

extract :: Lbs.ByteString -> IO [Tar.Entry]
extract lbs =
  fmap (listEntries . Tar.read) $ (evaluate $ Gz.decompress lbs) `catch` (\e -> return (e::SomeException) >> evaluate lbs)
