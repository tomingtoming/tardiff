{-# LANGUAGE CPP #-}

import Prelude hiding (catch)
import Control.Exception
import System.Environment (getArgs)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip as Gz
import qualified Data.ByteString.Lazy as Lbs
import Data.Either.Utils (fromRight)

import Diff
import TarInfo

main :: IO ()
main = do
  (bef:aft:_) <- getArgs
  befTar <- Lbs.readFile bef >>= extractRec >>= evaluate >>= return . fmap identify
  aftTar <- Lbs.readFile aft >>= extractRec >>= evaluate >>= return . fmap identify
  diffData <- evaluate $ diff getPath befTar aftTar :: IO [Diff EntryIdentifier]
  mapM_ (putStrLn . show) diffData
  return ()

extract :: Lbs.ByteString -> IO [Tar.Entry]
extract lbs =
  fmap (listEntries . Tar.read) $ (evaluate $ Gz.decompress lbs) `catch` (\e -> return (e::SomeException) >> evaluate lbs)

extractRec :: Lbs.ByteString -> IO [Tar.Entry]
extractRec lbs = extract lbs >>= recs
  where
    recs :: [Tar.Entry] -> IO [Tar.Entry]
    recs []                                                          = return []
    recs (e@(Tar.Entry _ (Tar.NormalFile bytes _) _ _ _ _):es)       = do
                                                                         xs <- recs es
                                                                         ys <- exts (Tar.entryPath e ++ "/") bytes
                                                                         return $ e : ys ++ xs
    recs (e:es)                                                      = recs es >>= \xs -> return $ e : xs
    exts :: PathPrefix -> Lbs.ByteString -> IO [Tar.Entry]
    exts prefix bytes = fmap (map $ prependTarPath prefix) $ (extract bytes >>= evaluate) `catch` (\e -> return (e::SomeException) >> return [])

type PathPrefix = String

prependTarPath :: PathPrefix -> Tar.Entry -> Tar.Entry
prependTarPath prefix entry =
  let path = fromRight $ Tar.toTarPath False $ (++) prefix $ Tar.fromTarPath (Tar.entryTarPath entry)
  in entry { Tar.entryTarPath = path }
