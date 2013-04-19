{-# LANGUAGE OverloadedStrings, CPP #-}

module TarInfo where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Posix.Types (CMode(CMode))
import Data.Digest.Pure.SHA

data EntryIdentifier = FileEntry      FilePath Permissions Ownership EpochTime !(Digest SHA1State) Integer
                     | DirectoryEntry FilePath Permissions Ownership EpochTime
                     | SymLinkEntry   FilePath Permissions Ownership EpochTime FilePath
                     | HardLinkEntry  FilePath Permissions Ownership EpochTime FilePath
                     | CharDevEntry   FilePath Permissions Ownership EpochTime Int Int
                     | BlockDevEntry  FilePath Permissions Ownership EpochTime Int Int
                     | NamedPipeEntry FilePath Permissions Ownership EpochTime
                     | OtherEntry     FilePath Permissions Ownership EpochTime Char !(Digest SHA1State) Integer
                     deriving (Eq, Ord)

instance Show EntryIdentifier where
  show = showEntryIdentifier

showEntryIdentifier :: EntryIdentifier -> String
showEntryIdentifier (FileEntry      path perm owner time sha size   ) = printf "%c%s %s %s %4s %s" '-' (showPermissions perm) (showOwnership owner) (take 7 $ show sha) (tgmk size) path
showEntryIdentifier (DirectoryEntry path perm owner time            ) = printf "%c%s %s %12s %s"    'd' (showPermissions perm) (showOwnership owner) ([]::String) path
showEntryIdentifier (SymLinkEntry   path perm owner time target     ) = error $ show [__FILE__, show __LINE__]
showEntryIdentifier (HardLinkEntry  path perm owner time target     ) = error $ show [__FILE__, show __LINE__]
showEntryIdentifier (CharDevEntry   path perm owner time mnr mjr    ) = error $ show [__FILE__, show __LINE__]
showEntryIdentifier (BlockDevEntry  path perm owner time mnr mjr    ) = error $ show [__FILE__, show __LINE__]
showEntryIdentifier (NamedPipeEntry path perm owner time            ) = error $ show [__FILE__, show __LINE__]
showEntryIdentifier (OtherEntry     path perm owner time ch sha size) = printf "%c%s %s %s %4s %s" ch (showPermissions perm) (showOwnership owner) (take 7 $ show sha) (tgmk size) path

showContentType :: Tar.EntryContent -> Lbs.ByteString
showContentType (NormalFile _ _)       = "-"
showContentType (Directory)            = "d"
showContentType (SymbolicLink _)       = "l"
showContentType (HardLink _)           = "l"
showContentType (CharacterDevice _ _)  = "c"
showContentType (BlockDevice _ _)      = "b"
showContentType (NamedPipe)            = "p"
showContentType (OtherEntryType _ _ _) = "?"

getPath :: EntryIdentifier -> FilePath
getPath (FileEntry      p _ _ _ _ _  ) = p
getPath (DirectoryEntry p _ _ _      ) = p
getPath (SymLinkEntry   p _ _ _ _    ) = p
getPath (HardLinkEntry  p _ _ _ _    ) = p
getPath (CharDevEntry   p _ _ _ _ _  ) = p
getPath (BlockDevEntry  p _ _ _ _ _  ) = p
getPath (NamedPipeEntry p _ _ _      ) = p
getPath (OtherEntry     p _ _ _ _ _ _) = p

identify :: Entry -> EntryIdentifier
identify e@(Entry _ (NormalFile bytes size       ) perm owner time _) = FileEntry      (entryPath e) perm owner time (sha1 bytes) (toInteger size)
identify e@(Entry _ (Directory                   ) perm owner time _) = DirectoryEntry (entryPath e) perm owner time
identify e@(Entry _ (SymbolicLink target         ) perm owner time _) = SymLinkEntry   (entryPath e) perm owner time (fromLinkTarget target)
identify e@(Entry _ (HardLink target             ) perm owner time _) = HardLinkEntry  (entryPath e) perm owner time (fromLinkTarget target)
identify e@(Entry _ (CharacterDevice major minor ) perm owner time _) = CharDevEntry   (entryPath e) perm owner time major minor
identify e@(Entry _ (BlockDevice major minor     ) perm owner time _) = BlockDevEntry  (entryPath e) perm owner time major minor
identify e@(Entry _ (NamedPipe                   ) perm owner time _) = NamedPipeEntry (entryPath e) perm owner time
identify e@(Entry _ (OtherEntryType ch bytes size) perm owner time _) = OtherEntry     (entryPath e) perm owner time ch (sha1 bytes) (toInteger size)

listEntries :: Tar.Entries Tar.FormatError -> [Tar.Entry]
listEntries (Tar.Next entry es) = entry : listEntries es
listEntries (Tar.Done) = []
listEntries (Tar.Fail e) = error $ show e

showPermissions :: Permissions -> String
showPermissions (CMode n) = concat $ map rwx $ printf "%03o" n

rwx :: Char -> String
rwx '0' = "---"
rwx '1' = "--x"
rwx '2' = "-w-"
rwx '3' = "-wx"
rwx '4' = "-r-"
rwx '5' = "r-x"
rwx '6' = "rw-"
rwx '7' = "rwx"
rwx  c  = error $ "FileMode Over 7: " ++ [c]

showOwnership :: Ownership -> String
showOwnership (Ownership uname gname uid gid) = printf "%s(%d)/%s(%d)" uname uid gname gid

showPath :: Tar.Entry -> Lbs.ByteString
showPath = C8.pack . entryPath

showTime :: Tar.Entry -> Lbs.ByteString
showTime = undefined

tgmk :: Integer -> String
tgmk n | n <             1000 = show n
       -- Kilo value with point
       | n <            10000 = (++ "K") $ take 3 $ show $ (fromInteger n) / 1000
       -- Kilo value without point
       | n <          1000000 = (++ "K") $ show $ (fromInteger n) `div` 1000
       -- Mega value with point
       | n <         10000000 = (++ "M") $ take 3 $ show $ (fromInteger n) / 1000000
       -- Mega value without point
       | n <       1000000000 = (++ "M") $ show $ (fromInteger n) `div` 1000000
       -- Giga value with point
       | n <      10000000000 = (++ "G") $ take 3 $ show $ (fromInteger n) / 1000000000
       -- Giga value without point
       | n <    1000000000000 = (++ "G") $ show $ (fromInteger n) `div` 1000000000
       -- Tera value with point
       | n <   10000000000000 = (++ "T") $ take 3 $ show $ (fromInteger n) / 1000000000000
       -- Tera value without point
       | n < 1000000000000000 = (++ "T") $ show $ (fromInteger n) `div` 1000000000000
       -- More big value
       | otherwise            = show n
