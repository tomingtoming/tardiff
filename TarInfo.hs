{-# LANGUAGE OverloadedStrings #-}

module TarInfo where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.ByteString.Lazy.Char8 as C8
import System.Posix.Types (FileMode, CMode(CMode))
import Data.Digest.Pure.SHA

data EntryIdentifier = FileEntry      FilePath Permissions Ownership EpochTime (Digest SHA512State)
                     | DirectoryEntry FilePath Permissions Ownership EpochTime
                     | SymLinkEntry   FilePath Permissions Ownership EpochTime FilePath
                     | HardLinkEntry  FilePath Permissions Ownership EpochTime FilePath
                     | CharDevEntry   FilePath Permissions Ownership EpochTime Int Int
                     | BlockDevEntry  FilePath Permissions Ownership EpochTime Int Int
                     | NamedPipeEntry FilePath Permissions Ownership EpochTime
                     | OtherEntry     FilePath Permissions Ownership EpochTime Char (Digest SHA512State) 
                     deriving (Show, Eq, Ord)

{- Useless statement beause of deriving.
instance Eq EntryIdentifier where
  (FileEntry      p1 _ _ _ _  ) == (FileEntry      p2 _ _ _ _  ) = undefined
  (DirectoryEntry p1 _ _ _    ) == (DirectoryEntry p2 _ _ _    ) = undefined
  (SymLinkEntry   p1 _ _ _ _  ) == (SymLinkEntry   p2 _ _ _ _  ) = undefined
  (HardLinkEntry  p1 _ _ _ _  ) == (HardLinkEntry  p2 _ _ _ _  ) = undefined
  (CharDevEntry   p1 _ _ _ _ _) == (CharDevEntry   p2 _ _ _ _ _) = undefined
  (BlockDevEntry  p1 _ _ _ _ _) == (BlockDevEntry  p2 _ _ _ _ _) = undefined
  (NamedPipeEntry p1 _ _ _    ) == (NamedPipeEntry p2 _ _ _    ) = undefined
  (OtherEntry     p1 _ _ _ _ _) == (OtherEntry     p2 _ _ _ _ _) = undefined
  _                             == _                             = False

instance Ord EntryIdentifier where
  a < b = getPath a < getPath b
-}

getPath (FileEntry      p _ _ _ _  ) = p
getPath (DirectoryEntry p _ _ _    ) = p
getPath (SymLinkEntry   p _ _ _ _  ) = p
getPath (HardLinkEntry  p _ _ _ _  ) = p
getPath (CharDevEntry   p _ _ _ _ _) = p
getPath (BlockDevEntry  p _ _ _ _ _) = p
getPath (NamedPipeEntry p _ _ _    ) = p
getPath (OtherEntry     p _ _ _ _ _) = p

identify :: Entry -> EntryIdentifier
identify e@(Entry _ (NormalFile bytes size       ) perm owner time _) = FileEntry      (entryPath e) perm owner time (sha512 bytes)
identify e@(Entry _ (Directory                   ) perm owner time _) = DirectoryEntry (entryPath e) perm owner time
identify e@(Entry _ (SymbolicLink target         ) perm owner time _) = SymLinkEntry   (entryPath e) perm owner time (fromLinkTarget target)
identify e@(Entry _ (HardLink target             ) perm owner time _) = HardLinkEntry  (entryPath e) perm owner time (fromLinkTarget target)
identify e@(Entry _ (CharacterDevice major minor ) perm owner time _) = CharDevEntry   (entryPath e) perm owner time major minor
identify e@(Entry _ (BlockDevice major minor     ) perm owner time _) = BlockDevEntry  (entryPath e) perm owner time major minor
identify e@(Entry _ (NamedPipe                   ) perm owner time _) = NamedPipeEntry (entryPath e) perm owner time
identify e@(Entry _ (OtherEntryType ch bytes size) perm owner time _) = OtherEntry     (entryPath e) perm owner time ch (sha512 bytes) 

instance Eq Entry where
  entry1@(Entry _ content1 perm1 owner1 time1 format1) == entry2@(Entry _ content2 perm2 owner2 time2 format2) =
    and [entryPath entry1 == entryPath entry2, content1 == content2, perm1 == perm2, owner1 == owner2]

instance Ord Entry where
  entry1@(Entry _ content1 perm1 owner1 time1 format1) < entry2@(Entry _ content2 perm2 owner2 time2 format2) =
    entryPath entry1 < entryPath entry2

showTarFile :: FilePath -> IO [Lbs.ByteString]
showTarFile path = do
  bs <- Lbs.readFile path
  return $ map showEntry $ listEntries $ Tar.read bs

listEntries :: Tar.Entries Tar.FormatError -> [Tar.Entry]
listEntries (Tar.Next entry es) = entry : listEntries es
listEntries (Tar.Done) = []
listEntries (Tar.Fail e) = error $ show e

showEntry :: Entry -> C8.ByteString
showEntry entry@(Entry _ content perm owner time format)
  = Lbs.concat [showContentType content, showPermissions perm, " ", showOwnership owner, " ", {- showTime entry, " ",-} showPath entry]

showContentType :: Tar.EntryContent -> Lbs.ByteString
showContentType (NormalFile _ _)       = "-"
showContentType (Directory)            = "d"
showContentType (SymbolicLink _)       = "l"
showContentType (HardLink _)           = "l"
showContentType (CharacterDevice _ _)  = "c"
showContentType (BlockDevice _ _)      = "b"
showContentType (NamedPipe)            = "p"
showContentType (OtherEntryType _ _ _) = "?"

showPermissions :: Permissions -> Lbs.ByteString
showPermissions (CMode n) = Lbs.concat $ map rwx $ printf "%03o" n

rwx :: Char -> Lbs.ByteString
rwx '0' = "---"
rwx '1' = "--x"
rwx '2' = "-w-"
rwx '3' = "-wx"
rwx '4' = "-r-"
rwx '5' = "r-x"
rwx '6' = "rw-"
rwx '7' = "rwx"
rwx  c  = error $ "FileMode Over 7: " ++ [c]

instance Show Ownership where
  show (Ownership uname gname uid gid) = printf "%s(%d)/%s(%d)" uname uid gname gid

showOwnership :: Ownership -> Lbs.ByteString
showOwnership (Ownership uname gname uid gid) = C8.pack $ printf "%s(%d)/%s(%d)" uname uid gname gid

showPath :: Tar.Entry -> Lbs.ByteString
showPath = C8.pack . entryPath

showTime :: Tar.Entry -> Lbs.ByteString
showTime = undefined
