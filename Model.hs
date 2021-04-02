module Model where

import Tree as T
import Tree (Tree, Polytree)
import Util as U

import Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- Repository state consists of the repository, the active HEAD (a Revision) of the working directory, and a tracking list
type RepositoryState = (Repository, Revision, [File])
-- Repository consists of a unique repository ID, list of all revisions, manifest (FileLog), and a list of FileLog for each file being versioned
type RepositoryID = String
type Repository = (RepositoryID, [Revision], FileLog, [FileLog])
-- Revision is a unique RevisionID and a NodeID pointing to a particular version of the manifest FileLog
type RevisionID = String
type Revision = (RevisionID, NodeID)
-- FileLog is a tree of FileVersion (each node is a FileVersion with two parents)
type FileID = Int
type FileName = String
type FileLog = (FileName, Polytree FileVersion)
-- FileVersion is a unique NodeID together with ByteString contents
type NodeID = Int
type FileContents = ByteString
type FileVersion = (NodeID, FileContents)
-- File represents a file, i.e. FileName and FileContents
type File = (FileName, FileContents)

getFileContents :: File -> ByteString
getFileContents (fname, fcontents) = fcontents

getFileContentsFromList :: FileName -> [File] -> ByteString
getFileContentsFromList fname [] = BS.empty
getFileContentsFromList fname (x:xs) =
  let (fname1, fcontents) = x
  in if fname1 == fname then fcontents else (getFileContentsFromList fname xs)
