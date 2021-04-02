module Revision where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- Creates an empty revision
initRevision :: Revision
initRevision = ("init", 0)

-- Takes a repository, previous revision (RevisionID + NodeID of manifest) and
-- a list of files, and returns a new repository with a new revision.
revise :: Repository -> RevisionID -> Revision -> [File] -> Repository
revise repo revId1 rev files =
  let (repoId, revs, man, logs) = repo
      (revId, manId) = rev
      fnames = map fst files
      -- Make sure all files in 'files' are logged, creating new FileLogs if not
      loggedFiles = map fst logs
      logs1 = map (\fname -> FL.fileLogLookup fname logs) (U.removeDuplicates (fnames ++ loggedFiles))
      -- Translate manifest version to a map of (FileName, NodeID) pairs, adding new ones for newly tracked files
      manMap = nodeIDListToMap [ (fname, 0) | fname <- fnames, notElem fname loggedFiles ] (manifestToMap man manId)
      -- For each log in the revision set, create a new version and add to log given parent ID, recording new NodeID
      logsAndNodeIdsNew = map (updateLog manMap files) logs1
      logsNew = map fst logsAndNodeIdsNew
      manMapNew = map snd logsAndNodeIdsNew
      -- Create new manifest version with manId as parent NodeID and new nodeIds as
      manVersionNew = FV.createVersion (nodeIDListToContents manMapNew) (manId, 0)
      manIdNew = FV.getVersionNodeID manVersionNew
      manNew = FL.addVersion man manVersionNew manId Nothing
  -- Return updated repository
  in (repoId, ((revId1, manIdNew):revs), manNew, logsNew)


-- Given a mapping of FileNames to NodeIDs (of previous revision), list of Files to be updated, and a FileLog,
-- update the FileLog and return it in a tuple with the new (FileName, NodeID) pair.
-- If file is not in list of files to add, logNew = log and nodeIdNew = parentId
updateLog :: Map FileName NodeID -> [File] -> FileLog -> (FileLog, (FileName, NodeID))
updateLog manMap files log =
  let (fname, _) = log
      fnames = map fst files
      parentId = nodeIDFromMap fname manMap
  in if elem fname fnames
     then
       let contents = M.getFileContentsFromList fname files
           versionNew = FV.createVersion contents (parentId, 0)
           nodeIdNew = FV.getVersionNodeID versionNew
           logNew = FL.addVersion log versionNew parentId Nothing
       in (logNew, (fname, nodeIdNew))
     else (log, (fname, parentId))

-- Given a manifest file and NodeID of the manifest revision, create mapping from FileName to NodeID
manifestToMap :: FileLog -> NodeID -> Map FileName NodeID
manifestToMap man manId = 
  let manContents = getVersionContents (FL.getVersion man manId)
  in (nodeIDMapFromManifest manContents)

-- Given contents of manifest file, parse into a hash map of FileName to NodeID
-- manifest file contents are of the form "fname1 id1,fname2 id2,..."
nodeIDMapFromManifest :: FileContents -> Map FileName NodeID
nodeIDMapFromManifest bstr =
  let str = byteStringToStr bstr
      lst = map (\l -> let (fname:xs) = (words l)
                           (id:_) = xs
                       in (fname, U.strToInt id))
            (U.splitString str ',')
  in nodeIDListToMap lst Map.empty

-- Takes a list of (FileName, NodeID) tuples and hash map, and recursively fills in the hash map
nodeIDListToMap :: [(FileName, NodeID)] -> Map FileName NodeID -> Map FileName NodeID
nodeIDListToMap [] map = map
nodeIDListToMap (x:xs) map =
  let (fname, id) = x
  in Map.insert fname id (nodeIDListToMap xs map)

-- Gets a NodeID from a (FileName, NodeID) map, or returns 0 if it doesn't exist
nodeIDFromMap :: FileName -> Map FileName NodeID -> NodeID
nodeIDFromMap fname map = case (Map.lookup fname map) of
  Just a -> a
  Nothing -> 0

-- Given a list of (FileName, NodeID) tuples, convert to string of form "fname1 id1,fname2 id2,..."
nodeIDListToContents :: [(FileName, NodeID)] -> FileContents
nodeIDListToContents lst = U.strToByteString (U.joinString "," (map (\l -> let (a,b) = l in a++" "++(show b)) lst))

-- Finds a Revision in a list of Revisions by RevisionID (returning the last one if it doesn't exist)
revisionLookup :: RevisionID -> [Revision] -> Revision
revisionLookup _ (x:[]) = x
revisionLookup revId (x:xs) = if (fst x) == revId then x else (revisionLookup revId xs)

-- Dumps all files in a particular version for a repository to the corresponding directory
-- NOTE: string output required to force IO
dumpRevisionFiles :: Repository -> RevisionID -> String
dumpRevisionFiles repo revId =
  let (repoId, revisions, man, logs) = repo
      manId = snd (revisionLookup revId revisions)
      manMap = manifestToMap man manId
  in foldr (dumpRevisionFile repoId logs) "" (Map.toList manMap)

-- Dumps a particular file to the corresponding directory
-- NOTE: string output required to force IO
dumpRevisionFile :: String -> [FileLog] -> (FileName, NodeID) -> String -> String
dumpRevisionFile repoId logs (fname, nodeId) acc =
  let fcontent = getVersionContents (FL.getVersion (FL.fileLogLookup fname logs) nodeId)
      fpath = repoId++"/"++fname
  in acc ++ (seq (seq (U.ensurePathExists fpath) (U.dumpFile fpath fcontent)) "")


-- TODO:
-- merge :: Revision -> Revision -> Maybe Revision



