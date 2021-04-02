module FileLog where

import Tree as T
import Model as M
import FileVersion as FV

-- Create an empty FileLog
createFileLog :: FileName -> FileLog
createFileLog fname = (fname, T.createPolytreeWithNode (FV.createEmptyVersion))

-- Retrieve a version from a FileLog by NodeID
-- Note: version is assumed to exist; if not this creates and
--       returns a new empty FileVersion.
getVersion :: FileLog -> NodeID -> FileVersion
getVersion log id =
  let (_, tree) = log
  in case (T.getNodeKeyPolytree id FV.getVersionNodeID tree) of
    Just a -> a
    Nothing -> (FV.createEmptyVersion)

-- Adds a version 
addVersion :: FileLog -> FileVersion -> NodeID -> Maybe NodeID -> FileLog
addVersion log version p1 p2Maybe =
  let (fname, tree) = log
      tree2Maybe = case p2Maybe of
        Just p2 -> T.addTwoParentsKeyPolytree version p1 p2 FV.getVersionNodeID tree
        Nothing -> T.addOneParentKeyPolytree version p1 FV.getVersionNodeID tree
      tree2 = case tree2Maybe of
        Just a -> a
        Nothing -> T.createPolytreeWithNode version
  in (fname, tree2)

-- Looks up a FileLog in a list of FileLogs, or creates a new FileLog if it doesn't exist
fileLogLookup :: FileName -> [FileLog] -> FileLog
fileLogLookup fname [] = createFileLog fname
fileLogLookup fname (x:xs) =
  let (fname1, _) = x
  in if fname1 == fname then x else (fileLogLookup fname xs)