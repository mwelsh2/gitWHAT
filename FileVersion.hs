module FileVersion where

import Model as M
import Util as U

-- Creates an empty file version with no parents
createEmptyVersion :: FileVersion
createEmptyVersion = (0, U.emptyByteString)

-- Create a new file version from a bytestring and two parents
createVersion :: FileContents -> (NodeID, NodeID) -> FileVersion
createVersion contents (p1, p2) =
  ((hashNodeID (contents <> (intToByteString p1) <> (intToByteString p2))), contents)

-- Retrieve contents of FileVersion
getVersionContents :: FileVersion -> FileContents
getVersionContents version = contents
  where (id, contents) = version

-- Retrieve NodeID of FileVersion
getVersionNodeID :: FileVersion -> NodeID
getVersionNodeID version = id
  where (id, contents) = version