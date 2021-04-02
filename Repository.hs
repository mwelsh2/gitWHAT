module Repository where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Tree as T

-- Create an empty repository
initRepository :: RepositoryID -> Repository
initRepository id = (id, [(RV.initRevision)], (FL.createFileLog ".manifest"), [])

-- Check if a repository has a given RevisionID
hasRevision :: RevisionID -> Repository -> Bool
hasRevision revId repo =
  let (_, revisions, _, _) = repo
  in elem revId (map fst revisions)

-- Gets a revision from a repository by ID
getRevision :: RevisionID -> Repository -> Revision
getRevision revId repo =
  let (_, revisions, _, _) = repo
  in RV.revisionLookup revId revisions
  
-- Returns all revisions from the repository that are heads (childless)
getHeads :: Repository -> [Revision]
getHeads (_, r, m, _) = concatMap (search r []) r
   where
      search [] heads n = heads ++ [n]
      search (h:t) heads n = 
         if isParentOf n h m then heads
         else search t heads n

-- Returns true if one revision is the parent of the other
isParentOf :: Revision -> Revision -> FileLog -> Bool
isParentOf (_, x) (_, y) (_, t) = 
      case T.getNodeParentsPolytree y FV.getVersionNodeID t of
      [] -> False
      [(p, _)] -> x == p
      [(p1, _), (p2, _)] -> x == p1 || x == p2

-- TODO:
-- store :: Repository -> Revision -> Repository
-- retrieve :: Repository -> Int -> Maybe Revision
