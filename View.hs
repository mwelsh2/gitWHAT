module View where

import Model as M
import Util as U
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import TrackingList as TL
import Tree as T

-- -- Given a Repository, prints the manifest file log
-- status :: Repository -> [IO ()]
-- status (repo_id, _, flog, _) = fmap putStrLn [show repo_id, printFileLog flog]
--  where
--   printFileLog flog = 
--    let (_, tree) = flog in printTree tree

-- Converts TrackingList to a string
printTrackingList :: [File] -> String
printTrackingList files = foldl (\acc v -> acc++"* "++(fst v)++"\n") "Tracked modifications:\n" files

-- Converts TrackingList to a string (printing file contents as well)
printTrackingListVerbose :: [File] -> String
printTrackingListVerbose files = foldl (\acc v -> acc++"* "++(fst v)++"\n"++(U.byteStringToStr (snd v))++"\n") "Tracked modifications:\n" files

-- Converts a FileLog to a string
printFileLog :: FileLog -> String
printFileLog log =
  let (fname, tree) = log
  in fname ++ " :\n" ++ (T.printPolytree tree)

-- Convert a revision to a string
printRevision :: Revision -> String
printRevision rev = 
  let (revID, nodeID) = rev
  in "* revision " ++ (show nodeID) ++ "\n   - " ++ revID

-- Convert a repository to a string, showing log of revisions
printRepository :: Repository -> String
printRepository repo =
  let (repoID, revisions, manifest, logs) = repo
  in repoID ++ " ::\n" ++ (U.joinString "\n" (map printRevision revisions))

-- Convert a repository to a string (printing FileLog information as well)
printRepositoryVerbose :: Repository -> String
printRepositoryVerbose repo =
  let (repoID, revisions, manifest, logs) = repo
  in (printRepository repo) ++ "\n" ++ createSep ++ (U.joinString createSep (map printFileLog ([manifest]++logs)))