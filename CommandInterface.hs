module CommandInterface where

import System.Environment
import Text.Read
import Data.List
import qualified Data.Map as Map

import Model as M
import Util as U
import Tree as T
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import TrackingList as TL
import qualified View as V

-- Parses input into command and list of additional parameters, based on spaces in input
parseInput :: String -> (String, [String])
parseInput input =
   let words = U.splitString input ' '
   in (head words, tail words)

-- Ensures that target repository exists in repoStates
repoExists :: String -> [RepositoryState] -> Bool
repoExists repo repoStates =
   let repoIds = [ repoId | ((repoId, _, _, _), _, _) <- repoStates ]
   in elem repo repoIds

-- Ensures that target repository has given revision
revisionExistsInRepo :: String -> RepositoryState -> Bool
revisionExistsInRepo revId repoState = 
   let (repo, _, _) = repoState
   in RP.hasRevision revId repo

-- Applies a command to a particular RepositoryState, returning a new list of RepositoryState
-- with the modified version.
applyToRepoState :: (RepositoryState -> RepositoryState) -> String -> [RepositoryState] -> [RepositoryState]
applyToRepoState _ _ [] = []
applyToRepoState f repo (x:xs) =
   let ((repoId, _, _, _), _, _) = x
   in if repo == repoId then ((f x):xs)
      else (x:(applyToRepoState f repo xs))

-- Retrieves a particular RepositoryState with a given name.
getRepoState :: String -> [RepositoryState] -> RepositoryState
getRepoState _ (x:[]) = x
getRepoState repo (x:xs) =
   let ((repoId, _, _, _), _, _) = x
   in if repo == repoId then x else (getRepoState repo xs)

-- Tests to see if the parameters for a given command are correct and valid.
-- NOTE: if command is not supported at all, this is caught elsewhere.
testForValidityOfCommand :: String -> [String] -> [RepositoryState] -> (Bool, String)
testForValidityOfCommand command params repoStates
   -- init <repoName>
   | (command == "init") && ((length params) /= 1) = (False, "init must be called as 'init <repoName>'.")
   | (command == "init") && (repoExists (params !! 0) repoStates) = (False, (params !! 0)++" already exists.")
   -- clone <newRepoName> <oldRepoName>
   | (command == "clone") && ((length params) /= 2) = (False, "init must be called as 'clone <repoName> <repoToClone>'.")
   | (command == "clone") && (repoExists (params !! 0) repoStates) = (False, (params !! 0)++" already exists.")
   | (command == "clone") && (not (repoExists (params !! 1) repoStates)) = (False, (params !! 1)++" does not exist.")
   -- add <repoName> <filePath1> [filePath2] [filePath3] ...
   | (command == "add") && ((length params) < 2) = (False, "add must be called as 'add <repoName> <filePath1> [filePath2] [filePath3] ...'")
   | (command == "add") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   | (command == "add") && (not (all U.fileExists (map (\fname -> (params !! 0)++"/"++fname) (tail params)))) =
      (False, "File does not exist. Make sure paths are correct.")
   -- remove <repoName> <filePath1> [filePath2] [filePath3] ...
   | (command == "remove") && ((length params) < 2) = (False, "remove must be called as 'remove <repoName> <filePath1> [filePath2] [filePath3] ...'")
   | (command == "remove") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   -- status <repoName>
   | (command == "status") && (((length params) < 1) || ((length params) > 2)) = (False, "status must be called as 'status <repoName> [-v]'")
   | (command == "status") && ((length params) == 2) && ((params !! 1) /= "-v") = (False, "status must be called as 'status <repoName> [-v]'")
   | (command == "status") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   -- heads <repoName> [-a]
   | (command == "heads") && (((length params) < 1) || ((length params) > 2)) = (False, "heads must be called as 'heads <repoName> [-a]'")
   | (command == "heads") && ((length params) == 2) && ((params !! 1) /= "-a") = (False, "heads must be called as 'heads <repoName> [-a]'")
   | (command == "heads") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   -- diff <repoName> <revID1> <revID2> [filePath]
   -- TBC
   -- cat <repoName> <revID> <filePath>
   | (command == "cat") && ((length params) /= 3) = (False, "cat must be called as 'cat <repoName> <revID> <filePath>'")
   | (command == "cat") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   | (command == "cat") && (not (revisionExistsInRepo (params !! 1) (getRepoState (params !! 0) repoStates))) =
      (False, "Revision "++(params !! 1)++" does not exist in "++(params !! 0)++".")
   -- checkout <repoName> <revID>
   | (command == "checkout") && ((length params) /= 2) = (False, "checkout must be called as 'checkout <repoName> <revID>'")
   | (command == "checkout") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   | (command == "checkout") && (not (revisionExistsInRepo (params !! 1) (getRepoState (params !! 0) repoStates))) =
      (False, "Revision "++(params !! 1)++" does not exist in "++(params !! 0)++".")
   -- commit <repoName> <revID>
   | (command == "commit") && ((length params) /= 2) = (False, "commit must be called as 'commit <repoName> <revID>'")
   | (command == "commit") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   -- log <repoName> [-v]
   | (command == "log") && (((length params) < 1) || ((length params) > 2)) = (False, "log must be called as 'log <repoName> [-v]'")
   | (command == "log") && ((length params) == 2) && ((params !! 1) /= "-v") = (False, "log must be called as 'log <repoName> [-v]'")
   | (command == "log") && (not (repoExists (params !! 0) repoStates)) = (False, "Repo "++(params !! 0)++" does not exist.")
   -- merge <repoName> <revID1> <revID2>
   -- TBC
   -- pull <repoName1> <repoName2>
   -- TBC
   -- push <repoName1> <repoName2>
   -- TBC
   | otherwise = (True, "")

-- Evaluates a command with a list of params, returning an informative message (either an
-- error or a view of the repository for a given command) and updated repository states
evalCommand :: String -> [String] -> [RepositoryState] -> (String, [RepositoryState])
evalCommand command params repoStates =
   let (validity, errorMsg) = testForValidityOfCommand command params repoStates
   in if validity == False then (errorMsg, repoStates)
      else executeCommand command params repoStates

-- Executes a command once validity of command has been verified. Returns an informative
-- message and updated repository states
executeCommand :: String -> [String] -> [RepositoryState] -> (String, [RepositoryState])
executeCommand "init" params repoStates = init_ (params !! 0) repoStates
executeCommand _ _ [] = ("First command must be 'init <repoName>'.", [])
executeCommand "repos" _ repoStates = (printRepos repoStates, repoStates)
executeCommand "clone" params repoStates = ("TBC", repoStates)
executeCommand "add" params repoStates = (add repoStates (params !! 0) (tail params))
executeCommand "remove" params repoStates = ("Tracking list updated.", (applyToRepoState (remove (tail params)) (params !! 0) repoStates))
executeCommand "status" params repoStates = ((status (getRepoState (params !! 0) repoStates) (tail params)), repoStates)
executeCommand "heads" params repoStates = ((heads (getRepoState (params !! 0) repoStates) (tail params)), repoStates)
executeCommand "diff" params repoStates = ("TBC", repoStates)
executeCommand "cat" params repoStates = (cat (getRepoState (params !! 0) repoStates) (params !! 1) (params !! 2), repoStates)
executeCommand "checkout" params repoStates = (checkout repoStates (params !! 0) (params !! 1))
executeCommand "commit" params repoStates = ("Commit successful.", (applyToRepoState (commit (params !! 1)) (params !! 0) repoStates))
executeCommand "log" params repoStates = ((log_ (getRepoState (params !! 0) repoStates) (tail params)), repoStates)
executeCommand "merge" params repoStates = ("TBC", repoStates)
executeCommand "pull" params repoStates = ("TBC", repoStates)
executeCommand "push" params repoStates = ("TBC", repoStates)
executeCommand _ _ repoStates = ("I did not understand that command.", repoStates)

------------------------------------------------------------------------------------
-- FUNCTIONS FOR COMMANDS DEFINED BELOW
-- TODO: if we want to be really modular, these can be moved into a separate
--       file/module, but it's not hugely important.
------------------------------------------------------------------------------------

-- Creates a repository
init_ :: RepositoryID -> [RepositoryState] -> (String, [RepositoryState])
init_ name repoStates =
   let message = seq (U.ensureDirectoryExists name) "Initializing new repository..."
   in (message, initRepo name repoStates)

-- Initializes an empty repository
initRepo :: RepositoryID -> [RepositoryState] -> [RepositoryState]
initRepo name repoStates =
   let repoNew = RP.initRepository name
       (_, revs, _, _) = repoNew
   in (repoNew, (head revs), []) : repoStates

-- Prints the IDs of all repositories in repoStates
printRepos :: [RepositoryState] -> String
printRepos [] = ""
printRepos (x:xs) =
   let ((repoID, _, _, _), _, _) = x
   in "* " ++ repoID ++ "\n" ++ (printRepos xs)
 
-- Clones a repository given it's repository Id and the list of repositories it
-- is stored in, returns a new list of repositories with the clone
-- TODO: update so RepositoryState is used rather than Repository

-- --note to self while working : it's going to have to make a new repo (empty)
-- --then add a NEW COPY ITS OWN of the filelog, then get the head of the revision
-- --from the actual repostate in the new clone
-- clone :: [RepositoryState] -> RepositoryID -> Maybe [RepositoryState]
-- clone [] _ = Nothing
-- clone repoStates id = newRepo repoStates rev [] id where
--    newRepo repoStates rev tracks id = case repoStates of
--       [] -> Nothing
--       Just (newRep, copyrevs ) where
--          newRep = initRepo ("cloned" ++ id, repoStates) --making new empty repo.. ask ben about id part
--          copyrevhead = 





-- clone repoStates id = search repoStates [] id where
--    search repoStates visited id = case repoStates of 
--       [] -> Nothing
--       (repo_id, revs, flog, flog_list) : t -> if repo_id == id then  
--           Just (visited ++ [(repo_id, revs, flog, flog_list)] ++ t ++ [(new_id ++ "-1", revs, flog, flog_list)]) else
--           search t (visited ++ [(repo_id, revs, flog, flog_list)]) id 
--           where
--             (new_id, _, _, _) = last t

-- Adds files to tracking list given a list of file paths
add :: [RepositoryState] -> String -> [String] -> (String, [RepositoryState])
add repoStates repoId fnames =
   let repoState = getRepoState repoId repoStates
       (txt, files) = fetchFiles repoId fnames
       message = ("Tracking list updated."++txt)
   in (message, (applyToRepoState (addFiles files) repoId repoStates))

-- Reads files into memory
-- NOTE: string return necessary to force IO to happen (somewhat hacky approach)
fetchFiles :: String -> [FileName] -> (String, [File])
fetchFiles repoId fnames = foldr (fetchFile repoId) ("", []) fnames

-- Reads a file into memory
-- NOTE: string return necessary to force IO to happen (somewhat hacky approach)
fetchFile :: String -> FileName -> (String, [File]) -> (String, [File])
fetchFile repoId fname (acc_s, acc_f) =
   let file = (fname, U.loadFile (repoId++"/"++fname))
   in (acc_s++(U.clearString (U.byteStringToStr (snd file))), (file : acc_f))

-- Adds a list of files to the tracking list, given a particular repository state
addFiles :: [File] -> RepositoryState -> RepositoryState
addFiles files repoState =
   let (repo, hd, trackingList) = repoState
   in (repo, hd, foldl TL.track trackingList files)


-- Removes a list of files from the tracking list, given a particular repository state
remove :: [FileName] -> RepositoryState -> RepositoryState
remove fnames repoState =
   let (repo, hd, trackingList) = repoState
   in (repo, hd, foldl TL.untrack trackingList fnames)


-- Gets status of tracking list given a repository state (and possibly a -v flag for verbose)
status :: RepositoryState -> [String] -> String
status repoState flags = 
   let (repo, hd, trackingList) = repoState
   in if flags == []
      then V.printTrackingList trackingList
      else V.printTrackingListVerbose trackingList

-- Print heads (with an -a flag to only print the active head)
heads :: RepositoryState -> [String] -> String
heads repoState flags =
   let (repo, hd, _) = repoState
   in if flags == []
      then intercalate "\n" (map V.printRevision (RP.getHeads repo))
      else V.printRevision hd

-- TBC

-- diff :: RepositoryState -> RevisionID -> RevisionID -> String
-- diff revis1 revis2 = 
--  if revis1 RevisionID == revis2 RevisionID then "difference not detected"
--  else "difference detected"
-- TBC

-- Print contents of the given filename stored in the given repository
cat :: RepositoryState -> RevisionID -> FileName -> String
cat (repo, _, _) revId fn = 
   let (_, revisions, man, logs) = repo
       manId = snd (RV.revisionLookup revId revisions)
       manMap = RV.manifestToMap man manId
       nodeId = Map.lookup fn manMap
   in 
      case nodeId of
         Nothing -> "The file was not found. Make sure file name is correct."
         Just x ->  show (FV.getVersionContents (FL.getVersion (FL.fileLogLookup fn logs) x))

-- Two steps: 1. write files and generate string, 2. apply to repo states and update active head
checkout :: [RepositoryState] -> String -> RevisionID -> (String, [RepositoryState])
checkout repoStates repoId revId = 
   let repoState = getRepoState repoId repoStates
       message = ("Checked out revision "++(updateWorkingDirectory repoState repoId revId)++revId)
   in (message, (applyToRepoState (updateActiveHead revId) repoId repoStates))

-- Step (1) of checkout; write files to corresponding directory.
-- NOTE: string return necessary to force IO to happen (somewhat hacky approach)
updateWorkingDirectory :: RepositoryState -> String -> RevisionID -> String
updateWorkingDirectory repoState repoId revId = 
   let (repo, _, _) = repoState
   in (seq (U.removeDirectoryContents repoId) (RV.dumpRevisionFiles repo revId))

-- Step (2) of checkout; return a new RepositoryState with an updated head
updateActiveHead :: RevisionID -> RepositoryState -> RepositoryState
updateActiveHead revId repoState =
   let (repo, hd, _) = repoState
   in (repo, (RP.getRevision revId repo), [])   
   

-- Commits files in the tracking list to a new revision
commit :: RevisionID -> RepositoryState -> RepositoryState
commit revId repoState =
   let (repo, hd, trackingList) = repoState
       repoNew = RV.revise repo revId hd trackingList
       (_, revs, _, _) = repoNew
   in (repoNew, (head revs), [])
      

-- Logs revisions in the repository
log_ :: RepositoryState -> [String] -> String
log_ repoState flags =
   let (repo, hd, trackingList) = repoState
   in if flags == []
      then V.printRepository repo
      else V.printRepositoryVerbose repo


-- merge :: RevisionID -> RevisionID -> RepositoryState -> RepositoryState
-- TBC

-- pull :: RepositoryState -> RepositoryState -> RepositoryState
-- TBC

-- push :: RepositoryState -> RepositoryState -> RepositoryState
-- TBC
