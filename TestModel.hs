import Model as M
import Util as U
import View as V
import FileVersion as FV
import FileLog as FL
import Revision as RV
import Repository as RP
import TrackingList as TL

import Data.Map (Map)
import qualified Data.Map as Map

main = do
  -- Track example files
  let trackingList = TL.emptyTrackingList
  let trackingList1 = TL.track trackingList "test/a.txt"
  let trackingList2 = TL.track trackingList1 "test/b.txt"
  -- print trackingList2

  -- Init empty repository
  let repo = RP.initRepository "testRepo"

  -- Get previous revision
  let (_, (lastRev:_), _, _) = repo

  -- Make new revision to repository (after tracking updates in file1 and file2)
  -- Requires previous revision to be provided as an argument
  let repo2 = RV.revise repo "test revision" lastRev trackingList2
  putStrLn ("\n"++(V.printRepositoryVerbose repo2))

  -- Track example files
  let trackingList3 = TL.emptyTrackingList
  let trackingList4 = TL.track trackingList3 "test/a.txt"
  let trackingList5 = TL.track trackingList4 "test/c.txt"
  -- print trackingList5

  -- Get previous revision
  let (_, (lastRev1:_), _, _) = repo2

  -- Make new revision to repository (after tracking updates in file1 and file2)
  -- Requires previous revision to be provided as an argument
  let repo3 = RV.revise repo2 "test revision2" lastRev1 trackingList5
  putStrLn ("\n"++(V.printRepositoryVerbose repo3))