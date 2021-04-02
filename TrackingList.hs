module TrackingList where

import Model as M
import Util as U

-- Initiates an empty tracking list
emptyTrackingList :: [File]
emptyTrackingList = []

-- Loads a file from fname and adds file to the tracking list (if file already exists, read the most recent version)
track :: [File] -> File -> [File]
track f_list file =
  let (fname, _) = file
  in if elem fname [ fname1 | (fname1, _) <- f_list ] then (track (untrack f_list fname) file)
     else file : f_list

-- Removes a file from the tracking list
untrack :: [File] -> FileName -> [File]
untrack f_list fname = filter (\l -> let (fname1, _) = l in fname1 /= fname) f_list