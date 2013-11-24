-- Joe Jevnik
-- 24.11.2013
-- polls the server for new messages and acts accordingly.

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative            ((<$>))
import Control.Concurrent             (threadDelay)
import Control.Monad                  (when,unless)
import System.Directory               (getDirectoryContents,removeFile)
import Text.Regex.TDFA                ((=~))

import DoorLock

-- The directory where new messages will appear.
mAIL_DIR :: FilePath
mAIL_DIR = "/home/joejev/maildir/INBOX/new/"

-- Polls every minute to see if any commands need to be dealt with.
poll :: IO ()
poll = do
    msg_fls <- map (mAIL_DIR ++) . filter (`notElem` [".",".."])
               <$> getDirectoryContents mAIL_DIR
    unless (null msg_fls) $ do
                msgs <- mapM readFile msg_fls
                let lock_true  = any (=~ ("lock=true" :: String))  msgs
                    lock_false = any (=~ ("lock=false" :: String)) msgs
                    lock_stat  = any (=~ ("lock_stat" :: String))  msgs
                when lock_true (lock_door >> send_response "door locked!")
                when lock_false (unlock_door >> send_response "door unlocked!")
                when lock_stat query_lock_status
                mapM_ removeFile msg_fls
    threadDelay 500000
    poll
