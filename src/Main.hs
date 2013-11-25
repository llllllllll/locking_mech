-- Joe Jevnik
-- 24.11.2013
-- polls the server for new messages and acts accordingly.

import Control.Applicative            ((<$>))
import Control.Concurrent             (forkIO,threadDelay)
import Control.Monad                  (when,unless,void)
import Control.Monad.Trans            (liftIO)
import Data.List                      (nub,stripPrefix)
import Data.Maybe                     (fromMaybe,isJust)
import qualified Data.Text as T       (pack)
import qualified Data.Text.Lazy as TL (pack)
import Network.Mail.SMTP              (Address(..),sendMailWithLogin
                                      ,simpleMail,plainTextPart)
import System.Directory               (getDirectoryContents
                                      ,removeFile,getHomeDirectory
                                      ,createDirectoryIfMissing)
import System.Environment             (getArgs)
import System.FilePath                ((</>))
import System.IO                      (hSetEcho,hFlush,stdin,stdout)
import System.Process                 (system)
import Text.Regex.TDFA                ((=~))

-- WARNING: ARDUINO SPECIFIC: PROTOTYPE ONLY
import DoorLock


-- -----------------------------------------------------------------------------
-- data and names

-- A data type to hold all the server's settings during runtime.
data Settings = Settings { smtp_server  :: String
                         , sender_name  :: String
                         , sender_email :: String
                         , sender_pass  :: String
                         , lock_pass    :: String
                         , mail_dir     :: FilePath
                         } deriving Show

-- default location of the config file.
config_fl_def :: IO FilePath
config_fl_def = getHomeDirectory >>= \s -> return (s ++ "/.door_lock/config")

-- -----------------------------------------------------------------------------
-- parsing and initalization

-- Parses a settings file for it's data.
parse_settings :: String -> String -> String -> Settings
parse_settings str server_pass door_pass =
    let ls     = filter (\l -> (not . null) l && head l /= '#') $ lines str
        server = parse_ln "server="   ls
        name   = parse_ln "name="     ls
        email  = parse_ln "email="    ls
        mail   = parse_ln "mail_dir=" ls
    in Settings { smtp_server  = server
                , sender_name  = name
                , sender_email = email
                , sender_pass  = server_pass
                , lock_pass    = door_pass
                , mail_dir     = mail
                }
  where
      parse_ln str ls = dropWhile (== ' ') $ fromMaybe ""
                        $ head $ filter isJust $ map (stripPrefix str) ls

-- Returns the sender of the request, the recipient of the response.
parse_rcpt :: String -> String
parse_rcpt str = (tail . init) $ dropWhile (/= '<') $ str =~ "Return-Path: <.*>"

-- Parses a request and acts accordingly.
handle_req :: Settings -> String -> IO ()
handle_req s req  = do
    if req =~ lock_pass s
      then do
          when (req =~ "lock=true") $
               lock_door >> send_response s (parse_rcpt req) "door locked"
          when (req =~ "lock=false") $
               unlock_door >> send_response s (parse_rcpt req) "door unlocked"
          when (req =~ "lock_stat") $
               query_lock_respond s (parse_rcpt req)
      else
          send_response s (parse_rcpt req) "incorrect password"

-- -----------------------------------------------------------------------------
-- main
main :: IO ()
main = do
    args <- getArgs
    let args' = filter (/= "-s") args
    putStr "Door Password: "
    hFlush stdout
    hSetEcho stdin False
    door_pass <- getLine
    putStr "\nMail Password: "
    server_pass <- getLine
    hSetEcho stdin True
    if any (== "-s") args
      then void $ forkIO $ void $ system "offlineimap"
      else return ()
    putStrLn "\nPolling..."
    if null args'
      then do
          config <- config_fl_def >>= readFile
          let s = parse_settings config server_pass door_pass
          createDirectoryIfMissing True (mail_dir s)
          poll s
      else do
          config <- readFile $ head args'
          let s = parse_settings config server_pass door_pass
          createDirectoryIfMissing True (mail_dir s)
          poll s

-- Polls every second to check for new messages to deal with them.
-- Deletes the messages that it has consumed, even if they are not valid.
poll :: Settings -> IO ()
poll s = do
    msg_fls <- map (mail_dir s </>) . filter (`notElem` [".",".."])
               <$> getDirectoryContents (mail_dir s)
    unless (null msg_fls) $ do
                reqs <- mapM readFile msg_fls
                mapM_ (handle_req s) reqs
                mapM_ removeFile msg_fls
    threadDelay 1000000
    poll s

-- -----------------------------------------------------------------------------
-- mail handling.
-- Polls every minute to see if any commands need to be dealt with.

-- Sends a message response
send_response :: Settings -> String -> String -> IO ()
send_response s rcpt msg = sendMailWithLogin (smtp_server s)
                           username password mail
  where
      mail         = simpleMail from to cc bcc subject [body]
      from         = Address Nothing (T.pack (sender_email s))
      to           = [Address Nothing (T.pack rcpt)]
      cc           = []
      bcc          = []
      subject      = T.pack ""
      body         = plainTextPart $ TL.pack msg
      username     = sender_name s
      password     = sender_pass s

-- -----------------------------------------------------------------------------
-- arduino specific
-- WARNING: PROTOTYPE ONLY

-- Query the arduino to check if the door is locked or not and then send an
-- appropriate response
-- A value greater than to 512 is LOCKED
-- a value less than or equal to 512 is UNLOCKED
query_lock_respond :: Settings -> String -> IO ()
query_lock_respond s rcpt =
    liftIO (putStrLn "querying door")
               >> (withArduino False "/dev/ttyACM0" $ do
                       setPinMode (analog 1) ANALOG
                       n <- analogRead (analog 1)
                       v <- timeOut 10000 (read_val n)
                       let val = case v of
                                     Nothing -> 0
                                     Just a  -> a
                       when (val > 512)  $ liftIO
                                $ send_response s rcpt "door is locked"
                       when (val <= 512) $ liftIO
                                $ send_response s rcpt "door is unlocked")
  where
      read_val v = do
          n <- analogRead (analog 1)
          if n == v
            then read_val n
            else return n
