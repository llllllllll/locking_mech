-- Joe Jevnik
-- 24.11.2013
-- Functions for interacting with the door locking mechanism.

{-# LANGUAGE OverloadedStrings #-}

module DoorLock
    ( lock_door         -- :: IO ()
    , unlock_door       -- :: IO ()
    , query_lock_status -- :: IO ()
    , send_response     -- :: String -> IO ()
    ) where

import Control.Concurrent             (forkIO)
import Control.Monad                  (when,liftM)
import Control.Monad.Trans            (liftIO)
import qualified Data.Text.Lazy as TL (pack)
import Network.Mail.SMTP              (Address(..),sendMailWithLogin
                                      ,simpleMail,plainTextPart)
import System.Hardware.Arduino

-- Lock the door.
-- TODO: Currently flashes a light.
lock_door :: IO ()
lock_door = do
    forkIO $ withArduino False "/dev/ttyACM0" flash
    putStrLn "Locking door!"
  where
      flash = do
          let led = digital 13
              pot = analog 3
          setPinMode pot ANALOG
          analogRead pot
          val <- analogRead pot
          setPinMode   led OUTPUT
          digitalWrite led True
          delay 1000
          digitalWrite led False
          delay 1000

-- Unlock the door.
-- TODO: Currently flashes a light
unlock_door :: IO ()
unlock_door = do
    forkIO $ withArduino False "/dev/ttyACM0" flash
    putStrLn "Unlocking door!"
  where
      flash = do
          let led = digital 13
              pot = analog 3
          setPinMode pot ANALOG
          val <- analogRead pot
          liftIO $ print val
          setPinMode led OUTPUT
          digitalWrite led True
          delay 1000
          digitalWrite led False
          delay 1000

-- Query the arduino to check if the door is locked or not and then send an
-- appropriate response
-- A value greater than to 512 is LOCKED
-- a value less than or equal to 512 is UNLOCKED
query_lock_status :: IO ()
query_lock_status = liftIO (putStrLn "querying door")
                    >> (withArduino False "/dev/ttyACM0" $ do
                            setPinMode (analog 1) ANALOG
                            n <- analogRead (analog 1)
                            v <- timeOut 10000 (read_val n)
                            let val = case v of
                                          Nothing -> 0
                                          Just a  -> a
                            liftIO $ print val
                            when (val > 512)  $ liftIO
                                     $ send_response "door is locked"
                            when (val <= 512) $ liftIO
                                     $ send_response "door is unlocked")
  where
      read_val v = do
          n <- analogRead (analog 1)
          if n == v
            then read_val n
            else return n

-- Sends a message response to my phone.
send_response :: String -> IO ()
send_response msg = sendMailWithLogin "email.wit.edu" username password mail
  where
      mail         = simpleMail from to cc bcc subject [body]
      from         = Address Nothing "jevnikj@wit.edu"
      to           = [Address (Just "Joe Jevnik")  "8602024741@txt.att.net"]
      cc           = []
      bcc          = []
      subject      = ""
      body         = plainTextPart $ TL.pack msg
      username     = "jevnikj@wit.edu"
      password     = "my_pass_true_txt_;_;" -- Deal with this!
