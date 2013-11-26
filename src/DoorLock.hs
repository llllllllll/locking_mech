-- Joe Jevnik
-- 24.11.2013
-- Functions for interacting with the door locking mechanism.

module DoorLock
    ( module System.Hardware.Arduino
    , lock_door    -- :: IO ()
    , unlock_door  -- :: IO ()
    ) where

import Control.Concurrent      (forkIO)
import Control.Monad           (when,liftM)
import Control.Monad.Trans     (liftIO)
import System.Hardware.Arduino
import System.Hardware.Arduino.Parts.Servo

-- Lock the door.
lock_door :: IO ()
lock_door = do
    forkIO $ withArduino False "/dev/ttyACM0" lock
    putStrLn "Locking door!"
  where
      lock = do
          let led = digital 13
          setPinMode led OUTPUT
          servo <- attach (digital 3) Nothing Nothing
          digitalWrite led True
          delay 1000
          setAngle servo 90
          digitalWrite led False


-- Unlock the door.
unlock_door :: IO ()
unlock_door = do
    forkIO $ withArduino False "/dev/ttyACM0" unlock
    putStrLn "Unlocking door!"
  where
      unlock = do
          let led = digital 13
          setPinMode led OUTPUT
          servo <- attach (digital 3) Nothing Nothing
          digitalWrite led True
          delay 1000
          setAngle servo 0
          digitalWrite led False
