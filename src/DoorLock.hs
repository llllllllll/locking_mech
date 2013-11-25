-- Joe Jevnik
-- 24.11.2013
-- Functions for interacting with the door locking mechanism.

{-# LANGUAGE OverloadedStrings #-}

module DoorLock
    ( module System.Hardware.Arduino
    , lock_door         -- :: IO ()
    , unlock_door       -- :: IO ()
    ) where

import Control.Concurrent      (forkIO)
import Control.Monad           (when,liftM)
import Control.Monad.Trans     (liftIO)
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
          setPinMode led OUTPUT
          digitalWrite led True
          delay 1000
          digitalWrite led False
          delay 1000
