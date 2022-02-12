module Error where

import Text.Read
import Control.Exception
import System.Exit

errorExit :: IO ()
errorExit = exitWith $ ExitFailure 84

successExit :: IO ()
successExit = exitSuccess