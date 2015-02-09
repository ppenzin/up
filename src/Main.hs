{- |
Module      :  $Header$
License     :  FreeBSD

Maintainer  :  penzin.dev@gmail.com
Stability   :  experimental
Portability :  non-portable (FreeBSD specific)

Entry point for the up application

-}
module Main where

import System.Environment
import System.Process
import System.Exit

import Pkg

-- |Entry point function
main :: IO ()
main = getArgs >>= parse >> exit

-- |Parse command-line arguments
parse ["status"]  = status >> exit
parse ["upgrade"] = upgrade >> exit
parse ["help"]    = usage >> exit
parse []          = usage >> exit

-- |Display help
usage = putStrLn "Usage: up <command>"
     >> putStrLn "The commands are:"
     >> putStrLn "status\tShow ports ready to be upgraded"
     >> putStrLn "upgrade\tRun upgrade on ports that don't have entries in UPDATING"
     >> putStrLn "\nup relies on ports tree being up-to-date. Run portsnap to update."
-- |Check for outdated packages
status = getPackages >>= printPackages
-- |Upgrade packages
upgrade = getPackages >>= upgradePackages
-- |Exhit with 0 status
exit = exitWith ExitSuccess
-- |Message to display for ports that can't be upgraded automatically
upgradeMsg = "\nThese seem to have entries in UPDATING, please check them manually:"

{-|Print two list of packages: `automatic' and `manual'
  |-}
printPackages :: ([String], [String]) -> IO ()
printPackages (as, ms) = printList "Ready to be updated:" as >>
                         printList upgradeMsg ms

{-|Upgrade `automatic' list, print `manual' list out
  |-}
upgradePackages :: ([String], [String]) -> IO ()
upgradePackages (as, ms) = upgradeSoftware as >>
                           printList upgradeMsg ms

{-|Conditionally print a list with a message: if the list is not empty, print
  | the message and the the list elements
  |-}
printList :: String -> [String] -> IO ()
printList message (x:xs) = (putStrLn message >> mapM_ putStrLn (x:xs))
printList _ []           = return ()

