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

main :: IO ()
main = getArgs >>= parse >> exit

parse ["fetch"]  = fetch >> exit
parse ["status"]  = status >> exit
parse ["upgrade"] = upgrade >> exit
parse ["fetch", "upgrade"] = fetch >> upgrade >> exit
parse []          = usage >> exit

usage = putStrLn "Usage: up <command>"
     >> putStrLn "The commands are:"
     >> putStrLn "fetch\tUpdate ports tree"
     >> putStrLn "status\tShow ports ready to be upgraded"
     >> putStrLn "upgrade\tRun upgrade on ports that don't have entries in UPDATING"
fetch = portsnapFetchUpdate -- TODO what about first-time update?
status = getPackages >>= printPackages
upgrade = getPackages >>= upgradePackages
exit = exitWith ExitSuccess
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

