module Main where

import System.Environment
import System.Process
import System.Exit

import Pkg

main :: IO ()
main = getArgs >>= parse >> exit

parse ["update"]  = update >> exit
parse ["status"]  = status >> exit
parse ["upgrade"] = upgrade >> exit
parse []          = usage >> exit

usage = putStrLn "TODO usage"
update = putStrLn "TODO update"
status = getPackages >>= printPackages
upgrade = putStrLn "TODO upgrade"
exit = exitWith ExitSuccess

{-|Print two list of packages: `automatic' and `manual'
  |-}
printPackages :: ([String], [String]) -> IO ()
printPackages (as, ms) = printList "Ready to be updated:" as >>
                         printList "\nThese seem to have entries in UPDATING, please check them manually:" ms

{-|Conditionally print a list with a message: if the list is not empty, print
  | the message and the the list elements
  |-}
printList :: String -> [String] -> IO ()
printList message (x:xs) = (putStrLn message >> mapM_ putStrLn (x:xs))
printList _ []           = return ()

