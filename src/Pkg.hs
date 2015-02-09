{- |
Module      :  $Header$
License     :  FreeBSD

Maintainer  :  penzin.dev@gmail.com
Stability   :  experimental
Portability :  non-portable (FreeBSD specific)

Primitives to work with ports and packages

-}
module Pkg (getPackages, portsnapFetchUpdate, upgradeSoftware) where

import System.Environment
import System.Process
import System.Exit
import Config.FreeBSD.Package
import Config.FreeBSD.PortTools

{-|Get two lists of package names: ones that can be updated automatically and
  | ones that need manual intervention
  |-}
getPackages :: IO ([String], [String])
getPackages = getOutdatedPackagesSmart
          >>= \ps -> separatePackages ps [] []

{-|Separate packages that have entries in UPDATING from the ones that don't
  | takes list of packages to separate, and initial lists for `green' and manual packages
  |-}
separatePackages :: [String] -> [String] -> [String] -> IO ([String], [String])
separatePackages (x:xs) as ms = isReadyToUpdate x 
                            >>= \r -> if r then (separatePackages xs (x:as) ms) else (separatePackages xs as (x:ms))
separatePackages [] as ms = return (as, ms)

{-|Check if a package is ready to update (doesn't have entries in UPDATING) -}
isReadyToUpdate :: String -> IO Bool
isReadyToUpdate p = readProcessWithExitCode "grep" ["-w", p, "/usr/ports/UPDATING"] ""
                >>= \(exitCode, _, _) -> if (exitCode == ExitSuccess) then return False else return True

{-|Choose one's upgrade tool. Choices are in the following order
  * Portmaster
  * Portupgrade
  * Make
  |-}
chooseUpgradeTool :: IO (String -> IO())
chooseUpgradeTool = isPortmasterPresent
               >>= \pm -> if pm then return (upgradeWithPortmaster) else isPkgToolsPresent
               >>= \pu -> if pu then return (upgradeWithPortupgrade) else return (upgradeWithMake)

{-|Upgrade a list of ports
  |-}
upgradeSoftware :: [String] -> IO ()
upgradeSoftware (p:ps) = chooseUpgradeTool
                     >>= \t ->runUpgrade t (p:ps)
                      >> putStrLn ("Upgraded " ++ (show $ length (p:ps)) ++ " packages")
upgradeSoftware [] = putStrLn "Nothing to upgrade in automatic mode"

{-|Run upgrade commands
  |-}
runUpgrade :: (String -> IO ()) -> [String] -> IO ()
runUpgrade tool (p:ps) = putStrLn ("Upgrading " ++ p  ++ ":")
                      >> tool p
                      >> runUpgrade tool ps
runUpgrade _ [] = return ()

