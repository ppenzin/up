module Pkg (getPackages, upgradeSoftware) where

import System.Environment
import System.Process
import System.Exit

{-|Get two lists of package names: ones that can be updated automatically and
  | ones that need manual intervention
  |-}
getPackages :: IO ([String], [String])
-- XXX needs support for older package system
getPackages = readProcess "pkg" ["version", "-l", "<"] []
          >>= \s -> separatePackages (pkgNames s) [] []

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

{-|Strip package names from pkg(ng) output -}
pkgNames :: String -> [String]
pkgNames = scrape . lines

{-|Scrape pkg metadata to get package names -}
scrape :: [String] -> [String]
scrape [] = []
scrape (x:xs) = (scrapeOne x):(scrape xs)
    where scrapeOne = reverse . tail . dropWhile ((/=) '-') . reverse

{-|Upgrade a list of packages
  |TODO support installation mechanisms other than portmaster
  |-}
upgradeSoftware :: [String] -> IO ()
upgradeSoftware (p:ps) = runUpgrade (p:ps) >> putStrLn ("Upgraded " ++ (show $ length (p:ps)) ++ " packages")
upgradeSoftware [] = putStrLn "Nothing to upgrade in automatic mode"

{-|Run upgrade commands
  |TODO support installation mechanisms other than portmaster
  |-}
runUpgrade :: [String] -> IO ()
runUpgrade (p:ps) = putStrLn ("Upgrading " ++ p  ++ ":")
                 >> callProcess "portmaster" [p]
                 >> runUpgrade ps
runUpgrade [] = return ()
