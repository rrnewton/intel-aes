#!/usr/bin/env runhaskell
import Distribution.Simple

import Distribution.Simple.UserHooks
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit
import System.IO.Unsafe
import System.Directory
import Data.Maybe
import Data.List
import Data.IORef
import Debug.Trace

main = do
  putStrLn$ "  [intel-aes] Running Setup.hs..."

  defaultMainWithHooks $
   simpleUserHooks 
    { postConf  = my_postConf
    , confHook  = my_conf
    , preBuild  = my_preBuild
    , buildHook = my_build
    , instHook  = my_install
    }

-- global_var_hack = unsafePerformIO (newIORef "UNSETVARIABLE")
-- Even worse, a file will be our global variable:
tmpfile = ".temp.install_dir.txt"


--------------------------------------------------------------------------------
my_conf :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
my_conf (gpd,hbi) flags = do 
  (confHook simpleUserHooks) (gpd,hbi) flags

--------------------------------------------------------------------------------
-- we override postConf to keep it from complaining about a missing library:
my_postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
my_postConf args conf desc localinfo = do
--  Just desc <- readDesc simpleUserHooks
  let buildinfos = allBuildInfo desc

  putStrLn$ "\n  [intel-aes] Extra libraries initially: "++ show (map extraLibs buildinfos)
  let desc2 = stripDesc desc
  putStrLn$ "  [intel-aes] Stripped extra libraries to: "++ show (map extraLibs $ allBuildInfo desc2)

  let libd  = libdir$ absoluteInstallDirs desc localinfo NoCopyDest
  -- This is lame but I'm not sure how to get the same information at the preBuild step.
  -- writeIORef global_var_hack libd
  putStrLn$ "  [intel-aes] Determined install dir to be: " ++ show libd 
  writeFile tmpfile libd
  putStrLn$ "  [intel-aes] Recorded install dir in " ++ show tmpfile

  orig <- readFile "intel-aes.cabal"
  let bak = "intel-aes.cabal.bak."
      findBak n = do
        e <- doesFileExist (bak ++ show n)
	if e then findBak (n+1)
	     else return (bak ++ show n)

  bakname <- findBak 0
  putStrLn$ "  [intel-aes] Backing up old .cabal file as " ++ show bakname
  renameFile "intel-aes.cabal" bakname
  curd <- getCurrentDirectory 
  writeFile "intel-aes.cabal" (unlines$ map (hackLine curd libd) $ lines orig)
  putStrLn$ "  [intel-aes] Rewrote .cabal file to set RPATH."

  -- Now call the default hook with a stripped down version:
  (postConf simpleUserHooks) args conf desc2 localinfo

stripDesc desc = 
       desc { library     = fmap stripLib (library desc) 
  	    , executables = map  stripExe (executables desc)
	    }
stripLib lib = lib { libBuildInfo = stripBI (libBuildInfo lib) }
stripExe exe = exe { buildInfo = stripBI (buildInfo exe) }
stripBI bi   = bi  { extraLibs = filter filt (extraLibs bi) }

filt str = not (isInfixOf "intel_aes" str)

-- DANGER DANGER:
--   ld-options: -Wl,-rpath=/opt/intel-aes/cbits/
hackLine curd libd line 
  --    | isInfixOf "-Wl,-rpath=" line =  "ld-options: -Wl,-rpath=" ++ libd
  --
    -- The extra-lib-dirs is for the benefit of the build process:
    | isInfixOf "extra-lib-dirs: ./cbits/" line =  "  extra-lib-dirs: " ++ curd ++"/cbits/"
    | isInfixOf "ld-options: HACKME"       line =  "  ld-options: -Wl,-rpath=" ++ libd
    | otherwise                                 =  line


------------------------------------------------------------
my_preBuild :: Args -> BuildFlags -> IO HookedBuildInfo
my_preBuild args flags = do 

  putStrLn$ "\n================================================================================"
  putStrLn$ "  [intel-aes] Running Makefile to build C/asm source..."
  rootdir <- getCurrentDirectory 
  setCurrentDirectory "./cbits/"
  system "make"
  setCurrentDirectory rootdir
  putStrLn$ "  [intel-aes] Done with external build job."
  putStrLn$ "================================================================================\n"

  (preBuild simpleUserHooks) args flags


--------------------------------------------------------------------------------
my_build :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
my_build desc linfo hooks flags = do 
  putStrLn$ "  [intel-aes] Build action started."
--  putStrLn$ "  [intel-aes] Modyfing the build action to include the .a file..."
  putStrLn$ " \nBUILD FLAGS: "++ show flags ++"\n"
-- Doesn't work, get error "setup: internal error InstallDirs.libsubdir"
--  putStrLn$ "Install dirs: " ++ show (absoluteInstallDirs desc linfo NoCopyDest)
-- absoluteInstallDirs :: PackageDescription -> LocalBuildInfo -> CopyDest -> InstallDirs FilePathSource

-- Is there another way to get out the install dirs?
-- LocalBuildInfo: 
-- installDirTemplates :: InstallDirTemplates
--     The installation directories for the various differnt kinds of files 

  -- rootdir <- getCurrentDirectory 
  -- We add this here because we can't do it in the .cabal file and have it be variable:
  -- FIXME: This should actually be the INSTALL PATH, but I haven't successfully extracted it.

  -- let newflags = flags { buildProgramArgs = 
  -- 			  ("ld",["-rpath="++ rootdir ++"/cbits/"]) : (buildProgramArgs flags)  }

  (buildHook simpleUserHooks) desc linfo hooks flags
  putStrLn$ "  [intel-aes] Build action finished.\n\n"



--------------------------------------------------------------------------------
my_install :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
my_install desc linfo hooks flags = do 
  putStrLn$ "  [intel-aes] Install action:"
  putStrLn$ "\n================================================================================"
  --libd <- readIORef global_var_hack 
  libd <- readFile tmpfile 
  let dest = (libd ++ "/libintel_aes.so")
  putStrLn$ "  [intel-aes] Copying shared library to: " ++ show dest
  copyFile "./cbits/libintel_aes.so" dest
  putStrLn$ "  [intel-aes] Done copying."
  -- removeFile tmpfile -- Might install more than once, right?
  putStrLn$ "================================================================================\n"
  (instHook simpleUserHooks) desc linfo hooks flags