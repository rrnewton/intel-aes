#!/usr/bin/env runhaskell
import Distribution.Simple

import Distribution.Simple.UserHooks
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit
import System.Directory
import Data.Maybe
import Data.List
import Debug.Trace


main = do
  putStrLn$ "*** Running Setup.hs..."

  defaultMainWithHooks $
   simpleUserHooks 
    { postConf = my_postConf
    , preBuild = my_preBuild
    , buildHook = my_build
    }

--------------------------------------------------------------------------------
-- We override postConf to keep it from complaining about a missing library:
my_postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
my_postConf args conf desc localinfo = do
--  Just desc <- readDesc simpleUserHooks
  let buildinfos = allBuildInfo desc

  putStrLn$ "\n EXTRA libraries: "++ show (map extraLibs buildinfos)
  let desc2 = stripDesc desc
  putStrLn$ "\n LIBS POST STRIP "++ show (map extraLibs $ allBuildInfo desc2)

  -- Now call the default hook with a stripped down version:
  (postConf simpleUserHooks) args conf desc2 localinfo

stripDesc desc = 
       desc { library     = fmap stripLib (library desc) 
  	    , executables = map  stripExe (executables desc)
	    }
stripLib lib = lib { libBuildInfo = stripBI (libBuildInfo lib) }
stripExe exe = exe { buildInfo = stripBI (buildInfo exe) }
stripBI bi   = bi  { extraLibs = filter filt (extraLibs bi) }

-- filt "intel_aes" = False
-- filt _           = True
filt str = not (isInfixOf "intel_aes" str)

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

  -- We add this here because we don't
  -- FIXME: This should actually be the INSTALL PATH, but I haven't successfully extracted it.
  let newflags = flags { buildProgramArgs = 
			  ("ld",["-rpath="++ rootdir ++"/cbits/"]) : (buildProgramArgs flags)
		       }

-- BUILD FLAGS: BuildFlags {buildProgramPaths = [], buildProgramArgs = [], buildDistPref = Flag "dist", buildVerbosity = Flag Normal}
-- BUILD FLAGS: BuildFlags {buildProgramPaths = [], buildProgramArgs = [("ld",["/home/newton/intel-aes/cbits/libintel_aes.a"])], buildDistPref = Flag "dist", buildVerbosity = Flag Deafening}
  (preBuild simpleUserHooks) args flags



my_build :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
my_build desc linfo hooks flags = do 
  putStrLn$ "  [intel-aes] Modyfing the build action to include the .a file..."
  putStrLn$ " \nBUILD FLAGS: "++ show flags ++"\n"
-- Doesn't work, get error "setup: internal error InstallDirs.libsubdir"
--  putStrLn$ "INstall dirs: " ++ show (absoluteInstallDirs desc linfo NoCopyDest)
-- absoluteInstallDirs :: PackageDescription -> LocalBuildInfo -> CopyDest -> InstallDirs FilePathSource

-- Is there another way to get out the install dirs?
-- LocalBuildInfo: 
-- installDirTemplates :: InstallDirTemplates
--     The installation directories for the various differnt kinds of files 


  (buildHook simpleUserHooks) desc linfo hooks flags
  putStrLn$ "  [intel-aes] Build action finished.\n\n"
