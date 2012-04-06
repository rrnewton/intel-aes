#!/usr/bin/env runhaskell
import Distribution.Simple

-- It has proven very difficult to package this library given that it
-- depends on assembly code files that we are compiling into a shared
-- library.

import Distribution.Simple.UserHooks
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit
import qualified System.Info as Info
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
    , cleanHook = my_clean
    }

-- A file will be our global variable:
tmpfile = ".temp.install_dir.txt"

--------------------------------------------------------------------------------
my_clean :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
my_clean desc () hooks flags = do 
  putStrLn$ "\n  [intel-aes] Running external clean (in cbits/) via Makefile"
  setCurrentDirectory "./cbits/"
  system "make clean"
  setCurrentDirectory ".."
  system "rm -f benchmark-intel-aes-rng"
-- Having a problem with this: 
--   Error while removing dist/: dist/setup: removeDirectory: unsatisified constraints (Directory not empty)
-- Disabling:
--  putStrLn$ "  [intel-aes] Done.  Now running normal cabal clean action.\n"
--  (cleanHook simpleUserHooks) desc () hooks flags
  putStrLn$ "  [intel-aes] Done.  Now removing dist/ directory.\n"  
  system "rm -rf dist"
  return ()

--------------------------------------------------------------------------------
my_conf :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
my_conf (gpd,hbi) flags = do 
  (confHook simpleUserHooks) (gpd,hbi) flags

--------------------------------------------------------------------------------
-- Work-around:
-- we override postConf to keep it from complaining about a missing library:
my_postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
my_postConf args conf desc localinfo = do
  let buildinfos = allBuildInfo desc
      desc2 = stripDesc desc
  putStrLn$ "  [intel-aes] Extra libraries initially: "++ show (map extraLibs buildinfos)
  putStrLn$ "  [intel-aes] Stripped extra libraries to: "++ show (map extraLibs $ allBuildInfo desc2)

  desc3 <- patchDesc desc2 localinfo
  -- Now call the default hook with a stripped down version:
  (postConf simpleUserHooks) args conf desc3 localinfo


--------------------------------------------------------------------------------
-- Patch a package description for our quirky build:
patchDesc desc localinfo = do
  let libd  = libdir$ absoluteInstallDirs desc localinfo NoCopyDest
  -- This is lame but I'm not sure how to get the same information at the preBuild step.
  -- writeIORef global_var_hack libd
  putStrLn$ "  [intel-aes] Determined install dir to be: " ++ show libd 
  writeFile tmpfile libd
  putStrLn$ "  [intel-aes] Recorded install dir in " ++ show tmpfile
 
  -- Originally I patched the .cabal itself.
  -- But the following is better.  It changes the options in-memory:
  root <- getCurrentDirectory
  let Just lib = library desc
      lbi  = libBuildInfo lib 
      oldO = ldOptions  lbi
      -- I'm not sure what the best policy is.  For now I'm adding
      -- BOTH the build dir and the eventual install dir...
      -- This will allow the package to function when it is built-but-not-installed.
      newO = ("-Wl,-rpath," ++ libd) :
	     ("-Wl,-rpath," ++ cbitsd) :
	     oldO
      cbitsd = root++"/cbits/"
      newlbi = lbi { ldOptions = newO 
		   , extraLibDirs = cbitsd : extraLibDirs lbi }
      -- Whew... nested record updates are painful:
      desc3 = desc { library = Just (lib { libBuildInfo = newlbi})}

  putStrLn$ "  [intel-aes] Modified package description structure with extra options. " 
  return desc3


----------------------------------------
-- Strip out the special extra-libraries: entry
stripDesc desc = 
       desc { library     = fmap stripLib (library desc) 
  	    , executables = map  stripExe (executables desc)
	    }
stripLib lib = lib { libBuildInfo = stripBI (libBuildInfo lib) }
stripExe exe = exe { buildInfo = stripBI (buildInfo exe) }
stripBI bi   = bi  { extraLibs = filter filt (extraLibs bi) }

filt str = not (isInfixOf "intel_aes" str)

------------------------------------------------------------
my_preBuild :: Args -> BuildFlags -> IO HookedBuildInfo
my_preBuild args flags = do 
  putStrLn$ "\n================================================================================"
  let 
      ext = libext
      cached_so = "./cbits/prebuilt/libintel_aes_"++ Info.os ++"_"++ Info.arch ++ ext
      dest = "./cbits/libintel_aes"++ext
  e <- doesFileExist cached_so
  if e then do 
      putStrLn$ "  [intel-aes] Using prebuilt dynamic library: "++ cached_so
      copyFile cached_so dest
      putStrLn$ "  [intel-aes] Done copying into position: "++ dest
   else do     
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
  desc2 <- patchDesc desc linfo
  (buildHook simpleUserHooks) desc2 linfo hooks flags
  putStrLn$ "  [intel-aes] Build action finished.\n\n"

--------------------------------------------------------------------------------
my_install :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
my_install desc linfo hooks flags = do 
  putStrLn$ "  [intel-aes] Install action:"
  putStrLn$ "\n================================================================================"
  desc2 <- patchDesc desc linfo
  libd <- readFile tmpfile 
  let dest = (libd ++ "/libintel_aes" ++ libext)
  putStrLn$ "  [intel-aes] Copying shared library to: " ++ show dest
  system$ "mkdir -p "++ libd -- NONPORTABLE
  copyFile ("./cbits/libintel_aes"++libext) dest
  putStrLn$ "  [intel-aes] Done copying."
  -- removeFile tmpfile -- Might install more than once, right?
  putStrLn$ "================================================================================\n"
  (instHook simpleUserHooks) desc2 linfo hooks flags

libext :: String
libext = case Info.os of
	      "linux"   -> ".so"
	      "darwin"  -> ".dylib"
	      "windows" -> ".dll"
	      _         -> error$ "Unexpected OS: " ++ (Info.os)
