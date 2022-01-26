{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_graphqshell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/bin"
libdir     = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/lib/x86_64-osx-ghc-8.10.7/graphqshell-0.1.0.0-FVvrOhoWdPVLr4sxelB826-graphqshell-exe"
dynlibdir  = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/share/x86_64-osx-ghc-8.10.7/graphqshell-0.1.0.0"
libexecdir = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/libexec/x86_64-osx-ghc-8.10.7/graphqshell-0.1.0.0"
sysconfdir = "/Users/david/Coding/graphqshell/graphqshell/.stack-work/install/x86_64-osx/deed8cae7bcba4ab0188393c63216c878e5528b436cb04130f0b197ee5acc9f8/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "graphqshell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "graphqshell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "graphqshell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "graphqshell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "graphqshell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "graphqshell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
