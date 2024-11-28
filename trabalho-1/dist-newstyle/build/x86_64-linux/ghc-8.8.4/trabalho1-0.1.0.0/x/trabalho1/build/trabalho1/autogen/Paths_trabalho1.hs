{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_trabalho1 (
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

bindir     = "/home/manu/.cabal/bin"
libdir     = "/home/manu/.cabal/lib/x86_64-linux-ghc-8.8.4/trabalho1-0.1.0.0-inplace-trabalho1"
dynlibdir  = "/home/manu/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/manu/.cabal/share/x86_64-linux-ghc-8.8.4/trabalho1-0.1.0.0"
libexecdir = "/home/manu/.cabal/libexec/x86_64-linux-ghc-8.8.4/trabalho1-0.1.0.0"
sysconfdir = "/home/manu/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trabalho1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trabalho1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "trabalho1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "trabalho1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trabalho1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trabalho1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
