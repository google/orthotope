{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_orthotope (
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

bindir     = "/usr/local/google/home/augustss/.cabal/bin"
libdir     = "/usr/local/google/home/augustss/.cabal/lib/x86_64-linux-ghc-8.10.1/orthotope-0.1.0.0-EFjWQ1VhT688J5Zx4oeK95"
dynlibdir  = "/usr/local/google/home/augustss/.cabal/lib/x86_64-linux-ghc-8.10.1"
datadir    = "/usr/local/google/home/augustss/.cabal/share/x86_64-linux-ghc-8.10.1/orthotope-0.1.0.0"
libexecdir = "/usr/local/google/home/augustss/.cabal/libexec/x86_64-linux-ghc-8.10.1/orthotope-0.1.0.0"
sysconfdir = "/usr/local/google/home/augustss/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "orthotope_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "orthotope_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "orthotope_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "orthotope_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "orthotope_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "orthotope_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
