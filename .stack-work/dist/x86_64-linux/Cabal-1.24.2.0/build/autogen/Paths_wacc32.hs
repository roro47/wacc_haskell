{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_wacc32 (
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

bindir     = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/bin"
libdir     = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/wacc32-0.1.0.0-4S7PksEjjU2F0BX6tEYlF3"
dynlibdir  = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/share/x86_64-linux-ghc-8.0.2/wacc32-0.1.0.0"
libexecdir = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/libexec"
sysconfdir = "/homes/tz2617/wacc_32/.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wacc32_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wacc32_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wacc32_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wacc32_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wacc32_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wacc32_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
