{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fractal_generator (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/include4eto/.cabal/bin"
libdir     = "/home/include4eto/.cabal/lib/x86_64-linux-ghc-8.0.1/fractal-generator-0.1.0.0"
datadir    = "/home/include4eto/.cabal/share/x86_64-linux-ghc-8.0.1/fractal-generator-0.1.0.0"
libexecdir = "/home/include4eto/.cabal/libexec"
sysconfdir = "/home/include4eto/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fractal_generator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fractal_generator_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fractal_generator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fractal_generator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fractal_generator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
