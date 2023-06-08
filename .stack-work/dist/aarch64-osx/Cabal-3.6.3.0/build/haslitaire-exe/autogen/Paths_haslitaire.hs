{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_haslitaire (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/bin"
libdir     = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/lib/aarch64-osx-ghc-9.2.7/haslitaire-0.1.0.0-HkeJWv37knUDosLiExnRYr-haslitaire-exe"
dynlibdir  = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/lib/aarch64-osx-ghc-9.2.7"
datadir    = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/share/aarch64-osx-ghc-9.2.7/haslitaire-0.1.0.0"
libexecdir = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/libexec/aarch64-osx-ghc-9.2.7/haslitaire-0.1.0.0"
sysconfdir = "/Users/alexandersommer/Desktop/fritid/haskell/haslitaire/.stack-work/install/aarch64-osx/0f01c001fe9e84d64732fdb84c33c5a9a0d393e8af9d6dd03321444c3fbd511b/9.2.7/etc"

getBinDir     = catchIO (getEnv "haslitaire_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "haslitaire_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "haslitaire_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "haslitaire_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haslitaire_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haslitaire_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
