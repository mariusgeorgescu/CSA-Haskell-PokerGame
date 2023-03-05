{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_P2P_PokerGame (
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
bindir     = "/Users/mariusgeorgescu/.cabal/bin"
libdir     = "/Users/mariusgeorgescu/.cabal/lib/x86_64-osx-ghc-8.10.7/P2P-PokerGame-0.1.0.0-inplace-P2P-PokerGame"
dynlibdir  = "/Users/mariusgeorgescu/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/mariusgeorgescu/.cabal/share/x86_64-osx-ghc-8.10.7/P2P-PokerGame-0.1.0.0"
libexecdir = "/Users/mariusgeorgescu/.cabal/libexec/x86_64-osx-ghc-8.10.7/P2P-PokerGame-0.1.0.0"
sysconfdir = "/Users/mariusgeorgescu/.cabal/etc"

getBinDir     = catchIO (getEnv "P2P_PokerGame_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "P2P_PokerGame_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "P2P_PokerGame_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "P2P_PokerGame_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "P2P_PokerGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "P2P_PokerGame_sysconfdir") (\_ -> return sysconfdir)




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
