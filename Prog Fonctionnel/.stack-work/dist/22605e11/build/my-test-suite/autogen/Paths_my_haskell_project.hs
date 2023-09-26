{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_my_haskell_project (
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
bindir     = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\bin"
libdir     = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\lib\\x86_64-windows-ghc-9.4.7\\my-haskell-project-0.1.0.0-HM7AmHrzj9vCp3YxO5jbQ7-my-test-suite"
dynlibdir  = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\lib\\x86_64-windows-ghc-9.4.7"
datadir    = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\share\\x86_64-windows-ghc-9.4.7\\my-haskell-project-0.1.0.0"
libexecdir = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\libexec\\x86_64-windows-ghc-9.4.7\\my-haskell-project-0.1.0.0"
sysconfdir = "C:\\Users\\MZIANI\\OneDrive - TALENT BUSINESS SOLUTIONS\\Bureau\\Projet_AYOTI\\.stack-work\\install\\418bb6be\\etc"

getBinDir     = catchIO (getEnv "my_haskell_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "my_haskell_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "my_haskell_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "my_haskell_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_haskell_project_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
