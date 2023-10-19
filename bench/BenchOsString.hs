{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeApplications    #-}

#define OSSTR osstr
#define OS_STRING OsString
#define OS_CHAR OsChar


module BenchOsString (benchMark) where

import           System.OsString                       (osstr)
import qualified System.OsString                       as S
import           System.OsString.Internal.Types        (OsString(..), OsChar(..), PosixChar(..), WindowsChar(..))

#include "Common.hs"

benchStr :: String
benchStr = "OsString"

w :: Int -> OsChar
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
w i = OsChar (WindowsChar (fromIntegral i))
#else
w i = OsChar (PosixChar (fromIntegral i))
#endif

hashWord8 :: OsChar -> OsChar
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
hashWord8 (OsChar (WindowsChar w)) = OsChar . WindowsChar . fromIntegral . hashInt . fromIntegral $ w
#else
hashWord8 (OsChar (PosixChar w)) = OsChar . PosixChar . fromIntegral . hashInt . fromIntegral $ w
#endif

iw :: OsChar -> Int
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
iw (OsChar (WindowsChar w)) = fromIntegral w
#else
iw (OsChar (PosixChar w)) = fromIntegral w
#endif

