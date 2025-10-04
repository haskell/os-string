{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnliftedFFITypes         #-}

#if !defined(__MHS__)
-- Required for WORDS_BIGENDIAN
#include <ghcautoconf.h>
#endif

-- |
-- Module      :  System.OsString.Data.ByteString.Short.Internal
-- Copyright   :  © 2022 Julian Ospald
-- License     :  MIT
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal low-level utilities mostly for 'System.OsPath.Data.ByteString.Short.Word16',
-- such as byte-array operations and other stuff not meant to be exported from Word16 module.
module System.OsString.Data.ByteString.Short.Internal where

import Control.Monad.ST
import Control.Exception (assert, throwIO)
import Data.ByteString.Short.Internal (ShortByteString(..), length)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
  ( Semigroup((<>)) )
import Foreign.C.Types
  ( CSize(..)
  , CInt(..)
  )
import Data.ByteString.Internal
  ( accursedUnutterablePerformIO
  )
#endif
#if !MIN_VERSION_bytestring(0,10,9)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.C.String ( CString, CStringLen )
import Foreign.C.Types ( CSize(..) )
import Foreign.Storable (pokeByteOff)
#endif
import Foreign.Marshal.Array (withArray0, peekArray0, newArray0, withArrayLen, peekArray)
import GHC.Exts
#if defined(__MHS__)
import Data.Word
import Control.Monad.ST ( ST )
#else
import GHC.Word
import GHC.ST
    ( ST (ST) )
#endif
import GHC.Stack ( HasCallStack )
import Prelude hiding
    ( length )

import qualified Data.ByteString.Short.Internal as BS
import qualified Data.Char as C
import qualified Data.List as List


_nul :: Word16
_nul = 0x00

isSpace :: Word16 -> Bool
isSpace = C.isSpace . word16ToChar

-- | Total conversion to char.
word16ToChar :: Word16 -> Char
word16ToChar = C.chr . fromIntegral

#if defined(__MHS__)
import Control.Monad(forM_)
import qualified Data.Array.Byte as B
import Foreign.Ptr(Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import System.IO.Unsafe(unsafePerformIO)

w8 :: Word16 -> Word8
w8 = fromIntegral

iw8 :: Int -> Word8
iw8 = fromIntegral

w16 :: Word8 -> Word16
w16 = fromIntegral

-- Use already existsing MHS types for the byte arrays
type BA = B.ByteArray
type MBA s = B.MutableByteArray s

create :: Int -> (forall s. MBA s -> ST s ()) -> BS.ShortByteString
create len fill =
  runST $ do
    mba <- B.newMutableByteArray len
    fill mba
    ba <- B.unsafeFreezeMutableByteArray mba
    return $ fromBA ba

asBA :: BS.ShortByteString -> BA
asBA = B.byteStringToByteArray . BS.fromShort

fromBA :: BA -> BS.ShortByteString
fromBA = BS.toShort . B.byteArrayToByteString

newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray len = B.newMutableByteArray len

newByteArray :: Int -> ST s (MBA s)
newByteArray len = B.newMutableByteArray len

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray src src_off dst dst_off len =
  forM_ [0..len-1] $ \ i ->
    B.writeWord8 dst (dst_off + i) (B.indexWord8 src (src_off + i))

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray = B.unsafeFreezeMutableByteArray

copyAddrToByteArray :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray src dst dst_off len =
  forM_ [0..len-1] $ \ i -> do
    b <- unsafeIOToST $ peekByteOff src i
    B.writeWord8 dst (dst_off + i) b

#else /* defined(__MHS__) */

create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)
{-# INLINE create #-}


asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#



data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)


newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray (I# len#) =
    ST $ \s -> case newPinnedByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

copyAddrToByteArray :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                 s' -> (# s', () #)
#endif /* defined(__MHS__) */


  -- this is a copy-paste from bytestring
#if !MIN_VERSION_bytestring(0,10,9)
------------------------------------------------------------------------
-- Primop replacements

-- ---------------------------------------------------------------------
--
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize


-- ---------------------------------------------------------------------
--
-- Uses our C code
--

-- | /O(n)./ Construct a new @ShortByteString@ from a @CString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
--
-- @since 0.10.10.0
packCString :: CString -> IO ShortByteString
packCString cstr = do
  len <- c_strlen cstr
  packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen (cstr, len) | len >= 0 = BS.createFromPtr cstr len
packCStringLen (_, len) =
  moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString bs action =
  allocaBytes (l+1) $ \buf -> do
      BS.copyToPtr bs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a 'CStringLen'.
-- As for 'useAsCString' this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- Beware that this function does not add a terminating @\NUL@ byte at the end of 'CStringLen'.
-- If you need to construct a pointer to a null-terminated sequence, use 'useAsCString'
-- (and measure length independently if desired).
--
-- @since 0.10.10.0
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen bs action =
  allocaBytes l $ \buf -> do
      BS.copyToPtr bs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length bs


#endif


-- | /O(n)./ Construct a new @ShortByteString@ from a @CWString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CWString@, and is managed on the Haskell heap. The original
-- @CWString@ must be null terminated.
--
-- @since 0.10.10.0
packCWString :: Ptr Word16 -> IO ShortByteString
packCWString cwstr = do
  cs <- peekArray0 _nul cwstr
  return (packWord16 cs)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CWStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CWStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCWStringLen :: (Ptr Word16, Int) -> IO ShortByteString
packCWStringLen (cp, len) = do
  cs <- peekArray len cp
  return (packWord16 cs)


-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CWString@.  The @CWString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWString :: ShortByteString -> (Ptr Word16 -> IO a) -> IO a
useAsCWString = withArray0 _nul . unpackWord16

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCWStringLen :: ShortByteString -> ((Ptr Word16, Int) -> IO a) -> IO a
useAsCWStringLen bs action = withArrayLen (unpackWord16 bs) $ \ len ptr -> action (ptr, len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CWStringLen@.
-- As for @useAsCWString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
newCWString :: ShortByteString -> IO (Ptr Word16)
newCWString = newArray0 _nul . unpackWord16




 -- ---------------------------------------------------------------------
-- Internal utilities

moduleErrorIO :: String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "System.OsPath.Data.ByteString.Short." ++ fun ++ ':':' ':msg

packWord16 :: [Word16] -> ShortByteString
packWord16 cs = packLenWord16 (List.length cs) cs

packLenWord16 :: Int -> [Word16] -> ShortByteString
packLenWord16 len ws0 =
    create (len * 2) (\mba -> go mba 0 ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba i w
      go mba (i+2) ws


unpackWord16 :: ShortByteString -> [Word16]
unpackWord16 sbs = go len []
  where
    len = length sbs
    go !i !acc
      | i < 1     = acc
      | otherwise = let !w = indexWord16Array (asBA sbs) (i - 2)
                    in go (i - 2) (w:acc)

packWord16Rev :: [Word16] -> ShortByteString
packWord16Rev cs = packLenWord16Rev (List.length cs * 2) cs

packLenWord16Rev :: Int -> [Word16] -> ShortByteString
packLenWord16Rev len ws0 =
    create len (\mba -> go mba len ws0)
  where
    go :: MBA s -> Int -> [Word16] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord16Array mba (i - 2) w
      go mba (i - 2) ws


#if defined(__MHS__)
-- | Encode Word16 as little-endian.
writeWord16Array :: MBA s
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
                 -> ST s ()
writeWord16Array mba i w = do
  let (hi, lo) = w `quotRem` 256
  B.writeWord8 mba i     (w8 lo)
  B.writeWord8 mba (i+1) (w8 hi)

indexWord8Array :: BA
                -> Int      -- ^ Word8 index
                -> Word8
indexWord8Array = B.indexWord8

-- | Decode Word16 from little-endian.
indexWord16Array :: BA
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
indexWord16Array ba i = w16 (B.indexWord8 ba (i+1)) * 256 + w16 (B.indexWord8 ba i)

setByteArray :: MBA s -> Int -> Int -> Int -> ST s ()
setByteArray dst off len c =
  forM_ [0..len-1] $ \ i -> B.writeWord8 dst (off + i) (iw8 c)

copyMutableByteArray :: MBA s -> Int -> MBA s -> Int -> Int -> ST s ()
copyMutableByteArray src src_off dst dst_off len =
  forM_ [0..len-1] $ \ i -> do
    b <- B.readWord8 src (src_off + i)
    B.writeWord8 dst (dst_off + i) b

createAndTrim :: Int -> (forall s. MBA s -> ST s (Int, a)) -> (ShortByteString, a)
createAndTrim l fill =
    runST $ do
      mba <- newByteArray l
      (l', res) <- fill mba
      if assert (l' <= l) $ l' >= l
          then do
            ba <- unsafeFreezeByteArray mba
            return (fromBA ba, res)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            ba <- unsafeFreezeByteArray mba2
            return (fromBA ba, res)

createAndTrim' :: Int -> (forall s. MBA s -> ST s Int) -> ShortByteString
createAndTrim' l fill =
    runST $ do
      mba <- newByteArray l
      l' <- fill mba
      if assert (l' <= l) $ l' >= l
          then do
            ba <- unsafeFreezeByteArray mba
            return (fromBA ba)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            ba <- unsafeFreezeByteArray mba2
            return (fromBA ba)
{-# INLINE createAndTrim' #-}

createAndTrim'' :: Int -> (forall s. MBA s -> MBA s -> ST s (Int, Int)) -> (ShortByteString, ShortByteString)
createAndTrim'' l fill =
    runST $ do
      mba1 <- newByteArray l
      mba2 <- newByteArray l
      (l1, l2) <- fill mba1 mba2
      sbs1 <- freeze' l1 mba1
      sbs2 <- freeze' l2 mba2
      pure (sbs1, sbs2)
  where
    freeze' :: Int -> MBA s -> ST s ShortByteString
    freeze' l' mba =
      if assert (l' <= l) $ l' >= l
          then do
            ba <- unsafeFreezeByteArray mba
            return (fromBA ba)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            ba <- unsafeFreezeByteArray mba2
            return (fromBA ba)

#else /* defined(__MHS__) */

-- | Encode Word16 as little-endian.
writeWord16Array :: MBA s
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
                 -> ST s ()
writeWord16Array (MBA# mba#) (I# i#) (W16# w#) = ST $ \s ->
  case writeWord8ArrayAsWord16# mba# i# (word16ToLE# w#) s of
    s' -> (# s', () #)

indexWord8Array :: BA
                -> Int      -- ^ Word8 index
                -> Word8
indexWord8Array (BA# ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

-- | Decode Word16 from little-endian.
indexWord16Array :: BA
                 -> Int      -- ^ Word8 index (not Word16)
                 -> Word16
indexWord16Array (BA# ba#) (I# i#) =
  W16# (word16FromLE# (indexWord8ArrayAsWord16# ba# i#))

#if MIN_VERSION_base(4,16,0)
word16ToLE#, word16FromLE# :: Word16# -> Word16#
#else
word16ToLE#, word16FromLE# :: Word# -> Word#
#endif
#ifdef WORDS_BIGENDIAN
#if MIN_VERSION_base(4,16,0)
word16ToLE# w = wordToWord16# (byteSwap16# (word16ToWord# w))
#else
word16ToLE# = byteSwap16#
#endif
#else
word16ToLE# w# = w#
#endif
word16FromLE# = word16ToLE#

setByteArray :: MBA s -> Int -> Int -> Int -> ST s ()
setByteArray (MBA# dst#) (I# off#) (I# len#) (I# c#) =
    ST $ \s -> case setByteArray# dst# off# len# c# s of
                 s' -> (# s', () #)

copyMutableByteArray :: MBA s -> Int -> MBA s -> Int -> Int -> ST s ()
copyMutableByteArray (MBA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyMutableByteArray# src# src_off# dst# dst_off# len# s of
                 s' -> (# s', () #)

-- | Given the maximum size needed and a function to make the contents
-- of a ShortByteString, createAndTrim makes the 'ShortByteString'.
-- The generating function is required to return the actual final size
-- (<= the maximum size) and the result value. The resulting byte array
-- is realloced to this size.
createAndTrim :: Int -> (forall s. MBA s -> ST s (Int, a)) -> (ShortByteString, a)
createAndTrim l fill =
    runST $ do
      mba <- newByteArray l
      (l', res) <- fill mba
      if assert (l' <= l) $ l' >= l
          then do
            BA# ba# <- unsafeFreezeByteArray mba
            return (SBS ba#, res)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            BA# ba# <- unsafeFreezeByteArray mba2
            return (SBS ba#, res)
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (forall s. MBA s -> ST s Int) -> ShortByteString
createAndTrim' l fill =
    runST $ do
      mba <- newByteArray l
      l' <- fill mba
      if assert (l' <= l) $ l' >= l
          then do
            BA# ba# <- unsafeFreezeByteArray mba
            return (SBS ba#)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            BA# ba# <- unsafeFreezeByteArray mba2
            return (SBS ba#)
{-# INLINE createAndTrim' #-}

createAndTrim'' :: Int -> (forall s. MBA s -> MBA s -> ST s (Int, Int)) -> (ShortByteString, ShortByteString)
createAndTrim'' l fill =
    runST $ do
      mba1 <- newByteArray l
      mba2 <- newByteArray l
      (l1, l2) <- fill mba1 mba2
      sbs1 <- freeze' l1 mba1
      sbs2 <- freeze' l2 mba2
      pure (sbs1, sbs2)
  where
    freeze' :: Int -> MBA s -> ST s ShortByteString
    freeze' l' mba =
      if assert (l' <= l) $ l' >= l
          then do
            BA# ba# <- unsafeFreezeByteArray mba
            return (SBS ba#)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            BA# ba# <- unsafeFreezeByteArray mba2
            return (SBS ba#)
{-# INLINE createAndTrim'' #-}
#endif /* defined(__MHS__) */

-- Returns the index of the first match or the length of the whole
-- bytestring if nothing matched.
findIndexOrLength :: (Word16 -> Bool) -> ShortByteString -> Int
findIndexOrLength k (assertEven -> sbs) = go 0
  where
    l = BS.length sbs
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n >= l     = l `div` 2
          | k (w n)    = n `div` 2
          | otherwise  = go (n + 2)
{-# INLINE findIndexOrLength #-}


-- | Returns the length of the substring matching, not the index.
-- If no match, returns 0.
findFromEndUntil :: (Word16 -> Bool) -> ShortByteString -> Int
findFromEndUntil k sbs = go (BS.length sbs - 2)
  where
    ba = asBA sbs
    w = indexWord16Array ba
    go !n | n < 0     = 0
          | k (w n)   = (n `div` 2) + 1
          | otherwise = go (n - 2)
{-# INLINE findFromEndUntil #-}


assertEven :: ShortByteString -> ShortByteString
#if defined(__MHS__)
assertEven sbs
  | even (BS.length sbs) = sbs
#else
assertEven sbs@(SBS barr#)
  | even (I# (sizeofByteArray# barr#)) = sbs
#endif
  | otherwise = error ("Uneven number of bytes: " <> show (BS.length sbs) <> ". This is not a Word16 bytestream.")


-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptySBS :: HasCallStack => String -> a
errorEmptySBS fun = moduleError fun "empty ShortByteString"
{-# NOINLINE errorEmptySBS #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

compareByteArraysOff :: BA  -- ^ array 1
                     -> Int -- ^ offset for array 1
                     -> BA  -- ^ array 2
                     -> Int -- ^ offset for array 2
                     -> Int -- ^ length to compare
                     -> Int -- ^ like memcmp
#if defined(__MHS__)
compareByteArraysOff ba1 ba1off ba2 ba2off len = unsafePerformIO $
  B.withByteArrayPtr ba1 $ \ ptr1 ->
    B.withByteArrayPtr ba2 $ \ ptr2 ->
      c_memcmp (plusPtr ptr1 ba1off) (plusPtr ptr2 ba2off) len
foreign import ccall "string.h memcmp" c_memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int

#else /* defined(__MHS__) */
#if MIN_VERSION_base(4,11,0)
compareByteArraysOff (BA# ba1#) (I# ba1off#) (BA# ba2#) (I# ba2off#) (I# len#) =
  I# (compareByteArrays#  ba1# ba1off# ba2# ba2off# len#)
#else
compareByteArraysOff (BA# ba1#) ba1off (BA# ba2#) ba2off len =
  assert (ba1off + len <= (I# (sizeofByteArray# ba1#)))
  $ assert (ba2off + len <= (I# (sizeofByteArray# ba2#)))
  $ fromIntegral $ accursedUnutterablePerformIO $
    c_memcmp_ByteArray ba1#
                       ba1off
                       ba2#
                       ba2off
                       (fromIntegral len)


foreign import ccall unsafe "static sbs_memcmp_off"
  c_memcmp_ByteArray :: ByteArray# -> Int -> ByteArray# -> Int -> CSize -> IO CInt
#endif
#endif /* defined(__MHS__) */

