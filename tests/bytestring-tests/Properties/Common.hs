-- |
-- Module      : Properties.ShortByteString
-- Copyright   : (c) Andrew Lelechenko 2021
-- License     : BSD-style

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- We are happy to sacrifice optimizations in exchange for faster compilation,
-- but need to test rewrite rules. As one can check using -ddump-rule-firings,
-- rewrite rules do not fire in -O0 mode, so we use -O1, but disable almost all
-- optimizations. It roughly halves compilation time.
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules
  -fmax-simplifier-iterations=1 -fsimplifier-phases=0
  -fno-call-arity -fno-case-merge -fno-cmm-elim-common-blocks -fno-cmm-sink
  -fno-cpr-anal -fno-cse -fno-do-eta-reduction -fno-float-in -fno-full-laziness
  -fno-loopification -fno-specialise -fno-strictness -Wno-unused-imports -Wno-unused-top-binds #-}

#ifdef OSWORD
module Properties.OsString (tests) where
import System.OsString.Internal.Types (OsString(..), OsChar(..), getOsChar)
import qualified System.OsString as B
import qualified System.OsString as BS
import qualified System.OsString.Data.ByteString.Short.Internal as BSI (_nul, isSpace)

#else

#ifdef WORD16
#ifdef WIN
module Properties.WindowsString (tests) where
import qualified System.OsString.Windows as B
import qualified System.OsString.Windows as BS
#else
module Properties.ShortByteString.Word16 (tests) where
import System.OsString.Data.ByteString.Short.Internal (_nul, isSpace)
import qualified System.OsString.Data.ByteString.Short.Word16 as B
import qualified System.OsString.Data.ByteString.Short as BS
#endif
#else
#ifdef POSIX
module Properties.PosixString (tests) where
import qualified System.OsString.Posix as B
import qualified System.OsString.Posix as BS
#else
module Properties.ShortByteString (tests) where
import qualified System.OsString.Data.ByteString.Short as B
#endif
#endif
#endif

import Data.ByteString.Short (ShortByteString)

import qualified Data.Char as C
import qualified System.OsString.Data.ByteString.Short.Word16 as B16
import qualified System.OsString.Data.ByteString.Short as B8

import Data.Word

import Control.Arrow
import Data.Coerce (coerce)
import Data.Type.Coercion (Coercion(..), coerceWith, sym)
import Data.Foldable
import Data.List as L
import Data.Semigroup
import Data.Tuple
import Test.QuickCheck
import Test.QuickCheck.Monadic ( monadicIO, run )
import Text.Show.Functions ()

import System.OsString.Internal.Types (WindowsString(..), WindowsChar(..), getWindowsChar, PosixChar(..), PosixString(..), getPosixChar, OsString(..), OsChar(..), getOsChar)
import qualified System.OsString.Posix as PBS
import qualified System.OsString.Windows as WBS
import qualified System.OsString as OBS
import qualified System.OsString.Data.ByteString.Short.Internal as BSI (_nul, isSpace)


instance Arbitrary PosixString where
  arbitrary = do
    bs <- sized sizedByteString'
    n  <- choose (0, 2)
    return (PBS.drop n bs) -- to give us some with non-0 offset
   where
    sizedByteString' :: Int -> Gen PosixString
    sizedByteString' n = do m <- choose(0, n)
                            fmap (PosixString . B8.pack) $ vectorOf m arbitrary

instance Arbitrary PosixChar where
  arbitrary = fmap PosixChar (arbitrary @Word8)

instance CoArbitrary PosixChar where
  coarbitrary s = coarbitrary (PBS.toChar s)

instance CoArbitrary PosixString where
  coarbitrary s = coarbitrary (PBS.unpack s)

deriving instance Num PosixChar

deriving instance Bounded PosixChar

instance Arbitrary WindowsString where
  arbitrary = do
    bs <- sized sizedByteString'
    n  <- choose (0, 2)
    return (WBS.drop n bs) -- to give us some with non-0 offset
   where
    sizedByteString' :: Int -> Gen WindowsString
    sizedByteString' n = do m <- choose(0, n)
                            fmap (WindowsString . B16.pack) $ vectorOf m arbitrary

instance Arbitrary WindowsChar where
  arbitrary = fmap WindowsChar (arbitrary @Word16)

instance CoArbitrary WindowsChar where
  coarbitrary s = coarbitrary (WBS.toChar s)

instance CoArbitrary WindowsString where
  coarbitrary s = coarbitrary (WBS.unpack s)

deriving instance Num WindowsChar

deriving instance Bounded WindowsChar

isSpaceWin :: WindowsChar -> Bool
isSpaceWin = BSI.isSpace . getWindowsChar

numWordWin :: WindowsString -> Int
numWordWin = B16.numWord16 . getWindowsString


swapWWin :: WindowsChar -> WindowsChar
swapWWin = WindowsChar . byteSwap16 . getWindowsChar

isSpacePosix :: PosixChar -> Bool
isSpacePosix = C.isSpace . word8ToChar . getPosixChar

numWordPosix :: PosixString -> Int
numWordPosix = B8.length . getPosixString


swapWPosix :: PosixChar -> PosixChar
swapWPosix = id

#ifdef OSWORD
isSpace :: OsChar -> Bool
isSpace = case OBS.coercionToPlatformTypes of
  Left (co, _) -> isSpaceWin . coerceWith co
  Right (co, _) -> isSpacePosix . coerceWith co

numWord :: OsString -> Int
numWord = case OBS.coercionToPlatformTypes of
  Left (_, co) -> numWordWin . coerceWith co
  Right (_, co) -> numWordPosix . coerceWith co

toElem :: OsChar -> OsChar
toElem = id

swapW :: OsChar -> OsChar
swapW = case OBS.coercionToPlatformTypes of
  Left (co, _) -> coerceWith (sym co) . swapWWin . coerceWith co
  Right (co, _) -> coerceWith (sym co) . swapWPosix . coerceWith co

instance Arbitrary OsString where
  arbitrary = OsString <$> arbitrary

instance Arbitrary OsChar where
  arbitrary = OsChar <$> arbitrary

instance CoArbitrary OsChar where
  coarbitrary s = coarbitrary (OBS.toChar s)

instance CoArbitrary OsString where
  coarbitrary s = coarbitrary (OBS.unpack s)

deriving instance Num OsChar
deriving instance Bounded OsChar

instance Arbitrary ShortByteString where
  arbitrary = case OBS.coercionToPlatformTypes of
    Left (_, _) -> getWindowsString <$> arbitrary
    Right (_, _) -> getPosixString <$> arbitrary

#else

#ifdef WORD16

instance Arbitrary ShortByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (B16.drop n bs) -- to give us some with non-0 offset
   where
    sizedByteString :: Int -> Gen ShortByteString
    sizedByteString n = do m <- choose(0, n)
                           fmap B16.pack $ vectorOf m arbitrary

instance CoArbitrary ShortByteString where
  coarbitrary s = coarbitrary (B16.unpack s)
#ifdef WIN

isSpace :: WindowsChar -> Bool
isSpace = isSpaceWin

numWord :: WindowsString -> Int
numWord = numWordWin

toElem :: WindowsChar -> WindowsChar
toElem = id

swapW :: WindowsChar -> WindowsChar
swapW = swapWWin

#else
numWord :: ShortByteString -> Int
numWord = B.numWord16

toElem :: Word16 -> Word16
toElem = id

swapW :: Word16 -> Word16
swapW = byteSwap16


#endif
#else
#ifdef POSIX

isSpace :: PosixChar -> Bool
isSpace = isSpacePosix

numWord :: PosixString -> Int
numWord = numWordPosix

toElem :: PosixChar -> PosixChar
toElem = id

swapW :: PosixChar -> PosixChar
swapW = swapWPosix

#else
_nul :: Word8
_nul = 0x00

isSpace :: Word8 -> Bool
isSpace = C.isSpace . word8ToChar


numWord :: ShortByteString -> Int
numWord = B8.length

toElem :: Word8 -> Word8
toElem = id

swapW :: Word8 -> Word8
swapW = id



#endif

instance Arbitrary ShortByteString where
  arbitrary = do
    bs <- sized sizedByteString'
    n  <- choose (0, 2)
    return (B8.drop n bs) -- to give us some with non-0 offset
   where
    sizedByteString' :: Int -> Gen ShortByteString
    sizedByteString' n = do m <- choose(0, n)
                            fmap B8.pack $ vectorOf m arbitrary
  shrink = map B8.pack . shrink . B8.unpack

instance CoArbitrary ShortByteString where
  coarbitrary s = coarbitrary (B8.unpack s)
#endif
#endif


tests :: [(String, Property)]
tests =
  [ ("pack . unpack",
   property $ \x -> x === B.pack (B.unpack x))
  , ("unpack . pack" ,
   property $ \(map toElem -> xs) -> xs === B.unpack (B.pack xs))
  , ("read . show" ,
   property $ \x -> (x :: ShortByteString) === read (show x))

  , ("==" ,
   property $ \x y -> (x == y) === (B.unpack x == B.unpack y))
  , ("== refl" ,
   property $ \x -> (x :: ShortByteString) == x)
  , ("== symm",
   property $ \x y -> ((x :: ShortByteString) == y) === (y == x))
  , ("== pack unpack",
   property $ \x -> x == B.pack (B.unpack x))

  , ("compare",
   property $ \x y -> compare x y === compare (swapW <$> B.unpack x) (swapW <$> B.unpack y))
  , ("compare EQ",
   property $ \x -> compare (x :: ShortByteString) x == EQ)
  , ("compare GT",
   property $ \x (toElem -> c) -> compare (B.snoc x c) x == GT)
  , ("compare LT",
   property $ \x (toElem -> c) -> compare x (B.snoc x c) == LT)
  , ("compare GT empty",
   property $ \x -> not (B.null x) ==> compare x B.empty == GT)
  , ("compare LT empty",
   property $ \x -> not (B.null x) ==> compare B.empty x == LT)
  , ("compare GT concat",
   property $ \x y -> not (B.null y) ==> compare (x `mappend` y) x == GT)
  , ("compare char" ,
   property $ \(toElem -> c) (toElem -> d) -> compare (swapW c) (swapW d) == compare (B.singleton c) (B.singleton d))
  , ("compare unsigned",
    once $ compare (B.singleton 255) (B.singleton 127) == GT)

  , ("null" ,
   property $ \x -> B.null x === null (B.unpack x))
  , ("empty 0" ,
    once $ numWord B.empty === 0)
  , ("empty []",
    once $ B.unpack B.empty === [])
  , ("mempty 0",
    once $ numWord mempty === 0)
  , ("mempty []",
    once $ B.unpack mempty === [])

#ifdef WORD16
#ifdef WIN
  , ("isInfixOf works correctly under UTF16",
    once $
      let foo    = WindowsString $ B8.pack [0xbb, 0x03]
          foo'   = WindowsString $ B8.pack [0xd2, 0xbb]
          bar    = WindowsString $ B8.pack [0xd2, 0xbb, 0x03, 0xad]
          bar'   = WindowsString $ B8.pack [0xd2, 0xbb, 0x03, 0xad, 0xd2, 0xbb, 0x03, 0xad, 0xbb, 0x03, 0x00, 0x00]
      in [B.isInfixOf foo bar, B.isInfixOf foo' bar, B.isInfixOf foo bar'] === [False, True, True]
    )
#else
  , ("isInfixOf works correctly under UTF16",
    once $
      let foo    = BS.pack [0xbb, 0x03]
          foo'   = BS.pack [0xd2, 0xbb]
          bar    = BS.pack [0xd2, 0xbb, 0x03, 0xad]
          bar'   = BS.pack [0xd2, 0xbb, 0x03, 0xad, 0xd2, 0xbb, 0x03, 0xad, 0xbb, 0x03, 0x00, 0x00]
      in [B.isInfixOf foo bar, B.isInfixOf foo' bar, B.isInfixOf foo bar'] === [False, True, True]
    )
#endif
#endif
  , ("break breakSubstring",
    property $ \(toElem -> c) x -> B.break (== c) x === B.breakSubstring (B.singleton c) x
    )
  , ("breakSubstring",
    property $ \x y -> not (B.null x) ==> B.null (snd (B.breakSubstring x y)) === not (B.isInfixOf x y)
    )
  , ("breakSubstring empty",
    property $ \x -> B.breakSubstring B.empty x === (B.empty, x)
    )
  , ("isInfixOf",
    property $ \x y -> B.isInfixOf x y === L.isInfixOf (B.unpack x) (B.unpack y))

  , ("mconcat" ,
   property $ \xs -> B.unpack (mconcat xs) === mconcat (map B.unpack xs))
  , ("mconcat [x,x]" ,
   property $ \x -> B.unpack (mconcat [x, x]) === mconcat [B.unpack x, B.unpack x])
  , ("mconcat [x,[]]" ,
   property $ \x -> B.unpack (mconcat [x, B.empty]) === mconcat [B.unpack x, []])

  , ("null" ,
   property $ \x -> B.null x === null (B.unpack x))
  , ("reverse" ,
   property $ \x -> B.unpack (B.reverse x) === reverse (B.unpack x))
  , ("all" ,
   property $ \f x -> B.all f x === all f (B.unpack x))
  , ("all ==" ,
   property $ \(toElem -> c) x -> B.all (== c) x === all (== c) (B.unpack x))
  , ("any" ,
   property $ \f x -> B.any f x === any f (B.unpack x))
  , ("any ==" ,
   property $ \(toElem -> c) x -> B.any (== c) x === any (== c) (B.unpack x))
  , ("mappend" ,
   property $ \x y -> B.unpack (mappend x y) === B.unpack x `mappend` B.unpack y)
  , ("<>" ,
   property $ \x y -> B.unpack (x `mappend` y) === B.unpack x `mappend` B.unpack y)
  , ("stimes" ,
   property $ \(Positive n) x -> stimes (n :: Int) (x :: ShortByteString) === mtimesDefault n x)

  , ("break" ,
   property $ \f x -> (B.unpack *** B.unpack) (B.break f x) === break f (B.unpack x))
  , ("break ==" ,
   property $ \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (== c) x) === break (== c) (B.unpack x))
  , ("break /=" ,
   property $ \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (/= c) x) === break (/= c) (B.unpack x))
  , ("break span" ,
   property $ \f x -> B.break f x === B.span (not . f) x)
  , ("breakEnd" ,
   property $ \f x -> B.breakEnd f x === swap ((B.reverse *** B.reverse) (B.break f (B.reverse x))))
  , ("breakEnd" ,
   property $ \f x -> B.breakEnd f x === B.spanEnd (not . f) x)
  , ("break isSpace" ,
   property $ \x -> (B.unpack *** B.unpack) (B.break isSpace x) === break isSpace (B.unpack x))

  , ("singleton" ,
   property $ \(toElem -> c) -> B.unpack (B.singleton c) === [c])
  , ("cons" ,
   property $ \(toElem -> c) x -> B.unpack (B.cons c x) === c : B.unpack x)
  , ("cons []" ,
   property $ \(toElem -> c) -> B.unpack (B.cons c B.empty) === [c])
  , ("uncons" ,
   property $ \x -> fmap (second B.unpack) (B.uncons x) === L.uncons (B.unpack x))
  , ("snoc" ,
   property $ \(toElem -> c) x -> B.unpack (B.snoc x c) === B.unpack x ++ [c])
  , ("snoc []" ,
   property $ \(toElem -> c) -> B.unpack (B.snoc B.empty c) === [c])
  , ("unsnoc" ,
   property $ \x -> fmap (first B.unpack) (B.unsnoc x) === unsnoc (B.unpack x))

  , ("drop" ,
   property $ \n x -> B.unpack (B.drop n x) === drop (fromIntegral n) (B.unpack x))
  , ("drop 10" ,
   property $ \x -> B.unpack (B.drop 10 x) === drop 10 (B.unpack x))
  , ("dropWhile" ,
   property $ \f x -> B.unpack (B.dropWhile f x) === dropWhile f (B.unpack x))
  , ("dropWhile ==" ,
   property $ \(toElem -> c) x -> B.unpack (B.dropWhile (== c) x) === dropWhile (== c) (B.unpack x))
  , ("dropWhile /=" ,
   property $ \(toElem -> c) x -> B.unpack (B.dropWhile (/= c) x) === dropWhile (/= c) (B.unpack x))
  , ("dropWhile isSpace" ,
   property $ \x -> B.unpack (B.dropWhile isSpace x) === dropWhile isSpace (B.unpack x))

  , ("take" ,
   property $ \n x -> B.unpack (B.take n x) === take (fromIntegral n) (B.unpack x))
  , ("take 10" ,
   property $ \x -> B.unpack (B.take 10 x) === take 10 (B.unpack x))
  , ("takeWhile" ,
   property $ \f x -> B.unpack (B.takeWhile f x) === takeWhile f (B.unpack x))
  , ("takeWhile ==" ,
   property $ \(toElem -> c) x -> B.unpack (B.takeWhile (== c) x) === takeWhile (== c) (B.unpack x))
  , ("takeWhile /=" ,
   property $ \(toElem -> c) x -> B.unpack (B.takeWhile (/= c) x) === takeWhile (/= c) (B.unpack x))

  , ("takeWhile isSpace" ,
   property $ \x -> B.unpack (B.takeWhile isSpace x) === takeWhile isSpace (B.unpack x))

  , ("dropEnd" ,
   property $ \n x -> B.dropEnd n x === B.take (numWord x - n) x)
  , ("dropWhileEnd" ,
   property $ \f x -> B.dropWhileEnd f x === B.reverse (B.dropWhile f (B.reverse x)))
  , ("takeEnd" ,
   property $ \n x -> B.takeEnd n x === B.drop (numWord x - n) x)
  , ("takeWhileEnd" ,
   property $ \f x -> B.takeWhileEnd f x === B.reverse (B.takeWhile f (B.reverse x)))

  , ("length" ,
   property $ \x -> numWord x === fromIntegral (length (B.unpack x)))
#if defined(OSWORD) || defined(WIN) || defined(POSIX)
  , ("length abc" ,
   once $ B.length (B.pack [0xbb, 0x03]) == 2)
#endif
  , ("count" ,
   property $ \(toElem -> c) x -> B.count c x === fromIntegral (length (elemIndices c (B.unpack x))))
  , ("filter" ,
   property $ \f x -> B.unpack (B.filter f x) === filter f (B.unpack x))
  , ("filter compose" ,
   property $ \f g x -> B.filter f (B.filter g x) === B.filter (\c -> f c && g c) x)
  , ("filter ==" ,
   property $ \(toElem -> c) x -> B.unpack (B.filter (== c) x) === filter (== c) (B.unpack x))
  , ("filter /=" ,
   property $ \(toElem -> c) x -> B.unpack (B.filter (/= c) x) === filter (/= c) (B.unpack x))
  , ("partition" ,
   property $ \f x -> (B.unpack *** B.unpack) (B.partition f x) === partition f (B.unpack x))

  , ("find" ,
   property $ \f x -> B.find f x === find f (B.unpack x))
  , ("findIndex" ,
   property $ \f x -> B.findIndex f x === fmap fromIntegral (findIndex f (B.unpack x)))
  , ("findIndices" ,
   property $ \f x -> B.findIndices f x === fmap fromIntegral (findIndices f (B.unpack x)))
  , ("findIndices ==" ,
   property $ \(toElem -> c) x -> B.findIndices (== c) x === fmap fromIntegral (findIndices (== c) (B.unpack x)))

  , ("elem" ,
   property $ \(toElem -> c) x -> B.elem c x === elem c (B.unpack x))
  , ("not elem" ,
   property $ \(toElem -> c) x -> not (B.elem c x) === notElem c (B.unpack x))
  , ("elemIndex" ,
   property $ \(toElem -> c) x -> B.elemIndex c x === fmap fromIntegral (elemIndex c (B.unpack x)))
  , ("elemIndices" ,
   property $ \(toElem -> c) x -> B.elemIndices c x === fmap fromIntegral (elemIndices c (B.unpack x)))


  , ("map" ,
   property $ \f x -> B.unpack (B.map (toElem . f) x) === map (toElem . f) (B.unpack x))
  , ("map compose" ,
   property $ \f g x -> B.map (toElem . f) (B.map (toElem . g) x) === B.map (toElem . f . toElem . g) x)
  , ("replicate" ,
   property $ \n (toElem -> c) -> B.unpack (B.replicate (fromIntegral n) c) === replicate n c)
  , ("replicate 0" ,
   property $ \(toElem -> c) -> B.unpack (B.replicate 0 c) === replicate 0 c)

  , ("span" ,
   property $ \f x -> (B.unpack *** B.unpack) (B.span f x) === span f (B.unpack x))
  , ("span ==" ,
   property $ \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (== c) x) === span (== c) (B.unpack x))
  , ("span /=" ,
   property $ \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (/= c) x) === span (/= c) (B.unpack x))
  , ("spanEnd" ,
   property $ \f x -> B.spanEnd f x === swap ((B.reverse *** B.reverse) (B.span f (B.reverse x))))
  , ("split" ,
   property $ \(toElem -> c) x -> map B.unpack (B.split c x) === split c (B.unpack x))
  , ("split empty" ,
   property $ \(toElem -> c) -> B.split c B.empty === [])
  , ("splitWith" ,
   property $ \f x -> map B.unpack (B.splitWith f x) === splitWith f (B.unpack x))
  , ("splitWith split" ,
   property $ \(toElem -> c) x -> B.splitWith (== c) x === B.split c x)
  , ("splitWith empty" ,
   property $ \f -> B.splitWith f B.empty === [])
  , ("splitWith length" ,
   property $ \f x -> let splits = B.splitWith f x; l1 = fromIntegral (length splits); l2 = numWord (B.filter f x) in
      (l1 == l2 || l1 == l2 + 1) && sum (map numWord splits) + l2 == numWord x)
  , ("splitAt" ,
   property $ \n x -> (B.unpack *** B.unpack) (B.splitAt n x) === splitAt (fromIntegral n) (B.unpack x))

  , ("head" ,
   property $ \x -> not (B.null x) ==> B.head x == head (B.unpack x))
  , ("last" ,
   property $ \x -> not (B.null x) ==> B.last x == last (B.unpack x))
  , ("tail" ,
   property $ \x -> not (B.null x) ==> B.unpack (B.tail x) == tail (B.unpack x))
  , ("tail length" ,
   property $ \x -> not (B.null x) ==> numWord x == 1 + numWord (B.tail x))
  , ("init" ,
   property $ \x -> not (B.null x) ==> B.unpack (B.init x) == init (B.unpack x))
  , ("init length" ,
   property $ \x -> not (B.null x) ==> numWord x == 1 + numWord (B.init x))

  , ("foldl" ,
   property $ \f (toElem -> c) x -> B.foldl ((toElem .) . f) c x === foldl ((toElem .) . f) c (B.unpack x))
  , ("foldl'" ,
   property $ \f (toElem -> c) x -> B.foldl' ((toElem .) . f) c x === foldl' ((toElem .) . f) c (B.unpack x))
  , ("foldr" ,
   property $ \f (toElem -> c) x -> B.foldr ((toElem .) . f) c x === foldr ((toElem .) . f) c (B.unpack x))
  , ("foldr'" ,
   property $ \f (toElem -> c) x -> B.foldr' ((toElem .) . f) c x === foldr' ((toElem .) . f) c (B.unpack x))

  , ("foldl cons" ,
   property $ \x -> B.foldl (flip B.cons) B.empty x === B.reverse x)
  , ("foldr cons" ,
   property $ \x -> B.foldr B.cons B.empty x === x)
  , ("foldl special" ,
   property $ \x (toElem -> c) -> B.unpack (B.foldl (\acc t -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldl (\acc t -> if t == c then acc else t : acc) [] (B.unpack x))
  , ("foldr special" ,
   property $ \x (toElem -> c) -> B.unpack (B.foldr (\t acc -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldr (\t acc -> if t == c then acc else t : acc) [] (B.unpack x))

  , ("foldl1" ,
   property $ \f x -> not (B.null x) ==> B.foldl1 ((toElem .) . f) x == foldl1 ((toElem .) . f) (B.unpack x))
  , ("foldl1'" ,
   property $ \f x -> not (B.null x) ==> B.foldl1' ((toElem .) . f) x == foldl1' ((toElem .) . f) (B.unpack x))
  , ("foldr1" ,
   property $ \f x -> not (B.null x) ==> B.foldr1 ((toElem .) . f) x == foldr1 ((toElem .) . f) (B.unpack x))
  , ("foldr1'", -- there is not Data.List.foldr1'
   property $ \f x -> not (B.null x) ==> B.foldr1' ((toElem .) . f) x == foldr1 ((toElem .) . f) (B.unpack x))

  , ("foldl1 const" ,
   property $ \x -> not (B.null x) ==> B.foldl1 const x == B.head x)
  , ("foldl1 flip const" ,
   property $ \x -> not (B.null x) ==> B.foldl1 (flip const) x == B.last x)
  , ("foldr1 const" ,
   property $ \x -> not (B.null x) ==> B.foldr1 const x == B.head x)
  , ("foldr1 flip const" ,
   property $ \x -> not (B.null x) ==> B.foldr1 (flip const) x == B.last x)
  , ("foldl1 max" ,
   property $ \x -> not (B.null x) ==> B.foldl1 max x == B.foldl max minBound x)
  , ("foldr1 max" ,
   property $ \x -> not (B.null x) ==> B.foldr1 max x == B.foldr max minBound x)

  , ("index" ,
   property $ \(NonNegative n) x -> fromIntegral n < numWord x ==> B.index x (fromIntegral n) == B.unpack x !! n)
  , ("indexMaybe" ,
   property $ \(NonNegative n) x -> fromIntegral n < numWord x ==> B.indexMaybe x (fromIntegral n) == Just (B.unpack x !! n))
  , ("indexMaybe Nothing" ,
   property $ \n x -> (n :: Int) < 0 || fromIntegral n >= numWord x ==> B.indexMaybe x (fromIntegral n) == Nothing)
  , ("!?" ,
   property $ \n x -> B.indexMaybe x (fromIntegral (n :: Int)) === x B.!? (fromIntegral n))

  , ("unfoldrN" ,
   property $ \n f (toElem -> c) -> B.unpack (fst (B.unfoldrN n (fmap (first toElem) . f) c)) ===
      take (fromIntegral n) (unfoldr (fmap (first toElem) . f) c))
  , ("unfoldrN replicate" ,
   property $ \n (toElem -> c) -> fst (B.unfoldrN n (\t -> Just (t, t)) c) === B.replicate n c)
  , ("unfoldr" ,
   property $ \n a (toElem -> c) -> B.unpack (B.unfoldr (\x -> if x <= 100 * n then Just (c, x + 1 :: Int) else Nothing) a) ===
      unfoldr (\x -> if x <= 100 * n then Just (c, x + 1) else Nothing) a)

  --, ("unfoldr" ,
  -- property $ \n f (toElem -> a) -> B.unpack (B.take (fromIntegral n) (B.unfoldr (fmap (first toElem) . f) a)) ===
  --    take n (unfoldr (fmap (first toElem) . f) a))
  --
#if defined(WORD16) && !defined(WIN) && !defined(OSWORD) && !defined(POSIX)
  , ("useAsCWString str packCWString == str" ,
   property $ \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCWString x B.packCWString >>= \x' -> pure (x == x')))
  , ("useAsCWStringLen str packCWStringLen == str" ,
   property $ \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCWStringLen x B.packCWStringLen >>= \x' -> pure (x == x')))
#endif
#if !defined(WORD16) && !defined(WIN) && !defined(OSWORD) && !defined(POSIX)
  , ("useAsCString str packCString == str" ,
   property $ \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCString x B.packCString >>= \x' -> pure (x == x')))
  , ("useAsCStringLen str packCStringLen == str" ,
   property $ \x -> not (B.any (== _nul) x)
      ==> monadicIO $ run (B.useAsCStringLen x B.packCStringLen >>= \x' -> pure (x == x')))
#endif
  ]

split :: Eq a => a -> [a] -> [[a]]
split c = splitWith (== c)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f ys = go [] ys
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
      | f x       = reverse acc : go [] xs
      | otherwise = go (x : acc) xs

#if !MIN_VERSION_base(4, 19, 0)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)
#endif

-- | Total conversion to char.
word8ToChar :: Word8 -> Char
word8ToChar = C.chr . fromIntegral
