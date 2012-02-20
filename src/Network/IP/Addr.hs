{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Internet Protocol addressing.
module Network.IP.Addr
  ( IP4(..)
  , ip4ToOctets
  , ip4FromOctets
  , ip4FromString
  , anyIP4
  , IP6(..)
  , ip6ToWords
  , ip6FromWords
  , anyIP6
  , IP(..)
  , NetAddr
  , Net4Addr
  , Net6Addr
  , netPrefix
  , netMask
  , netLength
  , mkNetAddr
  , InetPort(..)
  , InetAddr(..)
  , Inet4Addr
  , Inet6Addr
  ) where

import Data.Typeable (Typeable, Typeable1)
import Data.Word
import Data.Bits
import Data.DoubleWord (DoubleWord(..), Word128)
import Data.Char (ord)
import Data.Ix (Ix)
import Data.List (intercalate)
import Data.Endian
import Data.Default
import Data.Hashable
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Control.Applicative
import Control.Monad (join)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

-- | IPv4 address.
newtype IP4 = IP4 { unIP4 ∷ Word32 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable,
            Binary, Serialize)

instance Show IP4 where
  show = intercalate "." . (show <$>) . ip4ToOctets

instance Storable IP4 where
  alignment _ = alignment (undefined ∷ Word32)
  sizeOf _    = 4
  peek p      = IP4 . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unIP4

-- | Convert an IPv4 address to a list of octets.
ip4ToOctets ∷ IP4 → [Word8]
ip4ToOctets (IP4 w) = fromIntegral <$>
                        [w `shiftR` 24, w `shiftR` 16, w `shiftR` 8, w]

-- | Create an IPv4 address from four octets.
ip4FromOctets ∷ [Word8] → Maybe IP4
ip4FromOctets [o1, o2, o3, o4] =
  Just $ IP4  $  fromIntegral o1 `shiftL` 24
             .|. fromIntegral o2 `shiftL` 16
             .|. fromIntegral o3 `shiftL` 8
             .|. fromIntegral o4
ip4FromOctets _ = Nothing

splitOn ∷ Char → String → [String]
splitOn c s = go s [] []
  where go []      [] [] = []
        go []      r  [] = [reverse r]
        go []      r  rs = reverse (reverse r : rs)
        go (h : t) r  rs =
          if h == c
            then go t [] (reverse r : rs)
            else go t (h : r) rs

-- | Read an IPv4 address written in dot-decimal notation.
ip4FromString ∷ String → Maybe IP4
ip4FromString = join
              . fmap ip4FromOctets 
              . sequence
              . fmap octet
              . splitOn '.'
  where
    octet [d1]
      | d1 >= '0' && d1 <= '9'
      = Just $ fromIntegral $ ord d1 - ord '0'
    octet [d1, d2]
      | d1 >= '1' && d1 <= '9' && d2 >= '0' && d2 <= '9'
      = Just $ fromIntegral $ (ord d1 - ord '0') * 10 + (ord d2 - ord '0')
    octet [d1, d2, d3]
      | d1 >= '1' && d1 <= '9' && d2 >= '0' && d2 <= '9' &&
        d3 >= '0' && d3 <= '9'
      = let i = (ord d1 - ord '0') * 100 + (ord d2 - ord '0') * 10 +
                (ord d3 - ord '0') in
        if i > 255 then Nothing else Just (fromIntegral i)
    octet _ = Nothing

-- | IPv4 address @0.0.0.0@.
anyIP4 ∷ IP4
anyIP4 = IP4 0
{-# INLINE anyIP4 #-}

instance Default IP4 where
  def = anyIP4
  {-# INLINE def #-}

-- | IPv6 address.
newtype IP6 = IP6 { unIP6 ∷ Word128 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable)

instance Show IP6 where
  show = undefined

instance Storable IP6 where
  alignment _ = alignment (undefined ∷ Word64)
  sizeOf _    = 16
  peek p      = fmap IP6
              $ fromHiAndLo <$> (fromBigEndian <$> peek (castPtr p))
                            <*> (fromBigEndian <$> peek (castPtr p))
  poke p (IP6 w) = do
    poke (castPtr p) $ toBigEndian $ hiWord w
    poke (castPtr p) $ toBigEndian $ loWord w

instance Binary IP6 where
  get = fmap IP6 $ fromHiAndLo <$> B.get <*> B.get
  put (IP6 w) = do
    B.put $ hiWord w
    B.put $ loWord w

instance Serialize IP6 where
  get = fmap IP6 $ fromHiAndLo <$> S.get <*> S.get
  put (IP6 w) = do
    S.put $ hiWord w
    S.put $ loWord w

-- | Convert an IPv6 address to a list of 16-bit words.
ip6ToWords ∷ IP6 → [Word16]
ip6ToWords (IP6 w) = fromIntegral <$>
    [hi `shiftR` 48, hi `shiftR` 32, hi `shiftR` 16, hi,
     lo `shiftR` 48, lo `shiftR` 32, lo `shiftR` 16, lo]
  where hi = hiWord w
        lo = loWord w

-- | Create an IPv6 address from eight 16-bit words.
ip6FromWords ∷ [Word16] → Maybe IP6
ip6FromWords [w1, w2, w3, w4, w5, w6, w7, w8] = Just $ IP6 $ fromHiAndLo hi lo
  where hi  =  fromIntegral w1 `shiftL` 48
           .|. fromIntegral w2 `shiftL` 32
           .|. fromIntegral w3 `shiftL` 16
           .|. fromIntegral w4
        lo  =  fromIntegral w5 `shiftL` 48
           .|. fromIntegral w6 `shiftL` 32
           .|. fromIntegral w7 `shiftL` 16
           .|. fromIntegral w8
ip6FromWords _ = Nothing

-- | IPv6 address @::@.
anyIP6 ∷ IP6
anyIP6 = IP6 0
{-# INLINE anyIP6 #-}

instance Default IP6 where
  def = anyIP6
  {-# INLINE def #-}

data IP = IPv4 {-# UNPACK #-} !IP4
        | IPv6 {-# UNPACK #-} !IP6
        deriving (Typeable, Eq, Ord)

instance Show IP where
  show (IPv4 addr) = show addr
  show (IPv6 addr) = show addr

-- | Network address. Both mask and its length are stored for efficiency.
data NetAddr a = NetAddr { netPrefix ∷ a
                         , netMask   ∷ a
                         , netLength ∷ {-# UNPACK #-} !Word
                         }

deriving instance Typeable1 NetAddr

instance Eq a ⇒ Eq (NetAddr a) where
  (NetAddr addr1 _ len1) == (NetAddr addr2 _ len2) = len1 == len2 &&
                                                     addr1 == addr2
  {-# INLINE (==) #-}

instance Show a ⇒ Show (NetAddr a) where
  show (NetAddr a _ n) = show a ++ "/" ++ show n

type Net4Addr = NetAddr IP4
type Net6Addr = NetAddr IP6

instance Default Net4Addr where
  def = NetAddr anyIP4 anyIP4 0
  {-# INLINE def #-}

instance Default Net6Addr where
  def = NetAddr anyIP6 anyIP6 0
  {-# INLINE def #-}

-- | Make network address from IP address and routing prefix length.
mkNetAddr ∷ (Num a, Bits a) ⇒ a → Word → NetAddr a
mkNetAddr addr' len' = NetAddr addr mask len
  where bits = fromIntegral $ bitSize addr'
        len  = min len' bits
        mask = complement 0 `shiftL` fromIntegral (bits - len)
        addr = addr' .&. mask
{-# INLINE mkNetAddr #-}

-- | Port number.
newtype InetPort = InetPort { unInetPort ∷ Word16 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Real, Integral, Bits,
            Hashable)

instance Show InetPort where
  show (InetPort p) = show p

instance Storable InetPort where
  alignment _ = alignment (undefined ∷ Word16)
  sizeOf _    = 2
  peek p      = InetPort . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unInetPort

instance Binary InetPort where
  get = InetPort . fromBigEndian <$> B.get
  put = B.put . toBigEndian . unInetPort

instance Serialize InetPort where
  get = InetPort . fromBigEndian <$> S.get
  put = S.put . toBigEndian . unInetPort

-- | Socket address: IP address + port number.
data InetAddr a = InetAddr { inetAddr ∷ a
                           , inetPort ∷ {-# UNPACK #-} !InetPort
                           } deriving (Eq, Ord)

deriving instance Typeable1 InetAddr

type Inet4Addr = InetAddr IP4
type Inet6Addr = InetAddr IP6

instance Show Inet4Addr where
  show (InetAddr a p) = show a ++ ":" ++ show p

instance Show Inet6Addr where
  show (InetAddr a p) = "[" ++ show a ++ "]:" ++ show p

instance Show (InetAddr IP) where
  show (InetAddr (IPv4 a) p) = show $ InetAddr a p
  show (InetAddr (IPv6 a) p) = show $ InetAddr a p

instance Hashable a ⇒ Hashable (InetAddr a) where
  hash (InetAddr a p) = hash a `combine` hash p

instance Binary a ⇒ Binary (InetAddr a) where
  get = InetAddr <$> B.get <*> B.get
  put (InetAddr a p) = B.put a >> B.put p

instance Serialize a ⇒ Serialize (InetAddr a) where
  get = InetAddr <$> S.get <*> S.get
  put (InetAddr a p) = S.put a >> S.put p

