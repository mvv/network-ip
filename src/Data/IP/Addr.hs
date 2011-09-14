{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Internet Protocol addressing.
module Data.IP.Addr (
    IP4(..),
    ip4ToOctets,
    ip4FromOctets,
    anyIP4,
    NetAddr,
    Net4Addr,
    netPrefix,
    netMask,
    netLength,
    mkNetAddr,
    InetPort(..),
    InetAddr(..),
    Inet4Addr
  ) where

import Data.Typeable (Typeable, Typeable1)
import Data.Word
import Data.Bits
import Data.Ix (Ix)
import Data.List (intercalate)
import Data.Endian
import Data.Default
import Data.Hashable
import Data.Pointed
import Data.Functor.Plus
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Control.Applicative
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

-- | IPv4 address.
newtype IP4 = IP4 { unIP4 ∷ Word32 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable)

instance Show IP4 where
  show = intercalate "." . (show <$>) . ip4ToOctets

instance Storable IP4 where
  alignment _ = alignment (undefined ∷ Word32)
  sizeOf _    = 4
  peek p      = IP4 . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unIP4

instance Binary IP4 where
  get = IP4 . fromBigEndian <$> B.get
  put = B.put . toBigEndian . unIP4

instance Serialize IP4 where
  get = IP4 . fromBigEndian <$> S.get
  put = S.put . toBigEndian . unIP4

-- | Convert an IPv4 address to a list of octets.
ip4ToOctets ∷ IP4 → [Word8]
ip4ToOctets (IP4 w) = fromIntegral <$>
                        [w `shiftR` 24, w `shiftR` 16, w `shiftR` 8, w]

-- | Create an IPv4 address from four octets.
ip4FromOctets ∷ (Pointed f, Plus f) ⇒ [Word8] → f IP4
ip4FromOctets [o1, o2, o3, o4] =
  point $ IP4 $  fromIntegral o1 `shiftL` 24
             .|. fromIntegral o2 `shiftL` 16
             .|. fromIntegral o3 `shiftL` 8
             .|. fromIntegral o4
ip4FromOctets _ = zero

-- | IPv4 address @0.0.0.0@.
anyIP4 ∷ IP4
anyIP4 = IP4 0
{-# INLINE anyIP4 #-}

instance Default IP4 where
  def = anyIP4
  {-# INLINE def #-}

-- | Network address. Both mask and its length are stored for efficiency.
data NetAddr a = NetAddr { netPrefix ∷ a
                         , netMask   ∷ a
                         , netLength ∷ {-# UNPACK #-} !Word
                         }

deriving instance Typeable1 NetAddr

type Net4Addr = NetAddr IP4

instance Eq a ⇒ Eq (NetAddr a) where
  (NetAddr addr1 _ len1) == (NetAddr addr2 _ len2) = len1 == len2 &&
                                                     addr1 == addr2
  {-# INLINE (==) #-}

instance Show a ⇒ Show (NetAddr a) where
  show (NetAddr a _ n) = show a ++ "/" ++ show n

-- | Make network address from IP address and routing prefix length.
mkNetAddr ∷ Bits a ⇒ a → Word → NetAddr a
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
data InetAddr a = InetAddr a InetPort deriving Eq

deriving instance Typeable1 InetAddr

type Inet4Addr = InetAddr IP4

instance Show Inet4Addr where
  show (InetAddr a p) = show a ++ ":" ++ show p

instance Hashable Inet4Addr where
  hash (InetAddr a p) = hash a `combine` hash p

instance Binary a ⇒ Binary (InetAddr a) where
  get = InetAddr <$> B.get <*> B.get
  put (InetAddr a p) = B.put a >> B.put p

instance Serialize a ⇒ Serialize (InetAddr a) where
  get = InetAddr <$> S.get <*> S.get
  put (InetAddr a p) = S.put a >> S.put p

