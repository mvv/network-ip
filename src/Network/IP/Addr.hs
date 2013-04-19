{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Internet Protocol addressing.
module Network.IP.Addr
  ( 
    -- * Host address
    -- ** IPv4 address
    IP4(..)
  , anIP4
  , ip4ToOctets
  , ip4ToOctetList
  , ip4FromOctets
  , ip4FromOctetList
  , anyIP4
    -- ** IPv6 address
  , IP6(..)
  , anIP6
  , ip6ToWords
  , ip6ToWordList
  , ip6FromWords
  , ip6FromWordList
  , anyIP6
    -- ** IP address
  , IP(..)
  , anIP
  -- * Network address
  , IsNetAddr(..)
  , Net4Addr
  , Net6Addr
  , NetAddr
  , aNet4Addr
  , aNet6Addr
  , aNetAddr
  , printNetAddr
  , net4Parser
  , net6Parser
  , netParser
  -- * Port number
  , InetPort(..)
  , anInetPort
  -- * Socket address
  , InetAddr(..)
  , Inet4Addr
  , Inet6Addr
  , anInet4Addr
  , anInet6Addr
  , anInetAddr
  ) where

import Prelude hiding (print)
import Data.Typeable (Typeable, Typeable1)
import Data.Proxy (Proxy(..))
import Data.Word
import Data.Bits
import Data.DoubleWord (BinaryWord(..), DoubleWord(..), Word128)
import Data.Ix (Ix)
import Data.Endian
import Data.Default
import Data.Hashable
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Text.Printer (Printer, (<>))
import qualified Text.Printer as P
import qualified Text.Printer.Numerals as P
import Data.Textual
import Data.Textual.Numerals hiding (Binary)
import Text.Parser.Combinators ((<?>), try)
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC
import Text.Printf (printf)
import Control.Applicative
import Control.Monad (void, when)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

-- | IPv4 address.
newtype IP4 = IP4 { unIP4 ∷ Word32 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable,
            Binary, Serialize)

-- | 'IP4' proxy value.
anIP4 ∷ Proxy IP4
anIP4 = Proxy

instance Show IP4 where
  showsPrec p a = showParen (p > 10)
                $ showString "ip4FromOctets "
                . showsPrec 11 o1
                . showString " "
                . showsPrec 11 o2
                . showString " "
                . showsPrec 11 o3
                . showString " "
                . showsPrec 11 o4
    where (o1, o2, o3, o4) = ip4ToOctets a

instance Printable IP4 where
  print = P.fsep (P.char7 '.') . (P.nonNegative Decimal <$>) . ip4ToOctetList
  {-# INLINE print #-}

instance Textual IP4 where
  textual  =  (<?> "IPv4 address")
           $  ip4FromOctets
          <$> (octet <* PC.char '.')
          <*> (octet <* PC.char '.')
          <*> (octet <* PC.char '.')
          <*> octet
    where octet = (<?> "IPv4 octet") $ do
            o ← nnUpTo Decimal 3
            if o > 255
              then fail "out of bounds"
              else return $ fromIntegral (o ∷ Int)

instance Storable IP4 where
  alignment _ = alignment (undefined ∷ Word32)
  sizeOf _    = 4
  peek p      = IP4 . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unIP4

-- | The octets of an IPv4 address.
ip4ToOctets ∷ IP4 → (Word8, Word8, Word8, Word8)
ip4ToOctets (IP4 w) = ( fromIntegral $ w `shiftR` 24
                      , fromIntegral $ w `shiftR` 16
                      , fromIntegral $ w `shiftR` 8
                      , fromIntegral w
                      )
{-# INLINABLE ip4ToOctets #-}

-- | List the octets of an IPv4 address.
ip4ToOctetList ∷ IP4 → [Word8]
ip4ToOctetList (IP4 w) = fromIntegral <$>
                           [w `shiftR` 24, w `shiftR` 16, w `shiftR` 8, w]
{-# INLINABLE ip4ToOctetList #-}

-- | Assemble IPv4 address from the octets.
ip4FromOctets ∷ Word8 → Word8 → Word8 → Word8 → IP4
ip4FromOctets o1 o2 o3 o4 =
  IP4  $  fromIntegral o1 `shiftL` 24
      .|. fromIntegral o2 `shiftL` 16
      .|. fromIntegral o3 `shiftL` 8
      .|. fromIntegral o4
{-# INLINABLE ip4FromOctets #-}

-- | Assemble IPv4 address from the octet list.
ip4FromOctetList ∷ [Word8] → Maybe IP4
ip4FromOctetList [o1, o2, o3, o4] = Just $ ip4FromOctets o1 o2 o3 o4
ip4FromOctetList _                = Nothing
{-# INLINABLE ip4FromOctetList #-}

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

-- | 'IP6' proxy value.
anIP6 ∷ Proxy IP6
anIP6 = Proxy

instance Show IP6 where
  showsPrec p a
      = showParen (p > 10)
      $ showString
      $ printf "ip6FromWords 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x"
               w1 w2 w3 w4 w5 w6 w7 w8
    where
      (w1, w2, w3, w4, w5, w6, w7, w8) = ip6ToWords a

instance Printable IP6 where
  print addr = case addrZeroes of
      Nothing → P.fsep (P.char7 ':') $ hex <$> addrWords
      Just (i, n) →  P.fsep (P.char7 ':') (hex <$> take i addrWords)
                  <> P.string7 "::"
                  <> P.fsep (P.char7 ':') (hex <$> drop (i + n) addrWords)
    where hex        = P.nonNegative LowHex
          addrWords  = ip6ToWordList addr
          addrZeroes = go 0 0 0 addrWords
          go !i zi zn = \case
            []           → if zn <= 1 then Nothing else Just (zi, zn)
            (0 : words') → inZeroes (i + 1) zi zn i 1 words'
            (_ : words') → go (i + 1) zi zn words'
          inZeroes !i zi zn zi' !zn' = \case
            [] | zn' <= 1  → if zn <= 1 then Nothing else Just (zi, zn)
               | zn <= 1   → Just (zi', zn')
               | otherwise → Just $ if zn >= zn' then (zi, zn) else (zi', zn')
            (0 : words') → inZeroes (i + 1) zi zn zi' (zn' + 1) words'
            (_ : words') | zn' > zn  → go (i + 1) zi' zn' words'
                         | otherwise → go (i + 1) zi zn words'

instance Textual IP6 where
  textual = (<?> "IPv6 address") $ optional (PC.char ':') >>= \case
              Just _ → do
                void $ PC.char ':'
                optional word >>= \case
                  Just w → after 6 0 (fromIntegral w)
                  Nothing → return anyIP6
              Nothing → do
                w ← word
                before 7 (fromIntegral w)
    where 
      word = nnUpTo Hexadecimal 4 `hintTypeArg` aWord16 <?> "word"
      before 0 w = return $ IP6 w
      before 1 w = do
        void $ PC.char ':'
        optional (PC.char ':') >>= \case
          Just _ →
            return $ IP6 $ w `shiftL` 16
          Nothing → do
            w' ← word
            return $ IP6 $ (w `shiftL` 16) .|. fromIntegral w'
      before upTo w = do
        void $ PC.char ':'
        optional (PC.char ':') >>= \case
          Just _ → optional word >>= \case
            Just w' →
              after (upTo - 2) (w `shiftL` (upTo * 16)) (fromIntegral w')
            Nothing →
              return $ IP6 $ w `shiftL` (upTo * 16)
          Nothing → do
            w' ← word
            before (upTo - 1) ((w `shiftL` 16) .|. fromIntegral w')
      after 0 w1 w2 = return $ IP6 $ w1 .|. w2
      after upTo w1 w2 = optional (PC.char ':') >>= \case
        Just _ → do
          w ← word
          after (upTo - 1) w1 ((w2 `shiftL` 16) .|. fromIntegral w)
        Nothing →
          return $ IP6 $ w1 .|. w2

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

-- | The 16-bit pieces of an IPv6 address.
ip6ToWords ∷ IP6 → (Word16, Word16, Word16, Word16,
                    Word16, Word16, Word16, Word16)
ip6ToWords (IP6 w) =
    ( fromIntegral $ hi `shiftR` 48
    , fromIntegral $ hi `shiftR` 32
    , fromIntegral $ hi `shiftR` 16
    , fromIntegral hi
    , fromIntegral $ lo `shiftR` 48
    , fromIntegral $ lo `shiftR` 32
    , fromIntegral $ lo `shiftR` 16
    , fromIntegral lo
    )
  where hi = hiWord w
        lo = loWord w
{-# INLINABLE ip6ToWords #-}

-- | List the 16-bit pieces of an IPv6 address.
ip6ToWordList ∷ IP6 → [Word16]
ip6ToWordList (IP6 w) = fromIntegral <$>
    [ hi `shiftR` 48
    , hi `shiftR` 32
    , hi `shiftR` 16
    , hi
    , lo `shiftR` 48
    , lo `shiftR` 32
    , lo `shiftR` 16
    , lo
    ]
  where hi = hiWord w
        lo = loWord w
{-# INLINABLE ip6ToWordList #-}

-- | Assemble IPv6 address from the 16-bit pieces.
ip6FromWords ∷ Word16 → Word16 → Word16 → Word16
             → Word16 → Word16 → Word16 → Word16
             → IP6
ip6FromWords w1 w2 w3 w4 w5 w6 w7 w8 = IP6 $ fromHiAndLo hi lo
  where hi  =  fromIntegral w1 `shiftL` 48
           .|. fromIntegral w2 `shiftL` 32
           .|. fromIntegral w3 `shiftL` 16
           .|. fromIntegral w4
        lo  =  fromIntegral w5 `shiftL` 48
           .|. fromIntegral w6 `shiftL` 32
           .|. fromIntegral w7 `shiftL` 16
           .|. fromIntegral w8
{-# INLINABLE ip6FromWords #-}

-- | Assemble IPv6 address from the list of 16-bit pieces.
ip6FromWordList ∷ [Word16] → Maybe IP6
ip6FromWordList [w1, w2, w3, w4, w5, w6, w7, w8] =
  Just $ ip6FromWords w1 w2 w3 w4 w5 w6 w7 w8
ip6FromWordList _ = Nothing
{-# INLINABLE ip6FromWordList #-}

-- | IPv6 address @::@.
anyIP6 ∷ IP6
anyIP6 = IP6 0
{-# INLINE anyIP6 #-}

instance Default IP6 where
  def = anyIP6
  {-# INLINE def #-}

-- IP address.
data IP = IPv4 {-# UNPACK #-} !IP4
        | IPv6 {-# UNPACK #-} !IP6
        deriving (Typeable, Eq, Ord, Show)

-- | 'IP' proxy value.
anIP ∷ Proxy IP
anIP = Proxy

instance Printable IP where
  print (IPv4 a) = print a
  print (IPv6 a) = print a
  {-# INLINABLE print #-}

instance Textual IP where
  textual = try (IPv4 <$> textual) <|> (IPv6 <$> textual)
  {-# INLINABLE textual #-}

instance Binary IP where
  get = B.get >>= return . either IPv4 IPv6
  put (IPv4 a) = B.put (Left a ∷ Either IP4 IP6)
  put (IPv6 a) = B.put (Right a ∷ Either IP4 IP6)

instance Serialize IP where
  get = S.get >>= return . either IPv4 IPv6
  put (IPv4 a) = S.put (Left a ∷ Either IP4 IP6)
  put (IPv6 a) = S.put (Right a ∷ Either IP4 IP6)

-- | Network address.
class IsNetAddr n where
  -- | The address type.
  type NetHost n
  -- | Host address.
  netHost   ∷ n → NetHost n
  -- | Host index.
  netHostIx ∷ n → NetHost n
  -- | Network prefix.
  netPrefix ∷ n → NetHost n
  -- | Network mask.
  netMask   ∷ n → NetHost n
  -- | Network prefix length.
  netLength ∷ n → Word8
  -- | Make a network address.
  netAddr   ∷ NetHost n -- ^ Host address
            → Word8     -- ^ Network prefix length
            → n

-- | Network address: host address + network mask length.
data NetAddr a = NetAddr a {-# UNPACK #-} !Word8
                 deriving (Typeable, Eq)

type Net4Addr = NetAddr IP4
type Net6Addr = NetAddr IP6

-- | 'Net4Addr' proxy value.
aNet4Addr ∷ Proxy Net4Addr
aNet4Addr = Proxy

-- | 'Net6Addr' proxy value.
aNet6Addr ∷ Proxy Net6Addr
aNet6Addr = Proxy

-- | 'NetAddr' 'IP' proxy value.
aNetAddr ∷ Proxy (NetAddr IP)
aNetAddr = Proxy

instance Show a ⇒ Show (NetAddr a) where
  showsPrec p (NetAddr a w) = showParen (p > 10)
                            $ showString "netAddr "
                            . showsPrec 11 a
                            . showString " "
                            . showsPrec 11 w

instance Printable a ⇒ Printable (NetAddr a) where
  print (NetAddr a m) = print a <> P.char7 '/' <> print m
  {-# INLINE print #-}

instance Textual (NetAddr IP4) where
  textual = net4Parser
  {-# INLINE textual #-}

instance Textual (NetAddr IP6) where
  textual = net6Parser
  {-# INLINE textual #-}

instance Textual (NetAddr IP) where
  textual = netParser
  {-# INLINE textual #-}

instance Binary Net4Addr where
  get = netAddr <$> B.get <*> B.get
  put (NetAddr a w) = B.put a >> B.put w

instance Binary Net6Addr where
  get = netAddr <$> B.get <*> B.get
  put (NetAddr a w) = B.put a >> B.put w

instance Binary (NetAddr IP) where
  get = netAddr <$> B.get <*> B.get
  put (NetAddr a w) = B.put a >> B.put w

instance Serialize Net4Addr where
  get = netAddr <$> S.get <*> S.get
  put (NetAddr a w) = S.put a >> S.put w

instance Serialize Net6Addr where
  get = netAddr <$> S.get <*> S.get
  put (NetAddr a w) = S.put a >> S.put w

instance Serialize (NetAddr IP) where
  get = netAddr <$> S.get <*> S.get
  put (NetAddr a w) = S.put a >> S.put w

instance IsNetAddr Net4Addr where
  type NetHost Net4Addr = IP4
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = (a `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINE netHostIx #-}
  netPrefix (NetAddr a w) = (a `shiftR` l) `shiftL` l
    where l = 32 - fromIntegral w
  {-# INLINE netPrefix #-}
  netMask (NetAddr _ w) = IP4 $ (allOnes `shiftR` l) `shiftL` l
    where l = 32 - fromIntegral w
  {-# INLINE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` 32)
  {-# INLINE netAddr #-}

instance IsNetAddr Net6Addr where
  type NetHost Net6Addr = IP6
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = (a `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINE netHostIx #-}
  netPrefix (NetAddr a w) = (a `shiftR` l) `shiftL` l
    where l = 128 - fromIntegral w
  {-# INLINE netPrefix #-}
  netMask (NetAddr _ w) = IP6 $ (allOnes `shiftR` l) `shiftL` l
    where l = 128 - fromIntegral w
  {-# INLINE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` 128)
  {-# INLINE netAddr #-}

instance IsNetAddr (NetAddr IP) where
  type NetHost (NetAddr IP) = IP
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = case a of
      IPv4 a1 → IPv4 $ (a1 `shiftL` l) `shiftR` l
      IPv6 a1 → IPv6 $ (a1 `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINABLE netHostIx #-}
  netPrefix (NetAddr a w) = case a of
    IPv4 a1 → IPv4 $ (a1 `shiftR` l) `shiftL` l
      where l = 32 - fromIntegral w
    IPv6 a1 → IPv6 $ (a1 `shiftR` l) `shiftL` l
      where l = 128 - fromIntegral w
  {-# INLINABLE netPrefix #-}
  netMask (NetAddr a w) = case a of
    IPv4 _ → IPv4 $ IP4 $ (allOnes `shiftR` l) `shiftL` l
      where l = 32 - fromIntegral w
    IPv6 _ → IPv6 $ IP6 $ (allOnes `shiftR` l) `shiftL` l
      where l = 128 - fromIntegral w
  {-# INLINABLE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` m)
    where m = case a of
                IPv4 _ → 32
                IPv6 _ → 128
  {-# INLINABLE netAddr #-}

-- | Print network address (CIDR notation).
printNetAddr ∷ (IsNetAddr n, Printable (NetHost n), Printer p) ⇒ n → p
printNetAddr n = print (netHost n) <> P.char7 '/' <> print (netLength n)
{-# INLINABLE printNetAddr #-}

ip4Mask ∷ (CharParsing μ, Monad μ) ⇒ μ Word8
ip4Mask = (<?> "network prefix length") $ do
  m ← nncUpTo Decimal 2
  when (m > 32) $ fail "out of bounds"
  return m

-- | IPv4 network address parser (CIDR notation).
net4Parser ∷ (CharParsing μ, Monad μ, IsNetAddr n, NetHost n ~ IP4) ⇒ μ n
net4Parser  =  netAddr <$> textual <*> (PC.char '/' *> ip4Mask)
           <?> "IPv4 network address"
{-# INLINE net4Parser #-}

ip6Mask ∷ (CharParsing μ, Monad μ) ⇒ μ Word8
ip6Mask = (<?> "network prefix length") $ do
  m ← nncUpTo Decimal 3
  when (m > (128 ∷ Int)) $ fail "out of bounds"
  return $ fromIntegral m

-- | IPv6 network address parser (CIDR notation).
net6Parser ∷ (CharParsing μ, Monad μ, IsNetAddr n, NetHost n ~ IP6) ⇒ μ n
net6Parser  =  netAddr <$> textual <*> (PC.char '/' *> ip6Mask)
           <?> "IPv6 network address"
{-# INLINE net6Parser #-}

-- | IP network address parser (CIDR notation).
netParser ∷ (IsNetAddr n, NetHost n ~ IP, CharParsing μ, Monad μ) ⇒ μ n
netParser = (<?> "IP network address") $ do
    a ← textual
    void $ PC.char '/'
    m ← mask a
    return $ netAddr a m
  where
    mask (IPv4 _) = ip4Mask
    mask (IPv6 _) = ip6Mask

-- | Port number.
newtype InetPort = InetPort { unInetPort ∷ Word16 }
  deriving (Typeable, Eq, Ord, Bounded, Enum, Ix, Num, Real, Integral, Bits,
            Hashable, Binary, Serialize, Printable)

-- | 'InetPort' proxy value.
anInetPort ∷ Proxy InetPort
anInetPort = Proxy

instance Show InetPort where
  show (InetPort p) = show p
  {-# INLINE show #-}

instance Textual InetPort where
  textual = InetPort <$> nncBounded Decimal <?> "port number"

instance Storable InetPort where
  alignment _ = alignment (undefined ∷ Word16)
  sizeOf _    = 2
  peek p      = InetPort . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unInetPort

-- | Socket address: host address + port number.
data InetAddr a = InetAddr { inetHost ∷ a
                           , inetPort ∷ {-# UNPACK #-} !InetPort
                           } deriving (Eq, Ord, Show)

deriving instance Typeable1 InetAddr

type Inet4Addr = InetAddr IP4
type Inet6Addr = InetAddr IP6

-- | 'Inet4Addr' proxy value.
anInet4Addr ∷ Proxy Inet4Addr
anInet4Addr = Proxy

-- | 'Inet6Addr' proxy value.
anInet6Addr ∷ Proxy Inet6Addr
anInet6Addr = Proxy

-- | 'InetAddr' 'IP' proxy value.
anInetAddr ∷ Proxy (InetAddr IP)
anInetAddr = Proxy

instance Functor InetAddr where
  fmap f (InetAddr a p) = InetAddr (f a) p
  {-# INLINE fmap #-}

instance Printable Inet4Addr where
  print (InetAddr a p) = print a <> P.char7 ':' <> print p
  {-# INLINABLE print #-}

instance Printable Inet6Addr where
  print (InetAddr a p) = P.brackets (print a) <> P.char7 ':' <> print p
  {-# INLINABLE print #-}

instance Printable (InetAddr IP) where
  print (InetAddr (IPv4 a) p) = print (InetAddr a p)
  print (InetAddr (IPv6 a) p) = print (InetAddr a p)
  {-# INLINABLE print #-}

instance Textual Inet4Addr where
  textual  =  InetAddr <$> textual <*> (PC.char ':' *> textual)
          <?> "IPv4 socket address"
  {-# INLINE textual #-}

instance Textual Inet6Addr where
  textual  =  InetAddr <$> (PC.char '[' *> textual <* PC.char ']')
                      <*> (PC.char ':' *> textual)
          <?> "IPv6 socket address"
  {-# INLINE textual #-}

instance Textual (InetAddr IP) where
  textual  =  InetAddr
         <$> (optional (PC.char '[') >>= \case
                Nothing → IPv4 <$> textual
                Just _  → IPv6 <$> textual <* PC.char ']')
         <*> (PC.char ':' *> textual)
         <?> "IP socket address"
  {-# INLINABLE textual #-}

instance Hashable a ⇒ Hashable (InetAddr a) where
#if MIN_VERSION_hashable(1,2,0)
  hashWithSalt s (InetAddr a p) = s `hashWithSalt` a `hashWithSalt` p
  {-# INLINE hashWithSalt #-}
#else
  hash (InetAddr a p) = hash a `combine` hash p
  {-# INLINE hash #-}
#endif

instance Binary a ⇒ Binary (InetAddr a) where
  get = InetAddr <$> B.get <*> B.get
  put (InetAddr a p) = B.put a >> B.put p

instance Serialize a ⇒ Serialize (InetAddr a) where
  get = InetAddr <$> S.get <*> S.get
  put (InetAddr a p) = S.put a >> S.put p

