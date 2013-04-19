{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.DoubleWord
import Data.Textual
import Network.IP.Addr
import Control.Applicative

instance Arbitrary IP4 where
  arbitrary = IP4 <$> arbitrary
  shrink = fmap IP4 . shrink . unIP4

instance Arbitrary IP6 where
  arbitrary = fmap IP6 $ fromHiAndLo <$> arbitrary <*> arbitrary
  shrink = fmap IP6 . shrinkIntegral . unIP6

instance Arbitrary IP where
  arbitrary = oneof [IPv4 <$> arbitrary, IPv6 <$> arbitrary]
  shrink (IPv4 a) = IPv4 <$> shrink a
  shrink (IPv6 a) = IPv6 <$> shrink a

instance Arbitrary Net4Addr where
  arbitrary = netAddr <$> arbitrary <*> suchThat arbitrary (<= 32)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary Net6Addr where
  arbitrary = netAddr <$> arbitrary <*> suchThat arbitrary (<= 128)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary (NetAddr IP) where
  arbitrary = do
      a ← arbitrary
      m ← mask a
      return $ netAddr a m
    where
      mask (IPv4 _) = suchThat arbitrary (<= 32)
      mask (IPv6 _) = suchThat arbitrary (<= 128)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary InetPort where
  arbitrary = InetPort <$> arbitrary
  shrink = fmap InetPort . shrink . unInetPort

instance Arbitrary a ⇒ Arbitrary (InetAddr a) where
  arbitrary = InetAddr <$> arbitrary <*> arbitrary
  shrink n = uncurry InetAddr <$> shrink (inetHost n, inetPort n)

main = defaultMain
         [ testGroup "IP4"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP4)
             , testProperty "parsing \"1.2.3.4\"" $
                 fromString "1.2.3.4" == Just (IP4 0x01020304)
             , testProperty "parsing \"11.22.33.44\"" $
                 fromString "11.22.33.44" == Just (IP4 0x0b16212c)
             , testProperty "parsing \"1.256.3.4\" fails" $
                 fromStringAs anIP4 "1.256.3.4" == Nothing
             , testProperty "parsing \" 1.2.3.4\" fails" $
                 fromStringAs anIP4 " 1.2.3.4" == Nothing
             , testProperty "parsing \"1.2.3.4 \" fails" $
                 fromStringAs anIP4 "1.2.3.4 " == Nothing
             , testProperty "parsing \"1.2. 3.4\" fails" $
                 fromStringAs anIP4 "1.2. 3.4 " == Nothing
             , testProperty "parsing \"1.2.3..4\" fails" $
                 fromStringAs anIP4 "1.2.3..4" == Nothing
             , testProperty "parsing \"1.2.3.\" fails" $
                 fromStringAs anIP4 "1.2.3." == Nothing
             , testProperty "parsing \"1.2.3\" fails" $
                 fromStringAs anIP4 "1.2.3" == Nothing
             , testProperty "parsing \"1.2.3.4.5\" fails" $
                 fromStringAs anIP4 "1.2.3.4.5" == Nothing
             , testProperty "parsing \"1a.2.3.4\" fails" $
                 fromStringAs anIP4 "1a.2.3.4" == Nothing
             ]
         , testGroup "IP6"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP6)
             , testProperty "parsing \"::\"" $
                 fromString "::" == Just anyIP6
             , testProperty "parsing \":::\" fails" $
                 fromStringAs anIP6 ":::" == Nothing
             , testProperty "parsing \" 1::2\" fails" $
                 fromStringAs anIP6 " 1::2" == Nothing
             , testProperty "parsing \"1::2 \" fails" $
                 fromStringAs anIP6 "1::2 " == Nothing
             , testProperty "parsing \"1: :2\" fails" $
                 fromStringAs anIP6 "1: :2" == Nothing
             , testProperty "parsing \"1::2:\" fails" $
                 fromStringAs anIP6 "1::2:" == Nothing
             , testProperty "parsing \":1::2\" fails" $
                 fromStringAs anIP6 ":1::2" == Nothing
             , testProperty "parsing \"1:abcde::2\" fails" $
                 fromStringAs anIP6 "1:abcde::2" == Nothing
             , testProperty "parsing \"1:abcg::2\" fails" $
                 fromStringAs anIP6 "1:abcg::2" == Nothing
             , testProperty "parsing \"1:2:3:4a:5:6:7:8\"" $
                 fromString "1:2:3:4a:5:6:7:8" ==
                   Just (IP6 0x000100020003004a0005000600070008)
             , testProperty "parsing \"1:2:3:4:5bc:6:7::\"" $
                 fromString "1:2:3:4:5bc:6:7::" ==
                   Just (IP6 0x000100020003000405bc000600070000)
             , testProperty "parsing \"::1:2:3ad9:4:5:6:7\"" $
                 fromString "::1:2:3ad9:4:5:6:7" ==
                   Just (IP6 0x000100023ad90004000500060007)
             , testProperty "parsing \"1:2:3:4:a5:6::\"" $
                 fromString "1:2:3:4:a5:6::" ==
                   Just (IP6 0x000100020003000400a5000600000000)
             , testProperty "parsing \"::1:2:3:4:5:6\"" $
                 fromString "::1:2:3:4:5:6" ==
                   Just (IP6 0x000100020003000400050006)
             , testProperty "parsing \"1::2:3:4:5:6\"" $
                 fromString "1::2:3:4:5:6" ==
                   Just (IP6 0x00010000000000020003000400050006)
             , testProperty "parsing \"1:2:3::4:5:6\"" $
                 fromString "1:2:3::4:5:6" ==
                   Just (IP6 0x00010002000300000000000400050006)
             , testProperty "parsing \"1:2:3::4:5::6\" fails" $
                 fromStringAs anIP6 "1:2:3::4:5::6" == Nothing
             , testProperty "parsing \"::1:2:3::4:5:6\" fails" $
                 fromStringAs anIP6 "::1:2:3::4:5:6" == Nothing
             , testProperty "parsing \"1:2::3:4:5g\" fails" $
                 fromStringAs anIP6 "1:2::3:4:5g" == Nothing
             , testProperty "parsing \"1:2:3:4:5:6:7:8g\" fails" $
                 fromStringAs anIP6 "1:2:3:4:5:6:7:8g" == Nothing
             ]
         , testGroup "IP"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP)
             ]
         , testGroup "Net4Addr"
             [ testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ Net4Addr)
             , testProperty "parsing \"11.22.33.44/14\"" $
                 case fromStringAs aNet4Addr "11.22.33.44/14" of
                   Just n | netHost n   == IP4 0x0b16212c
                          , netHostIx n == IP4 0x0002212c
                          , netPrefix n == IP4 0x0b140000
                          , netMask n   == IP4 0xfffc0000
                          , netLength n == 14
                          → True
                   _ → False
             , testProperty "parsing \"1.2.3/16\" fails" $
                 fromStringAs aNet4Addr "1.2.3/16" == Nothing
             , testProperty "parsing \"1.2 .3.4/16\" fails" $
                 fromStringAs aNet4Addr "1.2 .3.4/16" == Nothing
             , testProperty "parsing \"1.2.3.4/ 16\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/ 16" == Nothing
             , testProperty "parsing \"1.2.3.4 / 16\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4 / 16" == Nothing
             , testProperty "parsing \"1.2.3.4/16 \" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/16 " == Nothing
             , testProperty "parsing \"1.2.3.4/33\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/33" == Nothing
             , testProperty "parsing \"1.2.3.4/012\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/012" == Nothing
             ]
         , testGroup "Net6Addr"
             [ testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ Net6Addr)
             , testProperty "parsing \"1234:5678:9abc:def::/22\"" $
                 case fromStringAs aNet6Addr "1234:5678:9abc:def::/22" of
                   Just n | netHost n   == IP6 0x123456789abc0def0000000000000000
                          , netHostIx n == IP6 0x000002789abc0def0000000000000000
                          , netPrefix n == IP6 0x12345400000000000000000000000000
                          , netMask n   == IP6 0xfffffc00000000000000000000000000
                          , netLength n == 22
                          → True
                   _ → False
             , testProperty "parsing \"1:2:3/16\" fails" $
                 fromStringAs aNet6Addr "1:2:3/16" == Nothing
             , testProperty "parsing \"1:2 :3:4::/16\" fails" $
                 fromStringAs aNet6Addr "1:2 :3:4::/16" == Nothing
             , testProperty "parsing \"::1:2:3:4/ 16\" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4/ 16" == Nothing
             , testProperty "parsing \"::1:2:3:4 / 16\" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4 / 16" == Nothing
             , testProperty "parsing \"::1:2:3:4/16 \" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4/16 " == Nothing
             , testProperty "parsing \"::/129\" fails" $
                 fromStringAs aNet6Addr "::/129" == Nothing
             , testProperty "parsing \"::/01\" fails" $
                 fromStringAs aNet6Addr "::/01" == Nothing
             ]
         , testGroup "NetAddr"
             [ testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ NetAddr IP)
             ]
         , testGroup "InetPort"
             [ testProperty "print -> parse" $ \p →
                 fromString (toString p) == Just (p ∷ InetPort)
             , testProperty "parsing \"01\" fails" $
                 fromStringAs anInetPort "01" == Nothing
             , testProperty "parsing \"123456\" fails" $
                 fromStringAs anInetPort "123456" == Nothing
             , testProperty "parsing \"65536\" fails" $
                 fromStringAs anInetPort "65536" == Nothing
             ]
         , testGroup "Inet4Addr"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ Inet4Addr)
             , testProperty "parsing \"11.22.33.44:55\"" $
                 case fromString "11.22.33.44:55" of
                   Just (InetAddr (IP4 0x0b16212c) 55) → True
                   _ → False
             ]
         , testGroup "Inet6Addr"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ Inet6Addr)
             , testProperty "parsing \"[1234:5678:9abc:def::]:55\"" $
                 case fromString "[1234:5678:9abc:def::]:55" of
                   Just (InetAddr (IP6 0x123456789abc0def0000000000000000) 55) → True
                   _ → False
             ]
         , testGroup "InetAddr"
             [ testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ InetAddr IP)
             ]
         ]

