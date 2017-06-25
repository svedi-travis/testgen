{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad
import qualified Data.Aeson             as Aeson
import qualified Data.Aeson.Lens        as Aeson
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Base64 as B
import           Data.List              as List
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Network.Wreq           as Wreq
import           System.Random

import qualified Lib as SKM

k1 :: SKM.Key
k1 = SKM.KeyContents
  { SKM.productId = 3720
  , SKM.id = 1
  , SKM.key = "BIGYF-VFZPK-CWZLH-RXVRL"
  , SKM.created = 0
  , SKM.expires = 0
  , SKM.period = 30
  , SKM.f1 = False
  , SKM.f2 = True
  , SKM.f3 = False
  , SKM.f4 = True
  , SKM.f5 = False
  , SKM.f6 = True
  , SKM.f7 = False
  , SKM.f8 = True
  , SKM.notes = ""
  , SKM.block = False
  , SKM.globalId = 0
  , SKM.customer = ()
  , SKM.activatedMachines = []
  , SKM.trialActivation = False
  , SKM.maxnoofmachines = 0
  , SKM.allowedMachines = []
  , SKM.dataObjects = []
  , SKM.signDate = 0
  }

k2 :: SKM.Key
k2 = SKM.KeyContents
  { SKM.productId = 3720
  , SKM.id = 2
  , SKM.key = "FDUXP-HSYRL-HFYWV-DGBZU"
  , SKM.created = 0
  , SKM.expires = 0
  , SKM.period = 40
  , SKM.f1 = True
  , SKM.f2 = False
  , SKM.f3 = True
  , SKM.f4 = False
  , SKM.f5 = True
  , SKM.f6 = False
  , SKM.f7 = True
  , SKM.f8 = False
  , SKM.notes = "TestKey1"
  , SKM.block = False
  , SKM.globalId = 0
  , SKM.customer = ()
  , SKM.activatedMachines = []
  , SKM.trialActivation = False
  , SKM.maxnoofmachines = 0
  , SKM.allowedMachines = []
  , SKM.dataObjects = []
  , SKM.signDate = 0
  }

k3 :: SKM.Key
k3 = SKM.KeyContents
  { SKM.productId = 3720
  , SKM.id = 3
  , SKM.key = "HHLMY-JJIHL-CCJOZ-FQOPV"
  , SKM.created = 0
  , SKM.expires = 0
  , SKM.period = 30
  , SKM.f1 = True
  , SKM.f2 = True
  , SKM.f3 = True
  , SKM.f4 = True
  , SKM.f5 = False
  , SKM.f6 = False
  , SKM.f7 = False
  , SKM.f8 = False
  , SKM.notes = "Second Test Key"
  , SKM.block = False
  , SKM.globalId = 0
  , SKM.customer = ()
  , SKM.activatedMachines = []
  , SKM.trialActivation = False
  , SKM.maxnoofmachines = 0
  , SKM.allowedMachines = []
  , SKM.dataObjects = []
  , SKM.signDate = 0
  }

k4 :: SKM.Key
k4 = SKM.KeyContents
  { SKM.productId = 3720
  , SKM.id = 4
  , SKM.key = "BLKWA-CMBGP-VPBKO-IQGEE"
  , SKM.created = 0
  , SKM.expires = 0
  , SKM.period = 30
  , SKM.f1 = False
  , SKM.f2 = False
  , SKM.f3 = False
  , SKM.f4 = False
  , SKM.f5 = True
  , SKM.f6 = True
  , SKM.f7 = True
  , SKM.f8 = True
  , SKM.notes = "One Data Object"
  , SKM.block = False
  , SKM.globalId = 0
  , SKM.customer = ()
  , SKM.activatedMachines = []
  , SKM.trialActivation = False
  , SKM.maxnoofmachines = 0
  , SKM.allowedMachines = []
  , SKM.dataObjects = [DataObject 657 "te st" 123 "test test"]
  , SKM.signDate = 0
  }

k5 :: SKM.Key
k5 = SKM.KeyContents
  { SKM.productId = 3720
  , SKM.id = 5
  , SKM.key = "BNDAZ-PBIMV-AJRTZ-LXYJT"
  , SKM.created = 0
  , SKM.expires = 0
  , SKM.period = 30
  , SKM.f1 = True
  , SKM.f2 = False
  , SKM.f3 = True
  , SKM.f4 = False
  , SKM.f5 = False
  , SKM.f6 = True
  , SKM.f7 = False
  , SKM.f8 = True
  , SKM.notes = "Two Data Objects"
  , SKM.block = False
  , SKM.globalId = 0
  , SKM.customer = ()
  , SKM.activatedMachines = []
  , SKM.trialActivation = False
  , SKM.maxnoofmachines = 0
  , SKM.allowedMachines = []
  , SKM.dataObjects = [DataObject 658 "o1" 0 "object", DataObject 659 "o2" 1 ""]
  , SKM.signDate = 0
  }

header :: String
header = concat
 [ "#include <iostream>\n"
 , "\n"
 , "#include \"gtest/gtest.h\"\n"
 , "\n"
 , "#include \"basic_SKM.hpp\"\n"
 , "#include \"LicenseKey.hpp\"\n"
 , "#include \"LicenseKeyChecker.hpp\"\n"
 , "#include \"RawLicenseKey.hpp\"\n"
 , "\n"
 , "#include \"RequestHandler_static.hpp\"\n"
 , "#include \"SignatureVerifier_OpenSSL.hpp\"\n"
 , "\n"
 , "using namespace serialkeymanager_com;\n"
 , "\n"
 , "struct FeatureChecker\n"
 , "{\n"
 , "  std::string license;\n"
 , "  bool f1;\n"
 , "  bool f2;\n"
 , "  bool f3;\n"
 , "  bool f4;\n"
 , "  bool f5;\n"
 , "  bool f6;\n"
 , "  bool f7;\n"
 , "  bool f8;\n"
 , "\n"
 , "  void check() const\n"
 , "  {\n"
 , "    optional<LicenseKey> license_key = LicenseKey::make_unsafe(license);\n"
 , "\n"
 , "    ASSERT_TRUE(license_key.has_value()) << \"Failed to construct LicenseKey object\";\n"
 , "\n"
 , "    EXPECT_EQ(license_key->get_f1(), f1);\n"
 , "    EXPECT_EQ(license_key->get_f2(), f2);\n"
 , "    EXPECT_EQ(license_key->get_f3(), f3);\n"
 , "    EXPECT_EQ(license_key->get_f4(), f4);\n"
 , "    EXPECT_EQ(license_key->get_f5(), f5);\n"
 , "    EXPECT_EQ(license_key->get_f6(), f6);\n"
 , "    EXPECT_EQ(license_key->get_f7(), f7);\n"
 , "    EXPECT_EQ(license_key->get_f8(), f8);\n"
 , "\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(1), f1);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(2), f2);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(3), f3);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(4), f4);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(5), f5);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(6), f6);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(7), f7);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_feature(8), f8);\n"
 , "\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(1), !f1);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(2), !f2);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(3), !f3);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(4), !f4);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(5), !f5);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(6), !f6);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(7), !f7);\n"
 , "    EXPECT_EQ((bool)license_key->check().has_not_feature(8), !f8);\n"
 , "  }\n"
 , "};\n"
 , "\n"
 , "int\n"
 , "main(int argc, char **argv)\n"
 , "{\n"
 , "  ::testing::InitGoogleTest(&argc, argv);\n"
 , "  return RUN_ALL_TESTS();\n"
 , "}\n"
 , "\n"
 ]

main :: IO ()
main = do
  let keys = [ ("Key1", k1), ("Key2", k2), ("Key3", k3), ("Key4", k4), ("Key5", k5)]
  putStrLn header
  forM_ keys (\(name, key) -> generateTestCaseForKey name key >>= putStrLn)

generateTestCaseForKey :: String -> SKM.Key -> IO String
generateTestCaseForKey testName key = do
  licenseKeyB <- activate key

  let Just created  = extractField licenseKeyB "Created"
  let Just expires  = extractField licenseKeyB "Expires"
  let Just signDate = extractField licenseKeyB "SignDate"

  let licenseKey = Text.unpack . Text.decodeUtf8 $ licenseKeyB
  let key' = key { SKM.created = created, SKM.expires = expires, SKM.signDate = signDate }

  let basic = 
        [ SKM.mandatory testName licenseKey key'
        , SKM.features  testName licenseKey key'
        ]
  optional <- forM SKM.optionalFields (\field -> do
                features1  <- fmap (SKM.withField field) $ randomRIO (1, 8191) :: IO Integer
                features2  <- fmap (SKM.withField field) $ randomRIO (1, 8191) :: IO Integer
                lk_with    <- fmap (Text.unpack . Text.decodeUtf8) $ activateWithFields features1 key'
                lk_without <- fmap (Text.unpack . Text.decodeUtf8) $ activateWithFields features2 key'
                return $ SKM.optional testName lk_with lk_without field key'
              )
  return . concat . List.intersperse "\n" $ (basic ++ optional)

activate :: SKM.Key -> IO B.ByteString
activate = activateWithFields 0

activateWithFields :: Integer -> SKM.Key -> IO B.ByteString
activateWithFields fields key = do
  let url = concat
         [ "https://serialkeymanager.com/api/key/Activate"
         , "?token=WyI1NzQiLCJUclYzcEJLQnFvaUpsd1oyVTNFN3dVK2M4QWZNcGY0WjNxSDF4RitLIl0="
         , "&ProductId=" ++ (show $ SKM.productId key)
         , "&Key=" ++ SKM.key key
         , "&Sign=true"
         , "&SignMethod=1"
         , "&FieldsToReturn=" ++ (show $ fields)
         , "&MachineCode=machine1"
         ]

  r <- Wreq.get url
  let Just licenseKey = extractLicenseKey =<< (r ^? Wreq.responseBody . Aeson.key "licenseKey")
  return licenseKey

extractLicenseKey :: Aeson.Value -> Maybe B.ByteString
extractLicenseKey v = do
  base64 <- case v of
              Aeson.String s -> Just $ Text.encodeUtf8 s
              _              -> Nothing
  case B.decode base64 of
    Left _  -> Nothing
    Right s -> Just s

extractField licenseKey field = do
  f <- licenseKey ^? Aeson.key field
  case Aeson.fromJSON f of
    Aeson.Success x -> Just x
    _               -> Nothing
