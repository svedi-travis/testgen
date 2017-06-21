module Lib where

import Prelude hiding (id)

data Key = KeyContents
  { productId :: Integer
  , id :: Integer
  , key :: String
  , created :: Integer
  , expires :: Integer
  , period :: Integer
  , f1 :: Bool
  , f2 :: Bool
  , f3 :: Bool
  , f4 :: Bool
  , f5 :: Bool
  , f6 :: Bool
  , f7 :: Bool
  , f8 :: Bool
  , notes :: String
  , block :: Bool
  , globalId :: Integer
  , customer :: ()
  , activatedMachines :: ()
  , trialActivation :: Bool
  , maxnoofmachines :: Integer
  , allowedMachines :: [String]
  , dataObjects :: ()
  , signDate :: Integer
  }

data Field =
    ProductId
  | Id
  | Key
  | Created
  | Expires
  | Period
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | Notes
  | Block
  | GlobalId
  | Customer
  | ActivatedMachines
  | TrialActivation
  | Maxnoofmachines
  | AllowedMachines
  | DataObjects
  | SignDate
  deriving (Eq, Ord, Show)

--mandatoryFields :: [Field]
--mandatoryFields = []

optionalFields :: [Field]
optionalFields = [ Id, Key, Notes, GlobalId, Customer, ActivatedMachines
                 , Maxnoofmachines, AllowedMachines, DataObjects
                 ]

accessor :: Field -> String
accessor ProductId         = "get_product_id()"
accessor Id                = "get_id()"
accessor Key               = "get_key()"
accessor Created           = "get_created()"
accessor Expires           = "get_expires()"
accessor Period            = "get_period()"
accessor F1                = "get_f1()"
accessor F2                = "get_f2()"
accessor F3                = "get_f3()"
accessor F4                = "get_f4()"
accessor F5                = "get_f5()"
accessor F6                = "get_f6()"
accessor F7                = "get_f7()"
accessor F8                = "get_f8()"
accessor Notes             = "get_notes()"
accessor Block             = "get_block()"
accessor GlobalId          = "get_global_id()"
accessor Customer          = "get_customer()"
accessor ActivatedMachines = "get_activated_machines()"
accessor TrialActivation   = "get_trial_activation()"
accessor Maxnoofmachines   = "get_maxnoofmachines()"
accessor AllowedMachines   = "get_allowed_machines()"
accessor DataObjects       = "get_data_objects()"
accessor SignDate          = "get_sign_date()"

k1 :: Key
k1 = KeyContents
  { productId = 3720
  , id = 0
  , key = "asdf"
  , created = 0
  , expires = 0
  , period = 0
  , f1 = False
  , f2 = True
  , f3 = False
  , f4 = True
  , f5 = False
  , f6 = True
  , f7 = False
  , f8 = True
  , notes = "asdf"
  , block = False
  , globalId = 0
  , customer = ()
  , activatedMachines = ()
  , trialActivation = False
  , maxnoofmachines = 100
  , allowedMachines = []
  , dataObjects = ()
  , signDate = 0
  }

mandatory :: String -> String -> Key -> String
mandatory testName license expected = 
    "TEST(" ++ testName ++ ", Mandatory) {\n"
 ++ "  std::string license{\"" ++ license ++ "\"};\n"
 ++ "  optional<LicenseKey> license_key = LicenseKey::make_unsafe(license);\n"
 ++ "\n"
 ++ "  ASSERT_TRUE(license_key.has_value()) << \"Failed to construct LicenseKey object\";\n"
 ++ "\n"
 ++ "  EXPECT_EQ(license_key->get_product_id(), " ++ (show $ productId expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_created(), " ++ (show $ created expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_expires(), " ++ (show $ expires expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_period(), " ++ (show $ period expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_block(), " ++ (show $ block expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_trial_activation(), " ++ (show $ trialActivation expected) ++ ");\n"
 ++ "  EXPECT_EQ(license_key->get_sign_date(), " ++ (show $ signDate expected) ++ ");\n"
 ++ "}\n"

features :: String -> String -> Key -> String
features testName license expected = 
    "TEST(" ++ testName ++ ", Features) {\n"
 ++ "  FeatureChecker f;\n"
 ++ "  f.license = \"" ++ license ++ "\";\n"
 ++ "  f.f1 = " ++ (show $ f1 expected) ++ ";\n"
 ++ "  f.f2 = " ++ (show $ f2 expected) ++ ";\n"
 ++ "  f.f3 = " ++ (show $ f3 expected) ++ ";\n"
 ++ "  f.f4 = " ++ (show $ f4 expected) ++ ";\n"
 ++ "  f.f5 = " ++ (show $ f5 expected) ++ ";\n"
 ++ "  f.f6 = " ++ (show $ f6 expected) ++ ";\n"
 ++ "  f.f7 = " ++ (show $ f7 expected) ++ ";\n"
 ++ "  f.f8 = " ++ (show $ f8 expected) ++ ";\n"
 ++ "\n"
 ++ "  f.check();\n"
 ++ "}\n"

optional :: String -> String -> String -> Field -> Key -> String
optional testName with_field without_field field expected =
    "TEST(" ++ testName ++ ", " ++ show field ++ ") {\n"
 ++ "  std::string license1{\"" ++ with_field ++ "\"};\n"
 ++ "  optional<LicenseKey> license_key_1 = LicenseKey::make_unsafe(license1);\n"
 ++ "\n"
 ++ "  std::string license2{\"" ++ without_field ++ "\"};\n"
 ++ "  optional<LicenseKey> license_key_2 = LicenseKey::make_unsafe(license2);\n"
 ++ "  ASSERT_TRUE(license_key_1.has_value()) << \"Failed to construct LicenseKey object\";\n"
 ++ "  ASSERT_TRUE(license_key_2.has_value()) << \"Failed to construct LicenseKey object\";\n"
 ++ "\n"
 ++ "  EXPECT_TRUE(license_key_1->" ++ accessor field ++ ".has_value());\n"
 ++ "  EXPECT_FALSE(license_key_2->" ++ accessor field ++ ".has_value());\n"
 ++ "}\n"

generateTest :: String -> Key -> String
generateTest testName expected =
    mandatory testName "license goes here!" expected
 ++ features testName "liceens goes here!" expected
 ++ (flip concatMap) optionalFields (\field ->
         optional testName "license with field here!" "license without field here!" field expected
      ++ "\n"
    )
