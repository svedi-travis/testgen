module Lib where

import Prelude hiding (id)

import Data.Bits

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
  , activatedMachines :: [String]
  , trialActivation :: Bool
  , maxnoofmachines :: Integer
  , allowedMachines :: [String]
  , dataObjects :: [DataObject]
  , signDate :: Integer
  }

data DataObject = DataObject Integer String Integer String

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

fieldsToReturnBit :: Field -> Int
fieldsToReturnBit ProductId         = error "Mandatory field"
fieldsToReturnBit Id                = 1
fieldsToReturnBit Key               = 2
fieldsToReturnBit Created           = error "Mandatory field"
fieldsToReturnBit Expires           = error "Mandatory field"
fieldsToReturnBit Period            = error "Mandatory field"
fieldsToReturnBit F1                = error "Mandatory field"
fieldsToReturnBit F2                = error "Mandatory field"
fieldsToReturnBit F3                = error "Mandatory field"
fieldsToReturnBit F4                = error "Mandatory field"
fieldsToReturnBit F5                = error "Mandatory field"
fieldsToReturnBit F6                = error "Mandatory field"
fieldsToReturnBit F7                = error "Mandatory field"
fieldsToReturnBit F8                = error "Mandatory field"
fieldsToReturnBit Notes             = 3
fieldsToReturnBit Block             = error "Mandatory field"
fieldsToReturnBit GlobalId          = 4
fieldsToReturnBit Customer          = 5
fieldsToReturnBit ActivatedMachines = 6
fieldsToReturnBit TrialActivation   = error "Mandatory field"
fieldsToReturnBit Maxnoofmachines   = 8
fieldsToReturnBit AllowedMachines   = 7
fieldsToReturnBit DataObjects       = 9
fieldsToReturnBit SignDate          = error "Mandatory field"

withField :: Bits a => Field -> a -> a
withField field x = x `setBit` (fieldsToReturnBit field)

withoutField :: Bits a => Field -> a -> a
withoutField field x = x `clearBit` (fieldsToReturnBit field)

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

mandatory :: String -> String -> Key -> String
mandatory testName license expected = concat
  [ "TEST(" ++ testName ++ ", Mandatory) {\n"
  , "  std::string license{" ++ show license ++ "};\n"
  , "  optional<LicenseKey> license_key = LicenseKey::make_unsafe(license);\n"
  , "\n"
  , "  ASSERT_TRUE(license_key.has_value()) << \"Failed to construct LicenseKey object\";\n"
  , "\n"
  , "  EXPECT_EQ(license_key->get_product_id(), " ++ (show $ productId expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_created(), " ++ (show $ created expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_expires(), " ++ (show $ expires expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_period(), " ++ (show $ period expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_block(), " ++ (show $ block expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_trial_activation(), " ++ (show $ trialActivation expected) ++ ");\n"
  , "  EXPECT_EQ(license_key->get_sign_date(), " ++ (show $ signDate expected) ++ ");\n"
  , "}\n"
  ]

features :: String -> String -> Key -> String
features testName license expected = concat
  [ "TEST(" ++ testName ++ ", Features) {\n"
  , "  FeatureChecker f;\n"
  , "  f.license = " ++ show license ++ ";\n"
  , "  f.f1 = " ++ (show $ f1 expected) ++ ";\n"
  , "  f.f2 = " ++ (show $ f2 expected) ++ ";\n"
  , "  f.f3 = " ++ (show $ f3 expected) ++ ";\n"
  , "  f.f4 = " ++ (show $ f4 expected) ++ ";\n"
  , "  f.f5 = " ++ (show $ f5 expected) ++ ";\n"
  , "  f.f6 = " ++ (show $ f6 expected) ++ ";\n"
  , "  f.f7 = " ++ (show $ f7 expected) ++ ";\n"
  , "  f.f8 = " ++ (show $ f8 expected) ++ ";\n"
  , "\n"
  , "  f.check();\n"
  , "}\n"
  ]

optional :: String -> String -> String -> Field -> Key -> String
optional testName with_field without_field field expected = concat
  [ "TEST(" ++ testName ++ ", " ++ show field ++ ") {\n"
  , "  std::string license1{" ++ show with_field ++ "};\n"
  , "  optional<LicenseKey> license_key_1 = LicenseKey::make_unsafe(license1);\n"
  , "\n"
  , "  std::string license2{" ++ show without_field ++ "};\n"
  , "  optional<LicenseKey> license_key_2 = LicenseKey::make_unsafe(license2);\n"
  , "  ASSERT_TRUE(license_key_1.has_value()) << \"Failed to construct LicenseKey object\";\n"
  , "  ASSERT_TRUE(license_key_2.has_value()) << \"Failed to construct LicenseKey object\";\n"
  , "\n"
  , optional_specific field expected
  , "}\n"
  ]

optional_specific DataObjects expected = concat
int object_count = 0;
(flip concatMap) (zip [0..] $ dataObjects expected)
                 (\(i, _) -> "bool seen_" ++ (show i) ++ " = false;\n")

for (size_t i = 0; i < license_key_1.size(); ++i) {
  DataObject const& o = *license_key_1.get_data_objects();
  for dataObjects expected (\object ->
    if ( o.id == object_id && o.name == object_name
      && o.ind_value == object_int_value && o.string_value == object_string_value)
    {
      if (!seen_i) { ++object_count; }
      seen_i = true;
    }
  )
}

EXPECT_EQ(object_count, length dataObjects expected);
EXPECT_EQ(license_key_2->get_data_objects()->size(), 0);


optional_specific _           expected = concat
  [ "  EXPECT_TRUE(license_key_1->" ++ accessor field ++ ".has_value());\n"
--  , "  EXPECT_FALSE(license_key_2->" ++ accessor field ++ ".has_value());\n"
  ]
