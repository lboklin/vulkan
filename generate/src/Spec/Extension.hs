module Spec.Extension where

import           Data.Int (Int32)

data Extension = Extension{ eName         :: String
                          , eNumber       :: Int
                          , eSupported    :: String
                          , eProtect      :: Maybe String
                          , eAuthor       :: Maybe String
                          , eContact      :: Maybe String
                          , eEnums        :: [ExtensionEnum]
                          , eConstants    :: [ExtensionConstant]
                          , eCommandNames :: [String]
                          , eTypeNames    :: [String]
                          }
  deriving(Show)

data Direction = Negative
               | Positive
  deriving(Show)

data ExtensionEnum = ExtensionEnum{ eeName      :: String
                                  , eeExtends   :: String
                                  , eeOffset    :: Int32
                                  , eeDirection :: Direction
                                  }
  deriving(Show)

data ExtensionConstant = ExtensionConstant{ ecName  :: String
                                          , ecValue :: Either String Integer
                                          }
  deriving(Show)

allExtensionNames :: Extension -> [String]
allExtensionNames e =
  -- TODO: Uncomment
  -- eeName <$> eExtensionEnums e ++
  -- evName <$> eConstants e ++
  eCommandNames e ++
  eTypeNames e
