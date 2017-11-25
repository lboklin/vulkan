{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.SamplerFilterMinmax where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.DeviceInitialization( VkFormatFeatureFlagBits(..)
                                           )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )

-- ** VkSamplerReductionModeEXT
newtype VkSamplerReductionModeEXT = VkSamplerReductionModeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerReductionModeEXT where
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = showString "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_MIN_EXT = showString "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
  showsPrec _ VK_SAMPLER_REDUCTION_MODE_MAX_EXT = showString "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
  showsPrec p (VkSamplerReductionModeEXT x) = showParen (p >= 11) (showString "VkSamplerReductionModeEXT " . showsPrec 11 x)

instance Read VkSamplerReductionModeEXT where
  readPrec = parens ( choose [ ("VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT", pure VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT)
                             , ("VK_SAMPLER_REDUCTION_MODE_MIN_EXT", pure VK_SAMPLER_REDUCTION_MODE_MIN_EXT)
                             , ("VK_SAMPLER_REDUCTION_MODE_MAX_EXT", pure VK_SAMPLER_REDUCTION_MODE_MAX_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerReductionModeEXT")
                        v <- step readPrec
                        pure (VkSamplerReductionModeEXT v)
                        )
                    )

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VkSamplerReductionModeEXT 0

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT = VkSamplerReductionModeEXT 1

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT = VkSamplerReductionModeEXT 2

data VkSamplerReductionModeCreateInfoEXT =
  VkSamplerReductionModeCreateInfoEXT{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkReductionMode :: VkSamplerReductionModeEXT 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkSamplerReductionModeCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerReductionModeCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerReductionModeCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkReductionMode (poked :: VkSamplerReductionModeCreateInfoEXT))
pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT = VkStructureType 1000130001
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VkFormatFeatureFlagBits 0x10000

data VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =
  VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT{ vkSType :: VkStructureType 
                                                  , vkPNext :: Ptr Void 
                                                  , vkFilterMinmaxSingleComponentFormats :: VkBool32 
                                                  , vkFilterMinmaxImageComponentMapping :: VkBool32 
                                                  }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                              <*> peek (ptr `plusPtr` 8)
                                                              <*> peek (ptr `plusPtr` 16)
                                                              <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkFilterMinmaxSingleComponentFormats (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkFilterMinmaxImageComponentMapping (poked :: VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME =  "VK_EXT_sampler_filter_minmax"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT = VkStructureType 1000130000
pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION =  0x1
