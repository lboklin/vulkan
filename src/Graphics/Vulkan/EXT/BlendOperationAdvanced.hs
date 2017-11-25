{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.BlendOperationAdvanced where

import Graphics.Vulkan.Pass( VkAccessFlagBits(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( VkBlendOp(..)
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
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )

pattern VK_BLEND_OP_LINEARLIGHT_EXT = VkBlendOp 1000148028
pattern VK_BLEND_OP_MULTIPLY_EXT = VkBlendOp 1000148012
pattern VK_BLEND_OP_DST_EXT = VkBlendOp 1000148002
pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VkBlendOp 1000148037
pattern VK_BLEND_OP_SRC_EXT = VkBlendOp 1000148001
pattern VK_BLEND_OP_DIFFERENCE_EXT = VkBlendOp 1000148021
-- ** VkBlendOverlapEXT
newtype VkBlendOverlapEXT = VkBlendOverlapEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendOverlapEXT where
  showsPrec _ VK_BLEND_OVERLAP_UNCORRELATED_EXT = showString "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
  showsPrec _ VK_BLEND_OVERLAP_DISJOINT_EXT = showString "VK_BLEND_OVERLAP_DISJOINT_EXT"
  showsPrec _ VK_BLEND_OVERLAP_CONJOINT_EXT = showString "VK_BLEND_OVERLAP_CONJOINT_EXT"
  showsPrec p (VkBlendOverlapEXT x) = showParen (p >= 11) (showString "VkBlendOverlapEXT " . showsPrec 11 x)

instance Read VkBlendOverlapEXT where
  readPrec = parens ( choose [ ("VK_BLEND_OVERLAP_UNCORRELATED_EXT", pure VK_BLEND_OVERLAP_UNCORRELATED_EXT)
                             , ("VK_BLEND_OVERLAP_DISJOINT_EXT", pure VK_BLEND_OVERLAP_DISJOINT_EXT)
                             , ("VK_BLEND_OVERLAP_CONJOINT_EXT", pure VK_BLEND_OVERLAP_CONJOINT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendOverlapEXT")
                        v <- step readPrec
                        pure (VkBlendOverlapEXT v)
                        )
                    )

pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2
pattern VK_BLEND_OP_INVERT_RGB_EXT = VkBlendOp 1000148024
pattern VK_BLEND_OP_SCREEN_EXT = VkBlendOp 1000148013
pattern VK_BLEND_OP_XOR_EXT = VkBlendOp 1000148011
pattern VK_BLEND_OP_HARDMIX_EXT = VkBlendOp 1000148030
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT = VkStructureType 1000148000
pattern VK_BLEND_OP_RED_EXT = VkBlendOp 1000148043
pattern VK_BLEND_OP_PLUS_EXT = VkBlendOp 1000148035
pattern VK_BLEND_OP_EXCLUSION_EXT = VkBlendOp 1000148022
pattern VK_BLEND_OP_SRC_OVER_EXT = VkBlendOp 1000148003
pattern VK_BLEND_OP_DST_IN_EXT = VkBlendOp 1000148006
pattern VK_BLEND_OP_PINLIGHT_EXT = VkBlendOp 1000148029
pattern VK_BLEND_OP_PLUS_CLAMPED_EXT = VkBlendOp 1000148036
pattern VK_BLEND_OP_SOFTLIGHT_EXT = VkBlendOp 1000148020
pattern VK_BLEND_OP_VIVIDLIGHT_EXT = VkBlendOp 1000148027
pattern VK_BLEND_OP_CONTRAST_EXT = VkBlendOp 1000148041
pattern VK_BLEND_OP_HSL_SATURATION_EXT = VkBlendOp 1000148032
pattern VK_BLEND_OP_LINEARBURN_EXT = VkBlendOp 1000148026
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = VkAccessFlagBits 0x80000
pattern VK_BLEND_OP_SRC_OUT_EXT = VkBlendOp 1000148007
pattern VK_BLEND_OP_INVERT_OVG_EXT = VkBlendOp 1000148042
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT = VkStructureType 1000148001
pattern VK_BLEND_OP_COLORDODGE_EXT = VkBlendOp 1000148017
pattern VK_BLEND_OP_LIGHTEN_EXT = VkBlendOp 1000148016
pattern VK_BLEND_OP_BLUE_EXT = VkBlendOp 1000148045

data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT =
  VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT{ vkSType :: VkStructureType 
                                                   , vkPNext :: Ptr Void 
                                                   , vkAdvancedBlendCoherentOperations :: VkBool32 
                                                   }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkAdvancedBlendCoherentOperations (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT = VkStructureType 1000148002
pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION =  0x2
pattern VK_BLEND_OP_DARKEN_EXT = VkBlendOp 1000148015
pattern VK_BLEND_OP_PLUS_DARKER_EXT = VkBlendOp 1000148038
pattern VK_BLEND_OP_DST_ATOP_EXT = VkBlendOp 1000148010
pattern VK_BLEND_OP_INVERT_EXT = VkBlendOp 1000148023
pattern VK_BLEND_OP_DST_OVER_EXT = VkBlendOp 1000148004
pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME =  "VK_EXT_blend_operation_advanced"
pattern VK_BLEND_OP_ZERO_EXT = VkBlendOp 1000148000
pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT = VkBlendOp 1000148034
pattern VK_BLEND_OP_COLORBURN_EXT = VkBlendOp 1000148018

data VkPipelineColorBlendAdvancedStateCreateInfoEXT =
  VkPipelineColorBlendAdvancedStateCreateInfoEXT{ vkSType :: VkStructureType 
                                                , vkPNext :: Ptr Void 
                                                , vkSrcPremultiplied :: VkBool32 
                                                , vkDstPremultiplied :: VkBool32 
                                                , vkBlendOverlap :: VkBlendOverlapEXT 
                                                }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineColorBlendAdvancedStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSrcPremultiplied (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkDstPremultiplied (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkBlendOverlap (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
pattern VK_BLEND_OP_MINUS_CLAMPED_EXT = VkBlendOp 1000148040

data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT =
  VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT{ vkSType :: VkStructureType 
                                                     , vkPNext :: Ptr Void 
                                                     , vkAdvancedBlendMaxColorAttachments :: Word32 
                                                     , vkAdvancedBlendIndependentBlend :: VkBool32 
                                                     , vkAdvancedBlendNonPremultipliedSrcColor :: VkBool32 
                                                     , vkAdvancedBlendNonPremultipliedDstColor :: VkBool32 
                                                     , vkAdvancedBlendCorrelatedOverlap :: VkBool32 
                                                     , vkAdvancedBlendAllOperations :: VkBool32 
                                                     }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
                                                                 <*> peek (ptr `plusPtr` 20)
                                                                 <*> peek (ptr `plusPtr` 24)
                                                                 <*> peek (ptr `plusPtr` 28)
                                                                 <*> peek (ptr `plusPtr` 32)
                                                                 <*> peek (ptr `plusPtr` 36)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkAdvancedBlendMaxColorAttachments (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkAdvancedBlendIndependentBlend (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkAdvancedBlendNonPremultipliedSrcColor (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkAdvancedBlendNonPremultipliedDstColor (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkAdvancedBlendCorrelatedOverlap (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkAdvancedBlendAllOperations (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
pattern VK_BLEND_OP_HSL_HUE_EXT = VkBlendOp 1000148031
pattern VK_BLEND_OP_MINUS_EXT = VkBlendOp 1000148039
pattern VK_BLEND_OP_SRC_ATOP_EXT = VkBlendOp 1000148009
pattern VK_BLEND_OP_HARDLIGHT_EXT = VkBlendOp 1000148019
pattern VK_BLEND_OP_GREEN_EXT = VkBlendOp 1000148044
pattern VK_BLEND_OP_SRC_IN_EXT = VkBlendOp 1000148005
pattern VK_BLEND_OP_HSL_COLOR_EXT = VkBlendOp 1000148033
pattern VK_BLEND_OP_DST_OUT_EXT = VkBlendOp 1000148008
pattern VK_BLEND_OP_OVERLAY_EXT = VkBlendOp 1000148014
pattern VK_BLEND_OP_LINEARDODGE_EXT = VkBlendOp 1000148025
