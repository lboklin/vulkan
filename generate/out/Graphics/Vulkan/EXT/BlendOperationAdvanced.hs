{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.BlendOperationAdvanced where

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
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )

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
