{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.IncrementalPresent where

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
import Graphics.Vulkan.Core( VkOffset2D(..)
                           , VkExtent2D(..)
                           , VkStructureType(..)
                           )

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION =  0x1

data VkPresentRegionKHR =
  VkPresentRegionKHR{ vkRectangleCount :: Word32 
                    , vkPRectangles :: Ptr VkRectLayerKHR 
                    }
  deriving (Eq, Ord, Show)
instance Storable VkPresentRegionKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentRegionKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRectangleCount (poked :: VkPresentRegionKHR))
                *> poke (ptr `plusPtr` 8) (vkPRectangles (poked :: VkPresentRegionKHR))

data VkPresentRegionsKHR =
  VkPresentRegionsKHR{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkSwapchainCount :: Word32 
                     , vkPRegions :: Ptr VkPresentRegionKHR 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkPresentRegionsKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPresentRegionsKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkPresentRegionsKHR))
                *> poke (ptr `plusPtr` 24) (vkPRegions (poked :: VkPresentRegionsKHR))
pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR = VkStructureType 1000084000

data VkRectLayerKHR =
  VkRectLayerKHR{ vkOffset :: VkOffset2D 
                , vkExtent :: VkExtent2D 
                , vkLayer :: Word32 
                }
  deriving (Eq, Ord, Show)
instance Storable VkRectLayerKHR where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkRectLayerKHR <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 8)
                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkRectLayerKHR))
                *> poke (ptr `plusPtr` 8) (vkExtent (poked :: VkRectLayerKHR))
                *> poke (ptr `plusPtr` 16) (vkLayer (poked :: VkRectLayerKHR))
pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME =  "VK_KHR_incremental_present"
