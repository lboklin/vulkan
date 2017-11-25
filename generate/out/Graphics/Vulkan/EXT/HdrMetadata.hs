{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.EXT.HdrMetadata where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
                                    )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkStructureType(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      )

-- | Chromaticity coordinate
data VkXYColorEXT =
  VkXYColorEXT{ vkX :: CFloat 
              , vkY :: CFloat 
              }
  deriving (Eq, Ord, Show)
instance Storable VkXYColorEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkXYColorEXT <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkXYColorEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkXYColorEXT))
-- ** vkSetHdrMetadataEXT
foreign import ccall "vkSetHdrMetadataEXT" vkSetHdrMetadataEXT ::
  VkDevice ->
  Word32 -> Ptr VkSwapchainKHR -> Ptr VkHdrMetadataEXT -> IO ()

data VkHdrMetadataEXT =
  VkHdrMetadataEXT{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkDisplayPrimaryRed :: VkXYColorEXT 
                  , vkDisplayPrimaryGreen :: VkXYColorEXT 
                  , vkDisplayPrimaryBlue :: VkXYColorEXT 
                  , vkWhitePoint :: VkXYColorEXT 
                  , vkMaxLuminance :: CFloat 
                  , vkMinLuminance :: CFloat 
                  , vkMaxContentLightLevel :: CFloat 
                  , vkMaxFrameAverageLightLevel :: CFloat 
                  }
  deriving (Eq, Ord, Show)
instance Storable VkHdrMetadataEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkHdrMetadataEXT <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 52)
                              <*> peek (ptr `plusPtr` 56)
                              <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 16) (vkDisplayPrimaryRed (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 24) (vkDisplayPrimaryGreen (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 32) (vkDisplayPrimaryBlue (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 40) (vkWhitePoint (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 48) (vkMaxLuminance (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 52) (vkMinLuminance (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 56) (vkMaxContentLightLevel (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 60) (vkMaxFrameAverageLightLevel (poked :: VkHdrMetadataEXT))
