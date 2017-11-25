{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.AMD.TextureGatherBiasLod where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )


data VkTextureLODGatherFormatPropertiesAMD =
  VkTextureLODGatherFormatPropertiesAMD{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkSupportsTextureGatherLODBiasAMD :: VkBool32 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkTextureLODGatherFormatPropertiesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkTextureLODGatherFormatPropertiesAMD <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkTextureLODGatherFormatPropertiesAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkTextureLODGatherFormatPropertiesAMD))
                *> poke (ptr `plusPtr` 16) (vkSupportsTextureGatherLODBiasAMD (poked :: VkTextureLODGatherFormatPropertiesAMD))
