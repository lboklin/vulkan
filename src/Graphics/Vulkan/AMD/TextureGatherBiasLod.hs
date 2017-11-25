{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
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
pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD = VkStructureType 1000041000
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION =  0x1
pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME =  "VK_AMD_texture_gather_bias_lod"
