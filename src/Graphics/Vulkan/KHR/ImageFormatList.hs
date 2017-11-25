{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.ImageFormatList where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFormat(..)
                           , VkStructureType(..)
                           )

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = VkStructureType 1000147000
pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME =  "VK_KHR_image_format_list"
pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION =  0x1

data VkImageFormatListCreateInfoKHR =
  VkImageFormatListCreateInfoKHR{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkViewFormatCount :: Word32 
                                , vkPViewFormats :: Ptr VkFormat 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkImageFormatListCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageFormatListCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkViewFormatCount (poked :: VkImageFormatListCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPViewFormats (poked :: VkImageFormatListCreateInfoKHR))
