{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
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
