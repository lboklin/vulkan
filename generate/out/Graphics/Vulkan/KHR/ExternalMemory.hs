{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.ExternalMemory where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )
import Graphics.Vulkan.KHR.ExternalMemoryCapabilities( VkExternalMemoryHandleTypeFlagBitsKHR(..)
                                                     , VkExternalMemoryHandleTypeFlagsKHR(..)
                                                     )


data VkExportMemoryAllocateInfoKHR =
  VkExportMemoryAllocateInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkExportMemoryAllocateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfoKHR))

data VkExternalMemoryImageCreateInfoKHR =
  VkExternalMemoryImageCreateInfoKHR{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryImageCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfoKHR))

data VkExternalMemoryBufferCreateInfoKHR =
  VkExternalMemoryBufferCreateInfoKHR{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryBufferCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryBufferCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryBufferCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryBufferCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryBufferCreateInfoKHR))
