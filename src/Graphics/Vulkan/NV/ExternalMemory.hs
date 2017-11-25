{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.NV.ExternalMemory where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.NV.ExternalMemoryCapabilities( VkExternalMemoryHandleTypeFlagBitsNV(..)
                                                    , VkExternalMemoryHandleTypeFlagsNV(..)
                                                    )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )


data VkExternalMemoryImageCreateInfoNV =
  VkExternalMemoryImageCreateInfoNV{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfoNV))

data VkExportMemoryAllocateInfoNV =
  VkExportMemoryAllocateInfoNV{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkExportMemoryAllocateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfoNV <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfoNV))
