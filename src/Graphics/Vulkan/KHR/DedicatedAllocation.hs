{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.DedicatedAllocation where

import Graphics.Vulkan.Buffer( VkBuffer(..)
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
import Graphics.Vulkan.Image( VkImage(..)
                            )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )


data VkMemoryDedicatedRequirementsKHR =
  VkMemoryDedicatedRequirementsKHR{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkPrefersDedicatedAllocation :: VkBool32 
                                  , vkRequiresDedicatedAllocation :: VkBool32 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkMemoryDedicatedRequirementsKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedRequirementsKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedRequirementsKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedRequirementsKHR))
                *> poke (ptr `plusPtr` 16) (vkPrefersDedicatedAllocation (poked :: VkMemoryDedicatedRequirementsKHR))
                *> poke (ptr `plusPtr` 20) (vkRequiresDedicatedAllocation (poked :: VkMemoryDedicatedRequirementsKHR))

data VkMemoryDedicatedAllocateInfoKHR =
  VkMemoryDedicatedAllocateInfoKHR{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkImage :: VkImage 
                                  , vkBuffer :: VkBuffer 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkMemoryDedicatedAllocateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedAllocateInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedAllocateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedAllocateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkMemoryDedicatedAllocateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkMemoryDedicatedAllocateInfoKHR))
