{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.SixteenBitStorage where

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


data VkPhysicalDevice16BitStorageFeaturesKHR =
  VkPhysicalDevice16BitStorageFeaturesKHR{ vkSType :: VkStructureType 
                                         , vkPNext :: Ptr Void 
                                         , vkStorageBuffer16BitAccess :: VkBool32 
                                         , vkUniformAndStorageBuffer16BitAccess :: VkBool32 
                                         , vkStoragePushConstant16 :: VkBool32 
                                         , vkStorageInputOutput16 :: VkBool32 
                                         }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDevice16BitStorageFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDevice16BitStorageFeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 20)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkUniformAndStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 24) (vkStoragePushConstant16 (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 28) (vkStorageInputOutput16 (poked :: VkPhysicalDevice16BitStorageFeaturesKHR))
