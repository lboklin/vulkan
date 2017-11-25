{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.GetMemoryRequirements2 where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
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
import Graphics.Vulkan.Memory( VkMemoryRequirements(..)
                             )
import Graphics.Vulkan.Image( VkImage(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.SparseResourceMemoryManagement( VkSparseImageFormatFlagBits(..)
                                                     , VkSparseImageFormatFlags(..)
                                                     , VkSparseImageFormatProperties(..)
                                                     , VkSparseImageMemoryRequirements(..)
                                                     )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkExtent3D(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )


data VkSparseImageMemoryRequirements2KHR =
  VkSparseImageMemoryRequirements2KHR{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkMemoryRequirements :: VkSparseImageMemoryRequirements 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkSparseImageMemoryRequirements2KHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSparseImageMemoryRequirements2KHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageMemoryRequirements2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageMemoryRequirements2KHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkSparseImageMemoryRequirements2KHR))
-- ** vkGetImageMemoryRequirements2KHR
foreign import ccall "vkGetImageMemoryRequirements2KHR" vkGetImageMemoryRequirements2KHR ::
  VkDevice ->
  Ptr VkImageMemoryRequirementsInfo2KHR ->
    Ptr VkMemoryRequirements2KHR -> IO ()

data VkMemoryRequirements2KHR =
  VkMemoryRequirements2KHR{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkMemoryRequirements :: VkMemoryRequirements 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkMemoryRequirements2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements2KHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryRequirements2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryRequirements2KHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryRequirements (poked :: VkMemoryRequirements2KHR))

data VkImageSparseMemoryRequirementsInfo2KHR =
  VkImageSparseMemoryRequirementsInfo2KHR{ vkSType :: VkStructureType 
                                         , vkPNext :: Ptr Void 
                                         , vkImage :: VkImage 
                                         }
  deriving (Eq, Ord, Show)
instance Storable VkImageSparseMemoryRequirementsInfo2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSparseMemoryRequirementsInfo2KHR <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSparseMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageSparseMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageSparseMemoryRequirementsInfo2KHR))

data VkImageMemoryRequirementsInfo2KHR =
  VkImageMemoryRequirementsInfo2KHR{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkImage :: VkImage 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkImageMemoryRequirementsInfo2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageMemoryRequirementsInfo2KHR <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkImageMemoryRequirementsInfo2KHR))
-- ** vkGetBufferMemoryRequirements2KHR
foreign import ccall "vkGetBufferMemoryRequirements2KHR" vkGetBufferMemoryRequirements2KHR ::
  VkDevice ->
  Ptr VkBufferMemoryRequirementsInfo2KHR ->
    Ptr VkMemoryRequirements2KHR -> IO ()

data VkBufferMemoryRequirementsInfo2KHR =
  VkBufferMemoryRequirementsInfo2KHR{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkBuffer :: VkBuffer 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkBufferMemoryRequirementsInfo2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferMemoryRequirementsInfo2KHR <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryRequirementsInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBufferMemoryRequirementsInfo2KHR))
-- ** vkGetImageSparseMemoryRequirements2KHR
foreign import ccall "vkGetImageSparseMemoryRequirements2KHR" vkGetImageSparseMemoryRequirements2KHR ::
  VkDevice ->
  Ptr VkImageSparseMemoryRequirementsInfo2KHR ->
    Ptr Word32 -> Ptr VkSparseImageMemoryRequirements2KHR -> IO ()
