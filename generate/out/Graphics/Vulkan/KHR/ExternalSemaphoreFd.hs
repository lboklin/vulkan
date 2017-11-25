{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.ExternalSemaphoreFd where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.ExternalSemaphoreCapabilities( VkExternalSemaphoreHandleTypeFlagBitsKHR(..)
                                                        )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Graphics.Vulkan.KHR.ExternalSemaphore( VkSemaphoreImportFlagsKHR(..)
                                            , VkSemaphoreImportFlagBitsKHR(..)
                                            )
import Foreign.C.Types( CInt
                      , CInt(..)
                      )


data VkImportSemaphoreFdInfoKHR =
  VkImportSemaphoreFdInfoKHR{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkSemaphore :: VkSemaphore 
                            , vkFlags :: VkSemaphoreImportFlagsKHR 
                            , vkHandleType :: VkExternalSemaphoreHandleTypeFlagBitsKHR 
                            , vkFd :: CInt 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkImportSemaphoreFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportSemaphoreFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 28)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportSemaphoreFdInfoKHR))

data VkSemaphoreGetFdInfoKHR =
  VkSemaphoreGetFdInfoKHR{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkSemaphore :: VkSemaphore 
                         , vkHandleType :: VkExternalSemaphoreHandleTypeFlagBitsKHR 
                         }
  deriving (Eq, Ord, Show)
instance Storable VkSemaphoreGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSemaphoreGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkSemaphoreGetFdInfoKHR))
-- ** vkImportSemaphoreFdKHR
foreign import ccall "vkImportSemaphoreFdKHR" vkImportSemaphoreFdKHR ::
  VkDevice -> Ptr VkImportSemaphoreFdInfoKHR -> IO VkResult
-- ** vkGetSemaphoreFdKHR
foreign import ccall "vkGetSemaphoreFdKHR" vkGetSemaphoreFdKHR ::
  VkDevice -> Ptr VkSemaphoreGetFdInfoKHR -> Ptr CInt -> IO VkResult
