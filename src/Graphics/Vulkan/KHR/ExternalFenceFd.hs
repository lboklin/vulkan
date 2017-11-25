{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.ExternalFenceFd where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  , FunPtr
                  , castFunPtr
                  )
import Graphics.Vulkan.KHR.ExternalFenceCapabilities( VkExternalFenceHandleTypeFlagBitsKHR(..)
                                                    )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
                            )
import Graphics.Vulkan.KHR.ExternalFence( VkFenceImportFlagsKHR(..)
                                        , VkFenceImportFlagBitsKHR(..)
                                        )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( vkGetDeviceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CInt
                      , CInt(..)
                      )

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION =  0x1
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME =  "VK_KHR_external_fence_fd"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR = VkStructureType 1000115000

data VkImportFenceFdInfoKHR =
  VkImportFenceFdInfoKHR{ vkSType :: VkStructureType 
                        , vkPNext :: Ptr Void 
                        , vkFence :: VkFence 
                        , vkFlags :: VkFenceImportFlagsKHR 
                        , vkHandleType :: VkExternalFenceHandleTypeFlagBitsKHR 
                        , vkFd :: CInt 
                        }
  deriving (Eq, Ord, Show)
instance Storable VkImportFenceFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportFenceFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportFenceFdInfoKHR))
-- ** vkGetFenceFdKHR
foreign import ccall "dynamic" mkvkGetFenceFdKHR :: FunPtr (VkDevice -> Ptr VkFenceGetFdInfoKHR -> Ptr CInt -> IO VkResult) -> (VkDevice -> Ptr VkFenceGetFdInfoKHR -> Ptr CInt -> IO VkResult)
vkGetFenceFdKHR :: VkDevice -> Ptr VkFenceGetFdInfoKHR -> Ptr CInt -> IO VkResult
vkGetFenceFdKHR d = (mkvkGetFenceFdKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetFenceFdKHR" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR = VkStructureType 1000115001

data VkFenceGetFdInfoKHR =
  VkFenceGetFdInfoKHR{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkFence :: VkFence 
                     , vkHandleType :: VkExternalFenceHandleTypeFlagBitsKHR 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkFenceGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFenceGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkFenceGetFdInfoKHR))
-- ** vkImportFenceFdKHR
foreign import ccall "dynamic" mkvkImportFenceFdKHR :: FunPtr (VkDevice -> Ptr VkImportFenceFdInfoKHR -> IO VkResult) -> (VkDevice -> Ptr VkImportFenceFdInfoKHR -> IO VkResult)
vkImportFenceFdKHR :: VkDevice -> Ptr VkImportFenceFdInfoKHR -> IO VkResult
vkImportFenceFdKHR d = (mkvkImportFenceFdKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkImportFenceFdKHR" $ vkGetDeviceProcAddr d
