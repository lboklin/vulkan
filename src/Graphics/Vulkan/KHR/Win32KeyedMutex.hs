{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.Win32KeyedMutex where

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
import Graphics.Vulkan.Memory( VkDeviceMemory(..)
                             )
import Graphics.Vulkan.Core( VkStructureType(..)
                           )


data VkWin32KeyedMutexAcquireReleaseInfoKHR =
  VkWin32KeyedMutexAcquireReleaseInfoKHR{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkAcquireCount :: Word32 
                                        , vkPAcquireSyncs :: Ptr VkDeviceMemory 
                                        , vkPAcquireKeys :: Ptr Word64 
                                        , vkPAcquireTimeouts :: Ptr Word32 
                                        , vkReleaseCount :: Word32 
                                        , vkPReleaseSyncs :: Ptr VkDeviceMemory 
                                        , vkPReleaseKeys :: Ptr Word64 
                                        }
  deriving (Eq, Ord, Show)
instance Storable VkWin32KeyedMutexAcquireReleaseInfoKHR where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkWin32KeyedMutexAcquireReleaseInfoKHR <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 56)
                                                    <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkAcquireCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPAcquireSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPAcquireKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPAcquireTimeouts (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkReleaseCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPReleaseSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkPReleaseKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoKHR))
pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME =  "VK_KHR_win32_keyed_mutex"
pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION =  0x1
pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR = VkStructureType 1000075000
