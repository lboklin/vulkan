{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.NV.Win32KeyedMutex where

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


data VkWin32KeyedMutexAcquireReleaseInfoNV =
  VkWin32KeyedMutexAcquireReleaseInfoNV{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkAcquireCount :: Word32 
                                       , vkPAcquireSyncs :: Ptr VkDeviceMemory 
                                       , vkPAcquireKeys :: Ptr Word64 
                                       , vkPAcquireTimeoutMilliseconds :: Ptr Word32 
                                       , vkReleaseCount :: Word32 
                                       , vkPReleaseSyncs :: Ptr VkDeviceMemory 
                                       , vkPReleaseKeys :: Ptr Word64 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkWin32KeyedMutexAcquireReleaseInfoNV where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkWin32KeyedMutexAcquireReleaseInfoNV <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 32)
                                                   <*> peek (ptr `plusPtr` 40)
                                                   <*> peek (ptr `plusPtr` 48)
                                                   <*> peek (ptr `plusPtr` 56)
                                                   <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 16) (vkAcquireCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPAcquireSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 32) (vkPAcquireKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 40) (vkPAcquireTimeoutMilliseconds (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 48) (vkReleaseCount (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 56) (vkPReleaseSyncs (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
                *> poke (ptr `plusPtr` 64) (vkPReleaseKeys (poked :: VkWin32KeyedMutexAcquireReleaseInfoNV))
