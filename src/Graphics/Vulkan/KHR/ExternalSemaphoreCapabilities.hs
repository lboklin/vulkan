{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.ExternalSemaphoreCapabilities where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME =  "VK_KHR_external_semaphore_capabilities"

data VkPhysicalDeviceExternalSemaphoreInfoKHR =
  VkPhysicalDeviceExternalSemaphoreInfoKHR{ vkSType :: VkStructureType 
                                          , vkPNext :: Ptr Void 
                                          , vkHandleType :: VkExternalSemaphoreHandleTypeFlagBitsKHR 
                                          }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceExternalSemaphoreInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalSemaphoreInfoKHR <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalSemaphoreInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalSemaphoreInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalSemaphoreInfoKHR))

data VkExternalSemaphorePropertiesKHR =
  VkExternalSemaphorePropertiesKHR{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkExportFromImportedHandleTypes :: VkExternalSemaphoreHandleTypeFlagsKHR 
                                  , vkCompatibleHandleTypes :: VkExternalSemaphoreHandleTypeFlagsKHR 
                                  , vkExternalSemaphoreFeatures :: VkExternalSemaphoreFeatureFlagsKHR 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkExternalSemaphorePropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalSemaphorePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalSemaphorePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalSemaphorePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkExportFromImportedHandleTypes (poked :: VkExternalSemaphorePropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkCompatibleHandleTypes (poked :: VkExternalSemaphorePropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkExternalSemaphoreFeatures (poked :: VkExternalSemaphorePropertiesKHR))
-- ** vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
foreign import ccall "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR" vkGetPhysicalDeviceExternalSemaphorePropertiesKHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceExternalSemaphoreInfoKHR ->
    Ptr VkExternalSemaphorePropertiesKHR -> IO ()
pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION =  0x1
-- ** VkExternalSemaphoreHandleTypeFlagsKHR
newtype VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalSemaphoreHandleTypeFlagBitsKHR
type VkExternalSemaphoreHandleTypeFlagsKHR = VkExternalSemaphoreHandleTypeFlagBitsKHR

instance Show VkExternalSemaphoreHandleTypeFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
  
  showsPrec p (VkExternalSemaphoreHandleTypeFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalSemaphoreHandleTypeFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalSemaphoreHandleTypeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR)
                             , ("VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalSemaphoreHandleTypeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalSemaphoreHandleTypeFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VkExternalSemaphoreHandleTypeFlagBitsKHR 0x1

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VkExternalSemaphoreHandleTypeFlagBitsKHR 0x2

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VkExternalSemaphoreHandleTypeFlagBitsKHR 0x4

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR = VkExternalSemaphoreHandleTypeFlagBitsKHR 0x8

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR = VkExternalSemaphoreHandleTypeFlagBitsKHR 0x10
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR = VkStructureType 1000076000
-- ** VkExternalSemaphoreFeatureFlagsKHR
newtype VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalSemaphoreFeatureFlagBitsKHR
type VkExternalSemaphoreFeatureFlagsKHR = VkExternalSemaphoreFeatureFlagBitsKHR

instance Show VkExternalSemaphoreFeatureFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR"
  showsPrec _ VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR = showString "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR"
  
  showsPrec p (VkExternalSemaphoreFeatureFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalSemaphoreFeatureFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalSemaphoreFeatureFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR)
                             , ("VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR", pure VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalSemaphoreFeatureFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalSemaphoreFeatureFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR = VkExternalSemaphoreFeatureFlagBitsKHR 0x1

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR = VkExternalSemaphoreFeatureFlagBitsKHR 0x2
pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR = VkStructureType 1000076001
