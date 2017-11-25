{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.ExternalMemoryCapabilities where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBufferUsageFlags(..)
                             , VkBufferCreateFlagBits(..)
                             , VkBufferCreateFlags(..)
                             , VkBufferUsageFlagBits(..)
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

-- ** VkExternalMemoryHandleTypeFlagsKHR
newtype VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalMemoryHandleTypeFlagBitsKHR
type VkExternalMemoryHandleTypeFlagsKHR = VkExternalMemoryHandleTypeFlagBitsKHR

instance Show VkExternalMemoryHandleTypeFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR"
  
  showsPrec p (VkExternalMemoryHandleTypeFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x1

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x2

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x4

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x8

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x10

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x20

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR = VkExternalMemoryHandleTypeFlagBitsKHR 0x40

data VkExternalBufferPropertiesKHR =
  VkExternalBufferPropertiesKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkExternalMemoryProperties :: VkExternalMemoryPropertiesKHR 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkExternalBufferPropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalBufferPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalBufferPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalBufferPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalBufferPropertiesKHR))

data VkPhysicalDeviceExternalImageFormatInfoKHR =
  VkPhysicalDeviceExternalImageFormatInfoKHR{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkHandleType :: VkExternalMemoryHandleTypeFlagBitsKHR 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceExternalImageFormatInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalImageFormatInfoKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalImageFormatInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalImageFormatInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalImageFormatInfoKHR))

data VkExternalMemoryPropertiesKHR =
  VkExternalMemoryPropertiesKHR{ vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlagsKHR 
                               , vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                               , vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryPropertiesKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExternalMemoryPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExternalMemoryFeatures (poked :: VkExternalMemoryPropertiesKHR))
                *> poke (ptr `plusPtr` 4) (vkExportFromImportedHandleTypes (poked :: VkExternalMemoryPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCompatibleHandleTypes (poked :: VkExternalMemoryPropertiesKHR))

data VkExternalImageFormatPropertiesKHR =
  VkExternalImageFormatPropertiesKHR{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkExternalMemoryProperties :: VkExternalMemoryPropertiesKHR 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkExternalImageFormatPropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalImageFormatPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalImageFormatPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkExternalMemoryProperties (poked :: VkExternalImageFormatPropertiesKHR))

data VkPhysicalDeviceExternalBufferInfoKHR =
  VkPhysicalDeviceExternalBufferInfoKHR{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFlags :: VkBufferCreateFlags 
                                       , vkUsage :: VkBufferUsageFlags 
                                       , vkHandleType :: VkExternalMemoryHandleTypeFlagBitsKHR 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceExternalBufferInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalBufferInfoKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalBufferInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalBufferInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPhysicalDeviceExternalBufferInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkUsage (poked :: VkPhysicalDeviceExternalBufferInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkPhysicalDeviceExternalBufferInfoKHR))
-- ** VkExternalMemoryFeatureFlagsKHR
newtype VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalMemoryFeatureFlagBitsKHR
type VkExternalMemoryFeatureFlagsKHR = VkExternalMemoryFeatureFlagBitsKHR

instance Show VkExternalMemoryFeatureFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR"
  
  showsPrec p (VkExternalMemoryFeatureFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR", pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR", pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = VkExternalMemoryFeatureFlagBitsKHR 0x1

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR = VkExternalMemoryFeatureFlagBitsKHR 0x2

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR = VkExternalMemoryFeatureFlagBitsKHR 0x4
-- ** vkGetPhysicalDeviceExternalBufferPropertiesKHR
foreign import ccall "vkGetPhysicalDeviceExternalBufferPropertiesKHR" vkGetPhysicalDeviceExternalBufferPropertiesKHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceExternalBufferInfoKHR ->
    Ptr VkExternalBufferPropertiesKHR -> IO ()
