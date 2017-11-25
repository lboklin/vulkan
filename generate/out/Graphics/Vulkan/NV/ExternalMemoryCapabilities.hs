{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NV.ExternalMemoryCapabilities where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              , VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( VkImageTiling(..)
                            , VkImageCreateFlags(..)
                            , VkImageType(..)
                            , VkImageUsageFlagBits(..)
                            , VkImageCreateFlagBits(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.DeviceInitialization( VkImageFormatProperties(..)
                                           )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkExtent3D(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkResult(..)
                           )

-- ** vkGetPhysicalDeviceExternalImageFormatPropertiesNV
foreign import ccall "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" vkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
  VkPhysicalDevice ->
  VkFormat ->
    VkImageType ->
      VkImageTiling ->
        VkImageUsageFlags ->
          VkImageCreateFlags ->
            VkExternalMemoryHandleTypeFlagsNV ->
              Ptr VkExternalImageFormatPropertiesNV -> IO VkResult
-- ** VkExternalMemoryFeatureFlagsNV
newtype VkExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalMemoryFeatureFlagBitsNV
type VkExternalMemoryFeatureFlagsNV = VkExternalMemoryFeatureFlagBitsNV

instance Show VkExternalMemoryFeatureFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
  
  showsPrec p (VkExternalMemoryFeatureFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV", pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV", pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBitsNV v)
                        )
                    )

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x1

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x2

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x4

data VkExternalImageFormatPropertiesNV =
  VkExternalImageFormatPropertiesNV{ vkImageFormatProperties :: VkImageFormatProperties 
                                   , vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlagsNV 
                                   , vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                                   , vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkExternalImageFormatPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatPropertiesNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 36)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImageFormatProperties (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 32) (vkExternalMemoryFeatures (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 36) (vkExportFromImportedHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkCompatibleHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
-- ** VkExternalMemoryHandleTypeFlagsNV
newtype VkExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalMemoryHandleTypeFlagBitsNV
type VkExternalMemoryHandleTypeFlagsNV = VkExternalMemoryHandleTypeFlagBitsNV

instance Show VkExternalMemoryHandleTypeFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
  
  showsPrec p (VkExternalMemoryHandleTypeFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBitsNV v)
                        )
                    )

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x1

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x2

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x4

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x8
