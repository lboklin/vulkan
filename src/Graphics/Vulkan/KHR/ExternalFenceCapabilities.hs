{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.ExternalFenceCapabilities where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  , FunPtr
                  , castFunPtr
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
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )

-- ** VkExternalFenceHandleTypeFlagsKHR
newtype VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalFenceHandleTypeFlagBitsKHR
type VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlagBitsKHR

instance Show VkExternalFenceHandleTypeFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
  showsPrec _ VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR = showString "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
  
  showsPrec p (VkExternalFenceHandleTypeFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalFenceHandleTypeFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalFenceHandleTypeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR)
                             , ("VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR", pure VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceHandleTypeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalFenceHandleTypeFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VkExternalFenceHandleTypeFlagBitsKHR 0x1

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VkExternalFenceHandleTypeFlagBitsKHR 0x2

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VkExternalFenceHandleTypeFlagBitsKHR 0x4

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR = VkExternalFenceHandleTypeFlagBitsKHR 0x8
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION =  0x1

data VkExternalFencePropertiesKHR =
  VkExternalFencePropertiesKHR{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkExportFromImportedHandleTypes :: VkExternalFenceHandleTypeFlagsKHR 
                              , vkCompatibleHandleTypes :: VkExternalFenceHandleTypeFlagsKHR 
                              , vkExternalFenceFeatures :: VkExternalFenceFeatureFlagsKHR 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkExternalFencePropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkExternalFencePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 20)
                                          <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalFencePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalFencePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkExportFromImportedHandleTypes (poked :: VkExternalFencePropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkCompatibleHandleTypes (poked :: VkExternalFencePropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkExternalFenceFeatures (poked :: VkExternalFencePropertiesKHR))
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR = VkStructureType 1000112001
-- ** VkExternalFenceFeatureFlagsKHR
newtype VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkExternalFenceFeatureFlagBitsKHR
type VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlagBitsKHR

instance Show VkExternalFenceFeatureFlagBitsKHR where
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = showString "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
  showsPrec _ VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = showString "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
  
  showsPrec p (VkExternalFenceFeatureFlagBitsKHR x) = showParen (p >= 11) (showString "VkExternalFenceFeatureFlagBitsKHR " . showsPrec 11 x)

instance Read VkExternalFenceFeatureFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR", pure VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR)
                             , ("VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR", pure VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalFenceFeatureFlagBitsKHR")
                        v <- step readPrec
                        pure (VkExternalFenceFeatureFlagBitsKHR v)
                        )
                    )

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = VkExternalFenceFeatureFlagBitsKHR 0x1

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = VkExternalFenceFeatureFlagBitsKHR 0x2
-- ** vkGetPhysicalDeviceExternalFencePropertiesKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceExternalFencePropertiesKHR :: FunPtr (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceExternalFenceInfoKHR ->
    Ptr VkExternalFencePropertiesKHR -> IO ()) -> (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceExternalFenceInfoKHR ->
    Ptr VkExternalFencePropertiesKHR -> IO ())
vkGetPhysicalDeviceExternalFencePropertiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    Ptr VkPhysicalDeviceExternalFenceInfoKHR ->
      Ptr VkExternalFencePropertiesKHR -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesKHR i = (mkvkGetPhysicalDeviceExternalFencePropertiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceExternalFencePropertiesKHR" $ vkGetInstanceProcAddr i
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR = VkStructureType 1000112000
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME =  "VK_KHR_external_fence_capabilities"

data VkPhysicalDeviceExternalFenceInfoKHR =
  VkPhysicalDeviceExternalFenceInfoKHR{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkHandleType :: VkExternalFenceHandleTypeFlagBitsKHR 
                                      }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceExternalFenceInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExternalFenceInfoKHR <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExternalFenceInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExternalFenceInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkPhysicalDeviceExternalFenceInfoKHR))
