{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Surface where

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
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  , FunPtr
                  , castFunPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkSystemAllocationScope(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkFreeFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkInternalAllocationType(..)
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           , VkInstance(..)
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkFormat(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

pattern VK_KHR_SURFACE_EXTENSION_NAME =  "VK_KHR_surface"
pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR =  0x0
-- ** vkGetPhysicalDeviceSurfaceFormatsKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult)
vkGetPhysicalDeviceSurfaceFormatsKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHR i = (mkvkGetPhysicalDeviceSurfaceFormatsKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfaceFormatsKHR" $ vkGetInstanceProcAddr i
pattern VK_KHR_SURFACE_SPEC_VERSION =  0x19
-- ** vkGetPhysicalDeviceSurfaceCapabilitiesKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult)
vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHR i = (mkvkGetPhysicalDeviceSurfaceCapabilitiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" $ vkGetInstanceProcAddr i
-- ** VkCompositeAlphaFlagsKHR
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkCompositeAlphaFlagBitsKHR
type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR

instance Show VkCompositeAlphaFlagBitsKHR where
  showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
  
  showsPrec p (VkCompositeAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkCompositeAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkCompositeAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR", pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR", pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompositeAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkCompositeAlphaFlagBitsKHR v)
                        )
                    )

pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x1

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x2

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x4

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x8
-- ** VkPresentModeKHR
newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkPresentModeKHR where
  showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
  showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR = showString "VK_PRESENT_MODE_MAILBOX_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_KHR = showString "VK_PRESENT_MODE_FIFO_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
  showsPrec p (VkPresentModeKHR x) = showParen (p >= 11) (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
  readPrec = parens ( choose [ ("VK_PRESENT_MODE_IMMEDIATE_KHR", pure VK_PRESENT_MODE_IMMEDIATE_KHR)
                             , ("VK_PRESENT_MODE_MAILBOX_KHR", pure VK_PRESENT_MODE_MAILBOX_KHR)
                             , ("VK_PRESENT_MODE_FIFO_KHR", pure VK_PRESENT_MODE_FIFO_KHR)
                             , ("VK_PRESENT_MODE_FIFO_RELAXED_KHR", pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPresentModeKHR")
                        v <- step readPrec
                        pure (VkPresentModeKHR v)
                        )
                    )

pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = VkResult (-1000000001)
newtype VkSurfaceKHR = VkSurfaceKHR Word64
  deriving (Eq, Ord, Storable, Show)
-- ** vkGetPhysicalDeviceSurfaceSupportKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (VkPhysicalDevice ->
  Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult) -> (VkPhysicalDevice ->
  Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult)
vkGetPhysicalDeviceSurfaceSupportKHR :: VkInstance ->
  VkPhysicalDevice ->
    Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHR i = (mkvkGetPhysicalDeviceSurfaceSupportKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfaceSupportKHR" $ vkGetInstanceProcAddr i
pattern VK_ERROR_SURFACE_LOST_KHR = VkResult (-1000000000)

data VkSurfaceFormatKHR =
  VkSurfaceFormatKHR{ vkFormat :: VkFormat 
                    , vkColorSpace :: VkColorSpaceKHR 
                    }
  deriving (Eq, Ord, Show)
instance Storable VkSurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSurfaceFormatKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormat (poked :: VkSurfaceFormatKHR))
                *> poke (ptr `plusPtr` 4) (vkColorSpace (poked :: VkSurfaceFormatKHR))
-- ** vkDestroySurfaceKHR
foreign import ccall "dynamic" mkvkDestroySurfaceKHR :: FunPtr (VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()) -> (VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ())
vkDestroySurfaceKHR :: VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()
vkDestroySurfaceKHR i = (mkvkDestroySurfaceKHR $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "vkDestroySurfaceKHR" $ vkGetInstanceProcAddr i
pattern VK_OBJECT_TYPE_SURFACE_KHR = VkObjectType 1000000000
-- ** VkColorSpaceKHR
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkColorSpaceKHR where
  showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
  showsPrec p (VkColorSpaceKHR x) = showParen (p >= 11) (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
  readPrec = parens ( choose [ ("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR", pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorSpaceKHR")
                        v <- step readPrec
                        pure (VkColorSpaceKHR v)
                        )
                    )

pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0
-- ** vkGetPhysicalDeviceSurfacePresentModesKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfacePresentModesKHR :: FunPtr (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult)
vkGetPhysicalDeviceSurfacePresentModesKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHR i = (mkvkGetPhysicalDeviceSurfacePresentModesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfacePresentModesKHR" $ vkGetInstanceProcAddr i
-- ** VkSurfaceTransformFlagsKHR
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSurfaceTransformFlagBitsKHR
type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR

instance Show VkSurfaceTransformFlagBitsKHR where
  showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
  
  showsPrec p (VkSurfaceTransformFlagBitsKHR x) = showParen (p >= 11) (showString "VkSurfaceTransformFlagBitsKHR " . showsPrec 11 x)

instance Read VkSurfaceTransformFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR", pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR", pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSurfaceTransformFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSurfaceTransformFlagBitsKHR v)
                        )
                    )

pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x1

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x2

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x4

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x8

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x10

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x20

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x40

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x80

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x100

data VkSurfaceCapabilitiesKHR =
  VkSurfaceCapabilitiesKHR{ vkMinImageCount :: Word32 
                          , vkMaxImageCount :: Word32 
                          , vkCurrentExtent :: VkExtent2D 
                          , vkMinImageExtent :: VkExtent2D 
                          , vkMaxImageExtent :: VkExtent2D 
                          , vkMaxImageArrayLayers :: Word32 
                          , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR 
                          , vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR 
                          , vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR 
                          , vkSupportedUsageFlags :: VkImageUsageFlags 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkSurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek ptr = VkSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMinImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMaxImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMinImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 24) (vkMaxImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 32) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkSupportedTransforms (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 40) (vkCurrentTransform (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 48) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilitiesKHR))
