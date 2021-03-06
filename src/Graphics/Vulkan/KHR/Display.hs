{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Display where

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
import Graphics.Vulkan.KHR.Surface( VkSurfaceTransformFlagsKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
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
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           , VkInstance(..)
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkOffset2D(..)
                           , VkExtent2D(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      , CChar
                      , CSize(..)
                      )


data VkDisplaySurfaceCreateInfoKHR =
  VkDisplaySurfaceCreateInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkFlags :: VkDisplaySurfaceCreateFlagsKHR 
                               , vkDisplayMode :: VkDisplayModeKHR 
                               , vkPlaneIndex :: Word32 
                               , vkPlaneStackIndex :: Word32 
                               , vkTransform :: VkSurfaceTransformFlagBitsKHR 
                               , vkGlobalAlpha :: CFloat 
                               , vkAlphaMode :: VkDisplayPlaneAlphaFlagBitsKHR 
                               , vkImageExtent :: VkExtent2D 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkDisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplaySurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 48)
                                           <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplayMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPlaneIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneStackIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkTransform (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkGlobalAlpha (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkAlphaMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageExtent (poked :: VkDisplaySurfaceCreateInfoKHR))
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = VkStructureType 1000002001

data VkDisplayPlaneCapabilitiesKHR =
  VkDisplayPlaneCapabilitiesKHR{ vkSupportedAlpha :: VkDisplayPlaneAlphaFlagsKHR 
                               , vkMinSrcPosition :: VkOffset2D 
                               , vkMaxSrcPosition :: VkOffset2D 
                               , vkMinSrcExtent :: VkExtent2D 
                               , vkMaxSrcExtent :: VkExtent2D 
                               , vkMinDstPosition :: VkOffset2D 
                               , vkMaxDstPosition :: VkOffset2D 
                               , vkMinDstExtent :: VkExtent2D 
                               , vkMaxDstExtent :: VkExtent2D 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkDisplayPlaneCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 12)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 28)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 52)
                                           <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSupportedAlpha (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMinSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (vkMaxSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (vkMinSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (vkMaxSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkMinDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkMaxDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (vkMinDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (vkMaxDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
pattern VK_KHR_DISPLAY_EXTENSION_NAME =  "VK_KHR_display"
-- ** vkGetDisplayModePropertiesKHR
foreign import ccall "dynamic" mkvkGetDisplayModePropertiesKHR :: FunPtr (VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult)
vkGetDisplayModePropertiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkDisplayKHR ->
      Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult
vkGetDisplayModePropertiesKHR i = (mkvkGetDisplayModePropertiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetDisplayModePropertiesKHR" $ vkGetInstanceProcAddr i

data VkDisplayPropertiesKHR =
  VkDisplayPropertiesKHR{ vkDisplay :: VkDisplayKHR 
                        , vkDisplayName :: Ptr CChar 
                        , vkPhysicalDimensions :: VkExtent2D 
                        , vkPhysicalResolution :: VkExtent2D 
                        , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR 
                        , vkPlaneReorderPossible :: VkBool32 
                        , vkPersistentContent :: VkBool32 
                        }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayPropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkDisplayPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplay (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkDisplayName (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDimensions (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkPhysicalResolution (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkSupportedTransforms (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneReorderPossible (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkPersistentContent (poked :: VkDisplayPropertiesKHR))
-- ** vkGetDisplayPlaneSupportedDisplaysKHR
foreign import ccall "dynamic" mkvkGetDisplayPlaneSupportedDisplaysKHR :: FunPtr (VkPhysicalDevice ->
  Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult) -> (VkPhysicalDevice ->
  Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult)
vkGetDisplayPlaneSupportedDisplaysKHR :: VkInstance ->
  VkPhysicalDevice ->
    Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult
vkGetDisplayPlaneSupportedDisplaysKHR i = (mkvkGetDisplayPlaneSupportedDisplaysKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetDisplayPlaneSupportedDisplaysKHR" $ vkGetInstanceProcAddr i
pattern VK_KHR_DISPLAY_SPEC_VERSION =  0x15
-- ** vkCreateDisplayModeKHR
foreign import ccall "dynamic" mkvkCreateDisplayModeKHR :: FunPtr (VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayModeCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayModeCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult)
vkCreateDisplayModeKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkDisplayKHR ->
      Ptr VkDisplayModeCreateInfoKHR ->
        Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult
vkCreateDisplayModeKHR i = (mkvkCreateDisplayModeKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCreateDisplayModeKHR" $ vkGetInstanceProcAddr i

data VkDisplayPlanePropertiesKHR =
  VkDisplayPlanePropertiesKHR{ vkCurrentDisplay :: VkDisplayKHR 
                             , vkCurrentStackIndex :: Word32 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkDisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkCurrentDisplay (poked :: VkDisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentStackIndex (poked :: VkDisplayPlanePropertiesKHR))
-- ** vkGetDisplayPlaneCapabilitiesKHR
foreign import ccall "dynamic" mkvkGetDisplayPlaneCapabilitiesKHR :: FunPtr (VkPhysicalDevice ->
  VkDisplayModeKHR ->
    Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult) -> (VkPhysicalDevice ->
  VkDisplayModeKHR ->
    Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult)
vkGetDisplayPlaneCapabilitiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    VkDisplayModeKHR ->
      Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult
vkGetDisplayPlaneCapabilitiesKHR i = (mkvkGetDisplayPlaneCapabilitiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetDisplayPlaneCapabilitiesKHR" $ vkGetInstanceProcAddr i

data VkDisplayModePropertiesKHR =
  VkDisplayModePropertiesKHR{ vkDisplayMode :: VkDisplayModeKHR 
                            , vkParameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplayMode (poked :: VkDisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkParameters (poked :: VkDisplayModePropertiesKHR))
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = VkStructureType 1000002000
pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000
-- ** VkDisplayPlaneAlphaFlagsKHR
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkDisplayPlaneAlphaFlagBitsKHR
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

instance Show VkDisplayPlaneAlphaFlagBitsKHR where
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
  
  showsPrec p (VkDisplayPlaneAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkDisplayPlaneAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPlaneAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkDisplayPlaneAlphaFlagBitsKHR v)
                        )
                    )

pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x1

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x2

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x4

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x8
-- ** VkDisplayModeCreateFlagsKHR-- | Opaque flag
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)

data VkDisplayModeCreateInfoKHR =
  VkDisplayModeCreateInfoKHR{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkDisplayModeCreateFlagsKHR 
                            , vkParameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkParameters (poked :: VkDisplayModeCreateInfoKHR))
-- ** vkGetPhysicalDeviceDisplayPlanePropertiesKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceDisplayPlanePropertiesKHR :: FunPtr (VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult) -> (VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult)
vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult
vkGetPhysicalDeviceDisplayPlanePropertiesKHR i = (mkvkGetPhysicalDeviceDisplayPlanePropertiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" $ vkGetInstanceProcAddr i
newtype VkDisplayModeKHR = VkDisplayModeKHR Word64
  deriving (Eq, Ord, Storable, Show)

data VkDisplayModeParametersKHR =
  VkDisplayModeParametersKHR{ vkVisibleRegion :: VkExtent2D 
                            , vkRefreshRate :: Word32 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVisibleRegion (poked :: VkDisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (vkRefreshRate (poked :: VkDisplayModeParametersKHR))
-- ** VkDisplaySurfaceCreateFlagsKHR-- | Opaque flag
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
newtype VkDisplayKHR = VkDisplayKHR Word64
  deriving (Eq, Ord, Storable, Show)
-- ** vkGetPhysicalDeviceDisplayPropertiesKHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceDisplayPropertiesKHR :: FunPtr (VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult) -> (VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult)
vkGetPhysicalDeviceDisplayPropertiesKHR :: VkInstance ->
  VkPhysicalDevice ->
    Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult
vkGetPhysicalDeviceDisplayPropertiesKHR i = (mkvkGetPhysicalDeviceDisplayPropertiesKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceDisplayPropertiesKHR" $ vkGetInstanceProcAddr i
-- ** vkCreateDisplayPlaneSurfaceKHR
foreign import ccall "dynamic" mkvkCreateDisplayPlaneSurfaceKHR :: FunPtr (VkInstance ->
  Ptr VkDisplaySurfaceCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance ->
  Ptr VkDisplaySurfaceCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)
vkCreateDisplayPlaneSurfaceKHR :: VkInstance ->
  Ptr VkDisplaySurfaceCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult
vkCreateDisplayPlaneSurfaceKHR i = (mkvkCreateDisplayPlaneSurfaceKHR $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "vkCreateDisplayPlaneSurfaceKHR" $ vkGetInstanceProcAddr i
