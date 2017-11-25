{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.MVK.MacosSurface where

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
import Graphics.Vulkan.KHR.Surface( VkSurfaceKHR(..)
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
import Graphics.Vulkan.DeviceInitialization( vkGetInstanceProcAddr
                                           , VkInstance(..)
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkCreateMacOSSurfaceMVK
foreign import ccall "dynamic" mkvkCreateMacOSSurfaceMVK :: FunPtr (VkInstance ->
  Ptr VkMacOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance ->
  Ptr VkMacOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)
vkCreateMacOSSurfaceMVK :: VkInstance ->
  Ptr VkMacOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult
vkCreateMacOSSurfaceMVK i = (mkvkCreateMacOSSurfaceMVK $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "vkCreateMacOSSurfaceMVK" $ vkGetInstanceProcAddr i
-- ** VkMacOSSurfaceCreateFlagsMVK-- | Opaque flag
newtype VkMacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME =  "VK_MVK_macos_surface"
pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION =  0x2
pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK = VkStructureType 1000123000

data VkMacOSSurfaceCreateInfoMVK =
  VkMacOSSurfaceCreateInfoMVK{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkFlags :: VkMacOSSurfaceCreateFlagsMVK 
                             , vkPView :: Ptr Void 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkMacOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMacOSSurfaceCreateInfoMVK <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMacOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 24) (vkPView (poked :: VkMacOSSurfaceCreateInfoMVK))
