{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.MVK.IosSurface where

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

-- ** VkIOSSurfaceCreateFlagsMVK-- | Opaque flag
newtype VkIOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME =  "VK_MVK_ios_surface"
pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK = VkStructureType 1000122000
-- ** vkCreateIOSSurfaceMVK
foreign import ccall "dynamic" mkvkCreateIOSSurfaceMVK :: FunPtr (VkInstance ->
  Ptr VkIOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance ->
  Ptr VkIOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)
vkCreateIOSSurfaceMVK :: VkInstance ->
  Ptr VkIOSSurfaceCreateInfoMVK ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult
vkCreateIOSSurfaceMVK i = (mkvkCreateIOSSurfaceMVK $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "vkCreateIOSSurfaceMVK" $ vkGetInstanceProcAddr i

data VkIOSSurfaceCreateInfoMVK =
  VkIOSSurfaceCreateInfoMVK{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkFlags :: VkIOSSurfaceCreateFlagsMVK 
                           , vkPView :: Ptr Void 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkIOSSurfaceCreateInfoMVK where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkIOSSurfaceCreateInfoMVK <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkIOSSurfaceCreateInfoMVK))
                *> poke (ptr `plusPtr` 24) (vkPView (poked :: VkIOSSurfaceCreateInfoMVK))
pattern VK_MVK_IOS_SURFACE_SPEC_VERSION =  0x2
