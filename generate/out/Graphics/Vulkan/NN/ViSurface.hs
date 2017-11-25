{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NN.ViSurface where

import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
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
import Graphics.Vulkan.DeviceInitialization( VkInstance(..)
                                           )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data VkViSurfaceCreateInfoNN =
  VkViSurfaceCreateInfoNN{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkViSurfaceCreateFlagsNN 
                         , vkWindow :: Ptr Void 
                         }
  deriving (Eq, Ord, Show)
instance Storable VkViSurfaceCreateInfoNN where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkViSurfaceCreateInfoNN <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkViSurfaceCreateInfoNN))
                *> poke (ptr `plusPtr` 24) (vkWindow (poked :: VkViSurfaceCreateInfoNN))
-- ** vkCreateViSurfaceNN
foreign import ccall "vkCreateViSurfaceNN" vkCreateViSurfaceNN ::
  VkInstance ->
  Ptr VkViSurfaceCreateInfoNN ->
    Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult
-- ** VkViSurfaceCreateFlagsNN-- | Opaque flag
newtype VkViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
