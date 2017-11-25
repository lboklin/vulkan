{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.DisplaySwapchain where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
                                    , VkSwapchainCreateInfoKHR(..)
                                    , VkSwapchainCreateFlagsKHR(..)
                                    , VkSwapchainCreateFlagBitsKHR(..)
                                    )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkCompositeAlphaFlagBitsKHR(..)
                                  , VkColorSpaceKHR(..)
                                  , VkPresentModeKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
                                  )
import Data.Int( Int32
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
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.Core( VkOffset2D(..)
                           , VkExtent2D(..)
                           , VkFormat(..)
                           , VkBool32(..)
                           , VkRect2D(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkSharingMode(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data VkDisplayPresentInfoKHR =
  VkDisplayPresentInfoKHR{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkSrcRect :: VkRect2D 
                         , vkDstRect :: VkRect2D 
                         , vkPersistent :: VkBool32 
                         }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSrcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkDstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPersistent (poked :: VkDisplayPresentInfoKHR))
-- ** vkCreateSharedSwapchainsKHR
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR ::
  VkDevice ->
  Word32 ->
    Ptr VkSwapchainCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult
