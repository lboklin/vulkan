{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Swapchain where

import Graphics.Vulkan.Device( VkDevice(..)
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
import Graphics.Vulkan.KHR.Surface( VkCompositeAlphaFlagBitsKHR(..)
                                  , VkColorSpaceKHR(..)
                                  , VkPresentModeKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
                                  )
import Graphics.Vulkan.Queue( VkQueue(..)
                            )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
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
                            , VkImageLayout(..)
                            , VkImage(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetDeviceProcAddr
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkFormat(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkSharingMode(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

pattern VK_ERROR_OUT_OF_DATE_KHR = VkResult (-1000001004)

data VkSwapchainCreateInfoKHR =
  VkSwapchainCreateInfoKHR{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkFlags :: VkSwapchainCreateFlagsKHR 
                          , vkSurface :: VkSurfaceKHR 
                          , vkMinImageCount :: Word32 
                          , vkImageFormat :: VkFormat 
                          , vkImageColorSpace :: VkColorSpaceKHR 
                          , vkImageExtent :: VkExtent2D 
                          , vkImageArrayLayers :: Word32 
                          , vkImageUsage :: VkImageUsageFlags 
                          , vkImageSharingMode :: VkSharingMode 
                          , vkQueueFamilyIndexCount :: Word32 
                          , vkPQueueFamilyIndices :: Ptr Word32 
                          , vkPreTransform :: VkSurfaceTransformFlagBitsKHR 
                          , vkCompositeAlpha :: VkCompositeAlphaFlagBitsKHR 
                          , vkPresentMode :: VkPresentModeKHR 
                          , vkClipped :: VkBool32 
                          , vkOldSwapchain :: VkSwapchainKHR 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkSwapchainCreateInfoKHR where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = VkSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkSurface (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMinImageCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkImageFormat (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkImageColorSpace (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageArrayLayers (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkImageUsage (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (vkImageSharingMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (vkPreTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (vkCompositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (vkPresentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (vkClipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (vkOldSwapchain (poked :: VkSwapchainCreateInfoKHR))
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = VkImageLayout 1000001002
-- ** vkGetSwapchainImagesKHR
foreign import ccall "dynamic" mkvkGetSwapchainImagesKHR :: FunPtr (VkDevice ->
  VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult) -> (VkDevice ->
  VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult)
vkGetSwapchainImagesKHR :: VkDevice ->
  VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult
vkGetSwapchainImagesKHR d = (mkvkGetSwapchainImagesKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetSwapchainImagesKHR" $ vkGetDeviceProcAddr d
-- ** vkDestroySwapchainKHR
foreign import ccall "dynamic" mkvkDestroySwapchainKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ())
vkDestroySwapchainKHR :: VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()
vkDestroySwapchainKHR d = (mkvkDestroySwapchainKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDestroySwapchainKHR" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = VkStructureType 1000001001
-- ** vkQueuePresentKHR
foreign import ccall "dynamic" mkvkQueuePresentKHR :: FunPtr (VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult) -> (VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult)
vkQueuePresentKHR :: VkInstance -> VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult
vkQueuePresentKHR i = (mkvkQueuePresentKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkQueuePresentKHR" $ vkGetInstanceProcAddr i
pattern VK_SUBOPTIMAL_KHR = VkResult 1000001003
-- ** VkSwapchainCreateFlagsKHR
newtype VkSwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSwapchainCreateFlagBitsKHR
type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR

instance Show VkSwapchainCreateFlagBitsKHR where
  
  
  showsPrec p (VkSwapchainCreateFlagBitsKHR x) = showParen (p >= 11) (showString "VkSwapchainCreateFlagBitsKHR " . showsPrec 11 x)

instance Read VkSwapchainCreateFlagBitsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSwapchainCreateFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSwapchainCreateFlagBitsKHR v)
                        )
                    )

pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR = VkObjectType 1000001000
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION =  0x44
-- ** vkCreateSwapchainKHR
foreign import ccall "dynamic" mkvkCreateSwapchainKHR :: FunPtr (VkDevice ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult) -> (VkDevice ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult)
vkCreateSwapchainKHR :: VkDevice ->
  Ptr VkSwapchainCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult
vkCreateSwapchainKHR d = (mkvkCreateSwapchainKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkCreateSwapchainKHR" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000001000
-- ** vkAcquireNextImageKHR
foreign import ccall "dynamic" mkvkAcquireNextImageKHR :: FunPtr (VkDevice ->
  VkSwapchainKHR ->
    Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult) -> (VkDevice ->
  VkSwapchainKHR ->
    Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult)
vkAcquireNextImageKHR :: VkDevice ->
  VkSwapchainKHR ->
    Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult
vkAcquireNextImageKHR d = (mkvkAcquireNextImageKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkAcquireNextImageKHR" $ vkGetDeviceProcAddr d

data VkPresentInfoKHR =
  VkPresentInfoKHR{ vkSType :: VkStructureType 
                  , vkPNext :: Ptr Void 
                  , vkWaitSemaphoreCount :: Word32 
                  , vkPWaitSemaphores :: Ptr VkSemaphore 
                  , vkSwapchainCount :: Word32 
                  , vkPSwapchains :: Ptr VkSwapchainKHR 
                  , vkPImageIndices :: Ptr Word32 
                  , vkPResults :: Ptr VkResult 
                  }
  deriving (Eq, Ord, Show)
instance Storable VkPresentInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSwapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPResults (poked :: VkPresentInfoKHR))
newtype VkSwapchainKHR = VkSwapchainKHR Word64
  deriving (Eq, Ord, Storable, Show)
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME =  "VK_KHR_swapchain"
