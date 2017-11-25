{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.GOOGLE.DisplayTiming where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
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
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( vkGetDeviceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkResult(..)
                           )


data VkPastPresentationTimingGOOGLE =
  VkPastPresentationTimingGOOGLE{ vkPresentID :: Word32 
                                , vkDesiredPresentTime :: Word64 
                                , vkActualPresentTime :: Word64 
                                , vkEarliestPresentTime :: Word64 
                                , vkPresentMargin :: Word64 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkPastPresentationTimingGOOGLE where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPastPresentationTimingGOOGLE <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPresentID (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkDesiredPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 16) (vkActualPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 24) (vkEarliestPresentTime (poked :: VkPastPresentationTimingGOOGLE))
                *> poke (ptr `plusPtr` 32) (vkPresentMargin (poked :: VkPastPresentationTimingGOOGLE))
pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = VkStructureType 1000092000
pattern VK_GOOGLE_DISPLAY_TIMING_SPEC_VERSION =  0x1

data VkPresentTimeGOOGLE =
  VkPresentTimeGOOGLE{ vkPresentID :: Word32 
                     , vkDesiredPresentTime :: Word64 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkPresentTimeGOOGLE where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPresentTimeGOOGLE <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPresentID (poked :: VkPresentTimeGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkDesiredPresentTime (poked :: VkPresentTimeGOOGLE))
pattern VK_GOOGLE_DISPLAY_TIMING_EXTENSION_NAME =  "VK_GOOGLE_display_timing"
-- ** vkGetPastPresentationTimingGOOGLE
foreign import ccall "dynamic" mkvkGetPastPresentationTimingGOOGLE :: FunPtr (VkDevice ->
  VkSwapchainKHR ->
    Ptr Word32 -> Ptr VkPastPresentationTimingGOOGLE -> IO VkResult) -> (VkDevice ->
  VkSwapchainKHR ->
    Ptr Word32 -> Ptr VkPastPresentationTimingGOOGLE -> IO VkResult)
vkGetPastPresentationTimingGOOGLE :: VkDevice ->
  VkSwapchainKHR ->
    Ptr Word32 -> Ptr VkPastPresentationTimingGOOGLE -> IO VkResult
vkGetPastPresentationTimingGOOGLE d = (mkvkGetPastPresentationTimingGOOGLE $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetPastPresentationTimingGOOGLE" $ vkGetDeviceProcAddr d
-- ** vkGetRefreshCycleDurationGOOGLE
foreign import ccall "dynamic" mkvkGetRefreshCycleDurationGOOGLE :: FunPtr (VkDevice ->
  VkSwapchainKHR -> Ptr VkRefreshCycleDurationGOOGLE -> IO VkResult) -> (VkDevice ->
  VkSwapchainKHR -> Ptr VkRefreshCycleDurationGOOGLE -> IO VkResult)
vkGetRefreshCycleDurationGOOGLE :: VkDevice ->
  VkSwapchainKHR -> Ptr VkRefreshCycleDurationGOOGLE -> IO VkResult
vkGetRefreshCycleDurationGOOGLE d = (mkvkGetRefreshCycleDurationGOOGLE $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetRefreshCycleDurationGOOGLE" $ vkGetDeviceProcAddr d

data VkRefreshCycleDurationGOOGLE =
  VkRefreshCycleDurationGOOGLE{ vkRefreshDuration :: Word64 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkRefreshCycleDurationGOOGLE where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek ptr = VkRefreshCycleDurationGOOGLE <$> peek (ptr `plusPtr` 0)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRefreshDuration (poked :: VkRefreshCycleDurationGOOGLE))

data VkPresentTimesInfoGOOGLE =
  VkPresentTimesInfoGOOGLE{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkSwapchainCount :: Word32 
                          , vkPTimes :: Ptr VkPresentTimeGOOGLE 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkPresentTimesInfoGOOGLE where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPresentTimesInfoGOOGLE <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkPresentTimesInfoGOOGLE))
                *> poke (ptr `plusPtr` 24) (vkPTimes (poked :: VkPresentTimesInfoGOOGLE))
