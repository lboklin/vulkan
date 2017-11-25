{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHX.DeviceGroup where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             , VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
                                    , VkSwapchainCreateFlagBitsKHR(..)
                                    )
import Graphics.Vulkan.Pass( VkDependencyFlagBits(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( VkPipelineCreateFlagBits(..)
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
import Graphics.Vulkan.KHR.Surface( VkSurfaceKHR(..)
                                  )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
                            )
import Graphics.Vulkan.Constants( VK_MAX_DEVICE_GROUP_SIZE_KHX
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
import Graphics.Vulkan.Image( VkImageCreateFlagBits(..)
                            )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetDeviceProcAddr
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkOffset2D(..)
                           , VkExtent2D(..)
                           , VkRect2D(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

-- ** VkDeviceGroupPresentModeFlagsKHX
newtype VkDeviceGroupPresentModeFlagBitsKHX = VkDeviceGroupPresentModeFlagBitsKHX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkDeviceGroupPresentModeFlagBitsKHX
type VkDeviceGroupPresentModeFlagsKHX = VkDeviceGroupPresentModeFlagBitsKHX

instance Show VkDeviceGroupPresentModeFlagBitsKHX where
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX = showString "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX = showString "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX"
  
  showsPrec p (VkDeviceGroupPresentModeFlagBitsKHX x) = showParen (p >= 11) (showString "VkDeviceGroupPresentModeFlagBitsKHX " . showsPrec 11 x)

instance Read VkDeviceGroupPresentModeFlagBitsKHX where
  readPrec = parens ( choose [ ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX", pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX", pure VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX", pure VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX", pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceGroupPresentModeFlagBitsKHX")
                        v <- step readPrec
                        pure (VkDeviceGroupPresentModeFlagBitsKHX v)
                        )
                    )
-- | Present from local memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHX = VkDeviceGroupPresentModeFlagBitsKHX 0x1
-- | Present from remote memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHX = VkDeviceGroupPresentModeFlagBitsKHX 0x2
-- | Present sum of local and/or remote memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHX = VkDeviceGroupPresentModeFlagBitsKHX 0x4
-- | Each physical device presents from local memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHX = VkDeviceGroupPresentModeFlagBitsKHX 0x8

data VkImageSwapchainCreateInfoKHX =
  VkImageSwapchainCreateInfoKHX{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkSwapchain :: VkSwapchainKHR 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkImageSwapchainCreateInfoKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSwapchainCreateInfoKHX <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSwapchainCreateInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageSwapchainCreateInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkImageSwapchainCreateInfoKHX))
pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHX = VkStructureType 1000060010

data VkBindImageMemorySwapchainInfoKHX =
  VkBindImageMemorySwapchainInfoKHX{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkSwapchain :: VkSwapchainKHR 
                                   , vkImageIndex :: Word32 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkBindImageMemorySwapchainInfoKHX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkBindImageMemorySwapchainInfoKHX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemorySwapchainInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemorySwapchainInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkBindImageMemorySwapchainInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkImageIndex (poked :: VkBindImageMemorySwapchainInfoKHX))

data VkMemoryAllocateFlagsInfoKHX =
  VkMemoryAllocateFlagsInfoKHX{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkFlags :: VkMemoryAllocateFlagsKHX 
                              , vkDeviceMask :: Word32 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkMemoryAllocateFlagsInfoKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateFlagsInfoKHX <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateFlagsInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateFlagsInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMemoryAllocateFlagsInfoKHX))
                *> poke (ptr `plusPtr` 20) (vkDeviceMask (poked :: VkMemoryAllocateFlagsInfoKHX))
-- ** vkCmdDispatchBaseKHX
foreign import ccall "dynamic" mkvkCmdDispatchBaseKHX :: FunPtr (VkCommandBuffer ->
  Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer ->
  Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ())
vkCmdDispatchBaseKHX :: VkInstance ->
  VkCommandBuffer ->
    Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
vkCmdDispatchBaseKHX i = (mkvkCmdDispatchBaseKHX $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDispatchBaseKHX" $ vkGetInstanceProcAddr i
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHX = VkStructureType 1000060004

data VkDeviceGroupSubmitInfoKHX =
  VkDeviceGroupSubmitInfoKHX{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkWaitSemaphoreCount :: Word32 
                            , vkPWaitSemaphoreDeviceIndices :: Ptr Word32 
                            , vkCommandBufferCount :: Word32 
                            , vkPCommandBufferDeviceMasks :: Ptr Word32 
                            , vkSignalSemaphoreCount :: Word32 
                            , vkPSignalSemaphoreDeviceIndices :: Ptr Word32 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupSubmitInfoKHX where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSubmitInfoKHX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
                                        <*> peek (ptr `plusPtr` 48)
                                        <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 32) (vkCommandBufferCount (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 40) (vkPCommandBufferDeviceMasks (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 48) (vkSignalSemaphoreCount (poked :: VkDeviceGroupSubmitInfoKHX))
                *> poke (ptr `plusPtr` 56) (vkPSignalSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfoKHX))
pattern VK_IMAGE_CREATE_BIND_SFR_BIT_KHX = VkImageCreateFlagBits 0x40

data VkDeviceGroupRenderPassBeginInfoKHX =
  VkDeviceGroupRenderPassBeginInfoKHX{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkDeviceMask :: Word32 
                                     , vkDeviceRenderAreaCount :: Word32 
                                     , vkPDeviceRenderAreas :: Ptr VkRect2D 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupRenderPassBeginInfoKHX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupRenderPassBeginInfoKHX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupRenderPassBeginInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupRenderPassBeginInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupRenderPassBeginInfoKHX))
                *> poke (ptr `plusPtr` 20) (vkDeviceRenderAreaCount (poked :: VkDeviceGroupRenderPassBeginInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPDeviceRenderAreas (poked :: VkDeviceGroupRenderPassBeginInfoKHX))
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHX = VkPipelineCreateFlagBits 0x10
-- ** VkPeerMemoryFeatureFlagsKHX
newtype VkPeerMemoryFeatureFlagBitsKHX = VkPeerMemoryFeatureFlagBitsKHX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkPeerMemoryFeatureFlagBitsKHX
type VkPeerMemoryFeatureFlagsKHX = VkPeerMemoryFeatureFlagBitsKHX

instance Show VkPeerMemoryFeatureFlagBitsKHX where
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX"
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX"
  
  showsPrec p (VkPeerMemoryFeatureFlagBitsKHX x) = showParen (p >= 11) (showString "VkPeerMemoryFeatureFlagBitsKHX " . showsPrec 11 x)

instance Read VkPeerMemoryFeatureFlagBitsKHX where
  readPrec = parens ( choose [ ("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX", pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX)
                             , ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX", pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX", pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX", pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPeerMemoryFeatureFlagBitsKHX")
                        v <- step readPrec
                        pure (VkPeerMemoryFeatureFlagBitsKHX v)
                        )
                    )
-- | Can read with vkCmdCopy commands
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHX = VkPeerMemoryFeatureFlagBitsKHX 0x1
-- | Can write with vkCmdCopy commands
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHX = VkPeerMemoryFeatureFlagBitsKHX 0x2
-- | Can read with any access type/command
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHX = VkPeerMemoryFeatureFlagBitsKHX 0x4
-- | Can write with and access type/command
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHX = VkPeerMemoryFeatureFlagBitsKHX 0x8
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHX = VkStructureType 1000060006
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHX = VkStructureType 1000060003

data VkDeviceGroupSwapchainCreateInfoKHX =
  VkDeviceGroupSwapchainCreateInfoKHX{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkModes :: VkDeviceGroupPresentModeFlagsKHX 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupSwapchainCreateInfoKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSwapchainCreateInfoKHX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSwapchainCreateInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupSwapchainCreateInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkModes (poked :: VkDeviceGroupSwapchainCreateInfoKHX))
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHX = VkStructureType 1000060012
pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHX = VkStructureType 1000060008
-- ** VkMemoryAllocateFlagsKHX
newtype VkMemoryAllocateFlagBitsKHX = VkMemoryAllocateFlagBitsKHX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkMemoryAllocateFlagBitsKHX
type VkMemoryAllocateFlagsKHX = VkMemoryAllocateFlagBitsKHX

instance Show VkMemoryAllocateFlagBitsKHX where
  showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX"
  
  showsPrec p (VkMemoryAllocateFlagBitsKHX x) = showParen (p >= 11) (showString "VkMemoryAllocateFlagBitsKHX " . showsPrec 11 x)

instance Read VkMemoryAllocateFlagBitsKHX where
  readPrec = parens ( choose [ ("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX", pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryAllocateFlagBitsKHX")
                        v <- step readPrec
                        pure (VkMemoryAllocateFlagBitsKHX v)
                        )
                    )
-- | Force allocation on specific devices
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX = VkMemoryAllocateFlagBitsKHX 0x1
-- ** vkGetPhysicalDevicePresentRectanglesKHX
foreign import ccall "dynamic" mkvkGetPhysicalDevicePresentRectanglesKHX :: FunPtr (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkRect2D -> IO VkResult) -> (VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr Word32 -> Ptr VkRect2D -> IO VkResult)
vkGetPhysicalDevicePresentRectanglesKHX :: VkInstance ->
  VkPhysicalDevice ->
    VkSurfaceKHR -> Ptr Word32 -> Ptr VkRect2D -> IO VkResult
vkGetPhysicalDevicePresentRectanglesKHX i = (mkvkGetPhysicalDevicePresentRectanglesKHX $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDevicePresentRectanglesKHX" $ vkGetInstanceProcAddr i

data VkAcquireNextImageInfoKHX =
  VkAcquireNextImageInfoKHX{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkSwapchain :: VkSwapchainKHR 
                           , vkTimeout :: Word64 
                           , vkSemaphore :: VkSemaphore 
                           , vkFence :: VkFence 
                           , vkDeviceMask :: Word32 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkAcquireNextImageInfoKHX where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkAcquireNextImageInfoKHX <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkTimeout (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 32) (vkSemaphore (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 40) (vkFence (poked :: VkAcquireNextImageInfoKHX))
                *> poke (ptr `plusPtr` 48) (vkDeviceMask (poked :: VkAcquireNextImageInfoKHX))
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHX = VkStructureType 1000060013
-- ** vkCmdSetDeviceMaskKHX
foreign import ccall "dynamic" mkvkCmdSetDeviceMaskKHX :: FunPtr (VkCommandBuffer -> Word32 -> IO ()) -> (VkCommandBuffer -> Word32 -> IO ())
vkCmdSetDeviceMaskKHX :: VkInstance -> VkCommandBuffer -> Word32 -> IO ()
vkCmdSetDeviceMaskKHX i = (mkvkCmdSetDeviceMaskKHX $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdSetDeviceMaskKHX" $ vkGetInstanceProcAddr i

data VkBindImageMemoryDeviceGroupInfoKHX =
  VkBindImageMemoryDeviceGroupInfoKHX{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkDeviceIndexCount :: Word32 
                                     , vkPDeviceIndices :: Ptr Word32 
                                     , vkSFRRectCount :: Word32 
                                     , vkPSFRRects :: Ptr VkRect2D 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkBindImageMemoryDeviceGroupInfoKHX where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryDeviceGroupInfoKHX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPDeviceIndices (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 32) (vkSFRRectCount (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 40) (vkPSFRRects (poked :: VkBindImageMemoryDeviceGroupInfoKHX))
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHX = VkPipelineCreateFlagBits 0x8
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHX = VkDependencyFlagBits 0x4
-- ** vkAcquireNextImage2KHX
foreign import ccall "dynamic" mkvkAcquireNextImage2KHX :: FunPtr (VkDevice ->
  Ptr VkAcquireNextImageInfoKHX -> Ptr Word32 -> IO VkResult) -> (VkDevice ->
  Ptr VkAcquireNextImageInfoKHX -> Ptr Word32 -> IO VkResult)
vkAcquireNextImage2KHX :: VkDevice ->
  Ptr VkAcquireNextImageInfoKHX -> Ptr Word32 -> IO VkResult
vkAcquireNextImage2KHX d = (mkvkAcquireNextImage2KHX $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkAcquireNextImage2KHX" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHX = VkStructureType 1000060009
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHX = VkStructureType 1000060007

data VkDeviceGroupPresentCapabilitiesKHX =
  VkDeviceGroupPresentCapabilitiesKHX{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkPresentMask :: Vector VK_MAX_DEVICE_GROUP_SIZE_KHX Word32 
                                     , vkModes :: VkDeviceGroupPresentModeFlagsKHX 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupPresentCapabilitiesKHX where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek ptr = VkDeviceGroupPresentCapabilitiesKHX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 144)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupPresentCapabilitiesKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupPresentCapabilitiesKHX))
                *> poke (ptr `plusPtr` 16) (vkPresentMask (poked :: VkDeviceGroupPresentCapabilitiesKHX))
                *> poke (ptr `plusPtr` 144) (vkModes (poked :: VkDeviceGroupPresentCapabilitiesKHX))
-- ** vkGetDeviceGroupPeerMemoryFeaturesKHX
foreign import ccall "dynamic" mkvkGetDeviceGroupPeerMemoryFeaturesKHX :: FunPtr (VkDevice ->
  Word32 ->
    Word32 -> Word32 -> Ptr VkPeerMemoryFeatureFlagsKHX -> IO ()) -> (VkDevice ->
  Word32 ->
    Word32 -> Word32 -> Ptr VkPeerMemoryFeatureFlagsKHX -> IO ())
vkGetDeviceGroupPeerMemoryFeaturesKHX :: VkDevice ->
  Word32 ->
    Word32 -> Word32 -> Ptr VkPeerMemoryFeatureFlagsKHX -> IO ()
vkGetDeviceGroupPeerMemoryFeaturesKHX d = (mkvkGetDeviceGroupPeerMemoryFeaturesKHX $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetDeviceGroupPeerMemoryFeaturesKHX" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHX = VkStructureType 1000060014
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHX = VkStructureType 1000060011

data VkDeviceGroupPresentInfoKHX =
  VkDeviceGroupPresentInfoKHX{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkSwapchainCount :: Word32 
                             , vkPDeviceMasks :: Ptr Word32 
                             , vkMode :: VkDeviceGroupPresentModeFlagBitsKHX 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupPresentInfoKHX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceGroupPresentInfoKHX <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupPresentInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupPresentInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkDeviceGroupPresentInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPDeviceMasks (poked :: VkDeviceGroupPresentInfoKHX))
                *> poke (ptr `plusPtr` 32) (vkMode (poked :: VkDeviceGroupPresentInfoKHX))
-- ** vkGetDeviceGroupSurfacePresentModesKHX
foreign import ccall "dynamic" mkvkGetDeviceGroupSurfacePresentModesKHX :: FunPtr (VkDevice ->
  VkSurfaceKHR -> Ptr VkDeviceGroupPresentModeFlagsKHX -> IO VkResult) -> (VkDevice ->
  VkSurfaceKHR -> Ptr VkDeviceGroupPresentModeFlagsKHX -> IO VkResult)
vkGetDeviceGroupSurfacePresentModesKHX :: VkDevice ->
  VkSurfaceKHR -> Ptr VkDeviceGroupPresentModeFlagsKHX -> IO VkResult
vkGetDeviceGroupSurfacePresentModesKHX d = (mkvkGetDeviceGroupSurfacePresentModesKHX $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetDeviceGroupSurfacePresentModesKHX" $ vkGetDeviceProcAddr d
pattern VK_SWAPCHAIN_CREATE_BIND_SFR_BIT_KHX = VkSwapchainCreateFlagBitsKHR 0x1
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHX = VkStructureType 1000060000
pattern VK_KHX_DEVICE_GROUP_SPEC_VERSION =  0x2

data VkDeviceGroupBindSparseInfoKHX =
  VkDeviceGroupBindSparseInfoKHX{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkResourceDeviceIndex :: Word32 
                                , vkMemoryDeviceIndex :: Word32 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupBindSparseInfoKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupBindSparseInfoKHX <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupBindSparseInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupBindSparseInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkResourceDeviceIndex (poked :: VkDeviceGroupBindSparseInfoKHX))
                *> poke (ptr `plusPtr` 20) (vkMemoryDeviceIndex (poked :: VkDeviceGroupBindSparseInfoKHX))
-- ** vkGetDeviceGroupPresentCapabilitiesKHX
foreign import ccall "dynamic" mkvkGetDeviceGroupPresentCapabilitiesKHX :: FunPtr (VkDevice -> Ptr VkDeviceGroupPresentCapabilitiesKHX -> IO VkResult) -> (VkDevice -> Ptr VkDeviceGroupPresentCapabilitiesKHX -> IO VkResult)
vkGetDeviceGroupPresentCapabilitiesKHX :: VkDevice -> Ptr VkDeviceGroupPresentCapabilitiesKHX -> IO VkResult
vkGetDeviceGroupPresentCapabilitiesKHX d = (mkvkGetDeviceGroupPresentCapabilitiesKHX $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetDeviceGroupPresentCapabilitiesKHX" $ vkGetDeviceProcAddr d
pattern VK_KHX_DEVICE_GROUP_EXTENSION_NAME =  "VK_KHX_device_group"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHX = VkStructureType 1000060005

data VkDeviceGroupCommandBufferBeginInfoKHX =
  VkDeviceGroupCommandBufferBeginInfoKHX{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkDeviceMask :: Word32 
                                        }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupCommandBufferBeginInfoKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupCommandBufferBeginInfoKHX <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupCommandBufferBeginInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupCommandBufferBeginInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupCommandBufferBeginInfoKHX))

data VkBindBufferMemoryDeviceGroupInfoKHX =
  VkBindBufferMemoryDeviceGroupInfoKHX{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkDeviceIndexCount :: Word32 
                                      , vkPDeviceIndices :: Ptr Word32 
                                      }
  deriving (Eq, Ord, Show)
instance Storable VkBindBufferMemoryDeviceGroupInfoKHX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryDeviceGroupInfoKHX <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindBufferMemoryDeviceGroupInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPDeviceIndices (poked :: VkBindBufferMemoryDeviceGroupInfoKHX))
