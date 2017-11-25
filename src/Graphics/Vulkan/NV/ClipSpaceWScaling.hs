{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NV.ClipSpaceWScaling where

import Graphics.Vulkan.Pipeline( VkDynamicState(..)
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
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      )

-- ** vkCmdSetViewportWScalingNV
foreign import ccall "dynamic" mkvkCmdSetViewportWScalingNV :: FunPtr (VkCommandBuffer ->
  Word32 -> Word32 -> Ptr VkViewportWScalingNV -> IO ()) -> (VkCommandBuffer ->
  Word32 -> Word32 -> Ptr VkViewportWScalingNV -> IO ())
vkCmdSetViewportWScalingNV :: VkInstance ->
  VkCommandBuffer ->
    Word32 -> Word32 -> Ptr VkViewportWScalingNV -> IO ()
vkCmdSetViewportWScalingNV i = (mkvkCmdSetViewportWScalingNV $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdSetViewportWScalingNV" $ vkGetInstanceProcAddr i
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME =  "VK_NV_clip_space_w_scaling"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = VkStructureType 1000087000

data VkViewportWScalingNV =
  VkViewportWScalingNV{ vkXcoeff :: CFloat 
                      , vkYcoeff :: CFloat 
                      }
  deriving (Eq, Ord, Show)
instance Storable VkViewportWScalingNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkViewportWScalingNV <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkXcoeff (poked :: VkViewportWScalingNV))
                *> poke (ptr `plusPtr` 4) (vkYcoeff (poked :: VkViewportWScalingNV))
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VkDynamicState 1000087000
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION =  0x1

data VkPipelineViewportWScalingStateCreateInfoNV =
  VkPipelineViewportWScalingStateCreateInfoNV{ vkSType :: VkStructureType 
                                             , vkPNext :: Ptr Void 
                                             , vkViewportWScalingEnable :: VkBool32 
                                             , vkViewportCount :: Word32 
                                             , vkPViewportWScalings :: Ptr VkViewportWScalingNV 
                                             }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportWScalingStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 20)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkViewportWScalingEnable (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportWScalings (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
