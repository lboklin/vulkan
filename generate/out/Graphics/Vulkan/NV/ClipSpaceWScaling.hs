{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.NV.ClipSpaceWScaling where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      )

-- ** vkCmdSetViewportWScalingNV
foreign import ccall "vkCmdSetViewportWScalingNV" vkCmdSetViewportWScalingNV ::
  VkCommandBuffer ->
  Word32 -> Word32 -> Ptr VkViewportWScalingNV -> IO ()

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
