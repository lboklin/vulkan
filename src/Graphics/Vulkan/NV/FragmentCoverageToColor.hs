{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NV.FragmentCoverageToColor where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )


data VkPipelineCoverageToColorStateCreateInfoNV =
  VkPipelineCoverageToColorStateCreateInfoNV{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkFlags :: VkPipelineCoverageToColorStateCreateFlagsNV 
                                            , vkCoverageToColorEnable :: VkBool32 
                                            , vkCoverageToColorLocation :: Word32 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineCoverageToColorStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineCoverageToColorStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCoverageToColorEnable (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkCoverageToColorLocation (poked :: VkPipelineCoverageToColorStateCreateInfoNV))
-- ** VkPipelineCoverageToColorStateCreateFlagsNV-- | Opaque flag
newtype VkPipelineCoverageToColorStateCreateFlagsNV = VkPipelineCoverageToColorStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
