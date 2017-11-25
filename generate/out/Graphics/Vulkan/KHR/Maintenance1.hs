{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Maintenance1 where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.CommandPool( VkCommandPool(..)
                                  )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFlags(..)
                           )

-- ** VkCommandPoolTrimFlagsKHR-- | Opaque flag
newtype VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
-- ** vkTrimCommandPoolKHR
foreign import ccall "vkTrimCommandPoolKHR" vkTrimCommandPoolKHR ::
  VkDevice -> VkCommandPool -> VkCommandPoolTrimFlagsKHR -> IO ()
