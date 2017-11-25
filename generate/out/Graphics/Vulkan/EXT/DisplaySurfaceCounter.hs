{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.DisplaySurfaceCounter where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkSurfaceTransformFlagsKHR(..)
                                  , VkCompositeAlphaFlagBitsKHR(..)
                                  , VkCompositeAlphaFlagsKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
                                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
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
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )


data VkSurfaceCapabilities2EXT =
  VkSurfaceCapabilities2EXT{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkMinImageCount :: Word32 
                           , vkMaxImageCount :: Word32 
                           , vkCurrentExtent :: VkExtent2D 
                           , vkMinImageExtent :: VkExtent2D 
                           , vkMaxImageExtent :: VkExtent2D 
                           , vkMaxImageArrayLayers :: Word32 
                           , vkSupportedTransforms :: VkSurfaceTransformFlagsKHR 
                           , vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR 
                           , vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR 
                           , vkSupportedUsageFlags :: VkImageUsageFlags 
                           , vkSupportedSurfaceCounters :: VkSurfaceCounterFlagsEXT 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkSurfaceCapabilities2EXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSurfaceCapabilities2EXT <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 48)
                                       <*> peek (ptr `plusPtr` 52)
                                       <*> peek (ptr `plusPtr` 56)
                                       <*> peek (ptr `plusPtr` 60)
                                       <*> peek (ptr `plusPtr` 64)
                                       <*> peek (ptr `plusPtr` 68)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 16) (vkMinImageCount (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 20) (vkMaxImageCount (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 24) (vkCurrentExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 32) (vkMinImageExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 40) (vkMaxImageExtent (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 48) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 52) (vkSupportedTransforms (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 56) (vkCurrentTransform (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 60) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 64) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilities2EXT))
                *> poke (ptr `plusPtr` 68) (vkSupportedSurfaceCounters (poked :: VkSurfaceCapabilities2EXT))
-- ** vkGetPhysicalDeviceSurfaceCapabilities2EXT
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilities2EXT" vkGetPhysicalDeviceSurfaceCapabilities2EXT ::
  VkPhysicalDevice ->
  VkSurfaceKHR -> Ptr VkSurfaceCapabilities2EXT -> IO VkResult
-- ** VkSurfaceCounterFlagsEXT
newtype VkSurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSurfaceCounterFlagBitsEXT
type VkSurfaceCounterFlagsEXT = VkSurfaceCounterFlagBitsEXT

instance Show VkSurfaceCounterFlagBitsEXT where
  showsPrec _ VK_SURFACE_COUNTER_VBLANK_EXT = showString "VK_SURFACE_COUNTER_VBLANK_EXT"
  
  showsPrec p (VkSurfaceCounterFlagBitsEXT x) = showParen (p >= 11) (showString "VkSurfaceCounterFlagBitsEXT " . showsPrec 11 x)

instance Read VkSurfaceCounterFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_SURFACE_COUNTER_VBLANK_EXT", pure VK_SURFACE_COUNTER_VBLANK_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSurfaceCounterFlagBitsEXT")
                        v <- step readPrec
                        pure (VkSurfaceCounterFlagBitsEXT v)
                        )
                    )

pattern VK_SURFACE_COUNTER_VBLANK_EXT = VkSurfaceCounterFlagBitsEXT 0x1
