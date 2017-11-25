{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.DiscardRectangles where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
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
import Data.Int( Int32
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
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkOffset2D(..)
                           , VkExtent2D(..)
                           , VkRect2D(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )

pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT = VkStructureType 1000099001

data VkPhysicalDeviceDiscardRectanglePropertiesEXT =
  VkPhysicalDeviceDiscardRectanglePropertiesEXT{ vkSType :: VkStructureType 
                                               , vkPNext :: Ptr Void 
                                               , vkMaxDiscardRectangles :: Word32 
                                               }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDiscardRectanglePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxDiscardRectangles (poked :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))
-- ** VkPipelineDiscardRectangleStateCreateFlagsEXT-- | Opaque flag
newtype VkPipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT = VkStructureType 1000099000
-- ** vkCmdSetDiscardRectangleEXT
foreign import ccall "dynamic" mkvkCmdSetDiscardRectangleEXT :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ())
vkCmdSetDiscardRectangleEXT :: VkInstance ->
  VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()
vkCmdSetDiscardRectangleEXT i = (mkvkCmdSetDiscardRectangleEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdSetDiscardRectangleEXT" $ vkGetInstanceProcAddr i
pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION =  0x1
pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT = VkDynamicState 1000099000
pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME =  "VK_EXT_discard_rectangles"

data VkPipelineDiscardRectangleStateCreateInfoEXT =
  VkPipelineDiscardRectangleStateCreateInfoEXT{ vkSType :: VkStructureType 
                                              , vkPNext :: Ptr Void 
                                              , vkFlags :: VkPipelineDiscardRectangleStateCreateFlagsEXT 
                                              , vkDiscardRectangleMode :: VkDiscardRectangleModeEXT 
                                              , vkDiscardRectangleCount :: Word32 
                                              , vkPDiscardRectangles :: Ptr VkRect2D 
                                              }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineDiscardRectangleStateCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineDiscardRectangleStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 24)
                                                          <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkDiscardRectangleMode (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkDiscardRectangleCount (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPDiscardRectangles (poked :: VkPipelineDiscardRectangleStateCreateInfoEXT))
-- ** VkDiscardRectangleModeEXT
newtype VkDiscardRectangleModeEXT = VkDiscardRectangleModeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDiscardRectangleModeEXT where
  showsPrec _ VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = showString "VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT"
  showsPrec _ VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = showString "VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT"
  showsPrec p (VkDiscardRectangleModeEXT x) = showParen (p >= 11) (showString "VkDiscardRectangleModeEXT " . showsPrec 11 x)

instance Read VkDiscardRectangleModeEXT where
  readPrec = parens ( choose [ ("VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT", pure VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT)
                             , ("VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT", pure VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDiscardRectangleModeEXT")
                        v <- step readPrec
                        pure (VkDiscardRectangleModeEXT v)
                        )
                    )

pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = VkDiscardRectangleModeEXT 0

pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = VkDiscardRectangleModeEXT 1
