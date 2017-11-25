{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.GlobalPriority where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
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
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkResult(..)
                           )

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT = VkStructureType 1000174000
pattern VK_ERROR_NOT_PERMITTED_EXT = VkResult (-1000174001)
pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION =  0x1
-- ** VkQueueGlobalPriorityEXT
newtype VkQueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkQueueGlobalPriorityEXT where
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_LOW = showString "VK_QUEUE_GLOBAL_PRIORITY_LOW"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_MEDIUM = showString "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_HIGH = showString "VK_QUEUE_GLOBAL_PRIORITY_HIGH"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_REALTIME = showString "VK_QUEUE_GLOBAL_PRIORITY_REALTIME"
  showsPrec p (VkQueueGlobalPriorityEXT x) = showParen (p >= 11) (showString "VkQueueGlobalPriorityEXT " . showsPrec 11 x)

instance Read VkQueueGlobalPriorityEXT where
  readPrec = parens ( choose [ ("VK_QUEUE_GLOBAL_PRIORITY_LOW", pure VK_QUEUE_GLOBAL_PRIORITY_LOW)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_MEDIUM", pure VK_QUEUE_GLOBAL_PRIORITY_MEDIUM)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_HIGH", pure VK_QUEUE_GLOBAL_PRIORITY_HIGH)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_REALTIME", pure VK_QUEUE_GLOBAL_PRIORITY_REALTIME)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueueGlobalPriorityEXT")
                        v <- step readPrec
                        pure (VkQueueGlobalPriorityEXT v)
                        )
                    )

pattern VK_QUEUE_GLOBAL_PRIORITY_LOW = VkQueueGlobalPriorityEXT 128

pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM = VkQueueGlobalPriorityEXT 256

pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH = VkQueueGlobalPriorityEXT 512

pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME = VkQueueGlobalPriorityEXT 1024
pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME =  "VK_EXT_global_priority"

data VkDeviceQueueGlobalPriorityCreateInfoEXT =
  VkDeviceQueueGlobalPriorityCreateInfoEXT{ vkSType :: VkStructureType 
                                          , vkPNext :: Ptr Void 
                                          , vkGlobalPriority :: VkQueueGlobalPriorityEXT 
                                          }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceQueueGlobalPriorityCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceQueueGlobalPriorityCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkGlobalPriority (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
