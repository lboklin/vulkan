{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.OtherTypes where

import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Graphics.Vulkan.Pass( VkAccessFlagBits(..)
                           , VkAccessFlags(..)
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
import Graphics.Vulkan.Image( VkImageSubresourceRange(..)
                            , VkImageLayout(..)
                            , VkImage(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )


data VkBufferMemoryBarrier =
  VkBufferMemoryBarrier{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkSrcAccessMask :: VkAccessFlags 
                       , vkDstAccessMask :: VkAccessFlags 
                       , vkSrcQueueFamilyIndex :: Word32 
                       , vkDstQueueFamilyIndex :: Word32 
                       , vkBuffer :: VkBuffer 
                       , vkOffset :: VkDeviceSize 
                       , vkSize :: VkDeviceSize 
                       }
  deriving (Eq, Ord, Show)
instance Storable VkBufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))

data VkDrawIndexedIndirectCommand =
  VkDrawIndexedIndirectCommand{ vkIndexCount :: Word32 
                              , vkInstanceCount :: Word32 
                              , vkFirstIndex :: Word32 
                              , vkVertexOffset :: Int32 
                              , vkFirstInstance :: Word32 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkDrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkIndexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkVertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (vkFirstInstance (poked :: VkDrawIndexedIndirectCommand))

data VkImageMemoryBarrier =
  VkImageMemoryBarrier{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkSrcAccessMask :: VkAccessFlags 
                      , vkDstAccessMask :: VkAccessFlags 
                      , vkOldLayout :: VkImageLayout 
                      , vkNewLayout :: VkImageLayout 
                      , vkSrcQueueFamilyIndex :: Word32 
                      , vkDstQueueFamilyIndex :: Word32 
                      , vkImage :: VkImage 
                      , vkSubresourceRange :: VkImageSubresourceRange 
                      }
  deriving (Eq, Ord, Show)
instance Storable VkImageMemoryBarrier where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))

data VkMemoryBarrier =
  VkMemoryBarrier{ vkSType :: VkStructureType 
                 , vkPNext :: Ptr Void 
                 , vkSrcAccessMask :: VkAccessFlags 
                 , vkDstAccessMask :: VkAccessFlags 
                 }
  deriving (Eq, Ord, Show)
instance Storable VkMemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))
-- ** VkObjectType-- | Enums to track objects of various types
newtype VkObjectType = VkObjectType Int32
  deriving (Eq, Ord, Storable)

instance Show VkObjectType where
  showsPrec _ VK_OBJECT_TYPE_UNKNOWN = showString "VK_OBJECT_TYPE_UNKNOWN"
  showsPrec _ VK_OBJECT_TYPE_INSTANCE = showString "VK_OBJECT_TYPE_INSTANCE"
  showsPrec _ VK_OBJECT_TYPE_PHYSICAL_DEVICE = showString "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE = showString "VK_OBJECT_TYPE_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_QUEUE = showString "VK_OBJECT_TYPE_QUEUE"
  showsPrec _ VK_OBJECT_TYPE_SEMAPHORE = showString "VK_OBJECT_TYPE_SEMAPHORE"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_BUFFER = showString "VK_OBJECT_TYPE_COMMAND_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_FENCE = showString "VK_OBJECT_TYPE_FENCE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE_MEMORY = showString "VK_OBJECT_TYPE_DEVICE_MEMORY"
  showsPrec _ VK_OBJECT_TYPE_BUFFER = showString "VK_OBJECT_TYPE_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_IMAGE = showString "VK_OBJECT_TYPE_IMAGE"
  showsPrec _ VK_OBJECT_TYPE_EVENT = showString "VK_OBJECT_TYPE_EVENT"
  showsPrec _ VK_OBJECT_TYPE_QUERY_POOL = showString "VK_OBJECT_TYPE_QUERY_POOL"
  showsPrec _ VK_OBJECT_TYPE_BUFFER_VIEW = showString "VK_OBJECT_TYPE_BUFFER_VIEW"
  showsPrec _ VK_OBJECT_TYPE_IMAGE_VIEW = showString "VK_OBJECT_TYPE_IMAGE_VIEW"
  showsPrec _ VK_OBJECT_TYPE_SHADER_MODULE = showString "VK_OBJECT_TYPE_SHADER_MODULE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_CACHE = showString "VK_OBJECT_TYPE_PIPELINE_CACHE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_LAYOUT = showString "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_RENDER_PASS = showString "VK_OBJECT_TYPE_RENDER_PASS"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE = showString "VK_OBJECT_TYPE_PIPELINE"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_SAMPLER = showString "VK_OBJECT_TYPE_SAMPLER"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_POOL = showString "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET"
  showsPrec _ VK_OBJECT_TYPE_FRAMEBUFFER = showString "VK_OBJECT_TYPE_FRAMEBUFFER"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_POOL = showString "VK_OBJECT_TYPE_COMMAND_POOL"
  showsPrec p (VkObjectType x) = showParen (p >= 11) (showString "VkObjectType " . showsPrec 11 x)

instance Read VkObjectType where
  readPrec = parens ( choose [ ("VK_OBJECT_TYPE_UNKNOWN", pure VK_OBJECT_TYPE_UNKNOWN)
                             , ("VK_OBJECT_TYPE_INSTANCE", pure VK_OBJECT_TYPE_INSTANCE)
                             , ("VK_OBJECT_TYPE_PHYSICAL_DEVICE", pure VK_OBJECT_TYPE_PHYSICAL_DEVICE)
                             , ("VK_OBJECT_TYPE_DEVICE", pure VK_OBJECT_TYPE_DEVICE)
                             , ("VK_OBJECT_TYPE_QUEUE", pure VK_OBJECT_TYPE_QUEUE)
                             , ("VK_OBJECT_TYPE_SEMAPHORE", pure VK_OBJECT_TYPE_SEMAPHORE)
                             , ("VK_OBJECT_TYPE_COMMAND_BUFFER", pure VK_OBJECT_TYPE_COMMAND_BUFFER)
                             , ("VK_OBJECT_TYPE_FENCE", pure VK_OBJECT_TYPE_FENCE)
                             , ("VK_OBJECT_TYPE_DEVICE_MEMORY", pure VK_OBJECT_TYPE_DEVICE_MEMORY)
                             , ("VK_OBJECT_TYPE_BUFFER", pure VK_OBJECT_TYPE_BUFFER)
                             , ("VK_OBJECT_TYPE_IMAGE", pure VK_OBJECT_TYPE_IMAGE)
                             , ("VK_OBJECT_TYPE_EVENT", pure VK_OBJECT_TYPE_EVENT)
                             , ("VK_OBJECT_TYPE_QUERY_POOL", pure VK_OBJECT_TYPE_QUERY_POOL)
                             , ("VK_OBJECT_TYPE_BUFFER_VIEW", pure VK_OBJECT_TYPE_BUFFER_VIEW)
                             , ("VK_OBJECT_TYPE_IMAGE_VIEW", pure VK_OBJECT_TYPE_IMAGE_VIEW)
                             , ("VK_OBJECT_TYPE_SHADER_MODULE", pure VK_OBJECT_TYPE_SHADER_MODULE)
                             , ("VK_OBJECT_TYPE_PIPELINE_CACHE", pure VK_OBJECT_TYPE_PIPELINE_CACHE)
                             , ("VK_OBJECT_TYPE_PIPELINE_LAYOUT", pure VK_OBJECT_TYPE_PIPELINE_LAYOUT)
                             , ("VK_OBJECT_TYPE_RENDER_PASS", pure VK_OBJECT_TYPE_RENDER_PASS)
                             , ("VK_OBJECT_TYPE_PIPELINE", pure VK_OBJECT_TYPE_PIPELINE)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT", pure VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT)
                             , ("VK_OBJECT_TYPE_SAMPLER", pure VK_OBJECT_TYPE_SAMPLER)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_POOL", pure VK_OBJECT_TYPE_DESCRIPTOR_POOL)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET", pure VK_OBJECT_TYPE_DESCRIPTOR_SET)
                             , ("VK_OBJECT_TYPE_FRAMEBUFFER", pure VK_OBJECT_TYPE_FRAMEBUFFER)
                             , ("VK_OBJECT_TYPE_COMMAND_POOL", pure VK_OBJECT_TYPE_COMMAND_POOL)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectType")
                        v <- step readPrec
                        pure (VkObjectType v)
                        )
                    )

pattern VK_OBJECT_TYPE_UNKNOWN = VkObjectType 0
-- | VkInstance
pattern VK_OBJECT_TYPE_INSTANCE = VkObjectType 1
-- | VkPhysicalDevice
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE = VkObjectType 2
-- | VkDevice
pattern VK_OBJECT_TYPE_DEVICE = VkObjectType 3
-- | VkQueue
pattern VK_OBJECT_TYPE_QUEUE = VkObjectType 4
-- | VkSemaphore
pattern VK_OBJECT_TYPE_SEMAPHORE = VkObjectType 5
-- | VkCommandBuffer
pattern VK_OBJECT_TYPE_COMMAND_BUFFER = VkObjectType 6
-- | VkFence
pattern VK_OBJECT_TYPE_FENCE = VkObjectType 7
-- | VkDeviceMemory
pattern VK_OBJECT_TYPE_DEVICE_MEMORY = VkObjectType 8
-- | VkBuffer
pattern VK_OBJECT_TYPE_BUFFER = VkObjectType 9
-- | VkImage
pattern VK_OBJECT_TYPE_IMAGE = VkObjectType 10
-- | VkEvent
pattern VK_OBJECT_TYPE_EVENT = VkObjectType 11
-- | VkQueryPool
pattern VK_OBJECT_TYPE_QUERY_POOL = VkObjectType 12
-- | VkBufferView
pattern VK_OBJECT_TYPE_BUFFER_VIEW = VkObjectType 13
-- | VkImageView
pattern VK_OBJECT_TYPE_IMAGE_VIEW = VkObjectType 14
-- | VkShaderModule
pattern VK_OBJECT_TYPE_SHADER_MODULE = VkObjectType 15
-- | VkPipelineCache
pattern VK_OBJECT_TYPE_PIPELINE_CACHE = VkObjectType 16
-- | VkPipelineLayout
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT = VkObjectType 17
-- | VkRenderPass
pattern VK_OBJECT_TYPE_RENDER_PASS = VkObjectType 18
-- | VkPipeline
pattern VK_OBJECT_TYPE_PIPELINE = VkObjectType 19
-- | VkDescriptorSetLayout
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VkObjectType 20
-- | VkSampler
pattern VK_OBJECT_TYPE_SAMPLER = VkObjectType 21
-- | VkDescriptorPool
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL = VkObjectType 22
-- | VkDescriptorSet
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET = VkObjectType 23
-- | VkFramebuffer
pattern VK_OBJECT_TYPE_FRAMEBUFFER = VkObjectType 24
-- | VkCommandPool
pattern VK_OBJECT_TYPE_COMMAND_POOL = VkObjectType 25

data VkDrawIndirectCommand =
  VkDrawIndirectCommand{ vkVertexCount :: Word32 
                       , vkInstanceCount :: Word32 
                       , vkFirstVertex :: Word32 
                       , vkFirstInstance :: Word32 
                       }
  deriving (Eq, Ord, Show)
instance Storable VkDrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkFirstInstance (poked :: VkDrawIndirectCommand))

data VkDispatchIndirectCommand =
  VkDispatchIndirectCommand{ vkX :: Word32 
                           , vkY :: Word32 
                           , vkZ :: Word32 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkDispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkDispatchIndirectCommand))
