{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NVX.DeviceGeneratedCommands where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             , VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( VkPipelineBindPoint(..)
                               , VkPipeline(..)
                               )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( VkDescriptorSet(..)
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
import Graphics.Vulkan.Memory( VkSystemAllocationScope(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkFreeFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkInternalAllocationType(..)
                             , PFN_vkInternalFreeNotification
                             )
import Graphics.Vulkan.CommandBufferBuilding( VkIndexType(..)
                                            )
import Graphics.Vulkan.PipelineLayout( VkPipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Shader( VkShaderStageFlags(..)
                             , VkShaderStageFlagBits(..)
                             )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

newtype VkIndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX Word64
  deriving (Eq, Ord, Storable, Show)

data VkObjectTablePushConstantEntryNVX =
  VkObjectTablePushConstantEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                                   , vkFlags :: VkObjectEntryUsageFlagsNVX 
                                   , vkPipelineLayout :: VkPipelineLayout 
                                   , vkStageFlags :: VkShaderStageFlags 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTablePushConstantEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTablePushConstantEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkStageFlags (poked :: VkObjectTablePushConstantEntryNVX))
-- ** vkCreateIndirectCommandsLayoutNVX
foreign import ccall "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX ::
  VkDevice ->
  Ptr VkIndirectCommandsLayoutCreateInfoNVX ->
    Ptr VkAllocationCallbacks ->
      Ptr VkIndirectCommandsLayoutNVX -> IO VkResult
-- ** vkCmdReserveSpaceForCommandsNVX
foreign import ccall "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX ::
  VkCommandBuffer -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -> IO ()
-- ** vkDestroyObjectTableNVX
foreign import ccall "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX ::
  VkDevice -> VkObjectTableNVX -> Ptr VkAllocationCallbacks -> IO ()

data VkCmdReserveSpaceForCommandsInfoNVX =
  VkCmdReserveSpaceForCommandsInfoNVX{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkObjectTable :: VkObjectTableNVX 
                                     , vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX 
                                     , vkMaxSequencesCount :: Word32 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkCmdReserveSpaceForCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkMaxSequencesCount (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
-- ** vkUnregisterObjectsNVX
foreign import ccall "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX ::
  VkDevice ->
  VkObjectTableNVX ->
    Word32 -> Ptr VkObjectEntryTypeNVX -> Ptr Word32 -> IO VkResult
-- ** vkCmdProcessCommandsNVX
foreign import ccall "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX ::
  VkCommandBuffer -> Ptr VkCmdProcessCommandsInfoNVX -> IO ()

data VkCmdProcessCommandsInfoNVX =
  VkCmdProcessCommandsInfoNVX{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkObjectTable :: VkObjectTableNVX 
                             , vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX 
                             , vkIndirectCommandsTokenCount :: Word32 
                             , vkPIndirectCommandsTokens :: Ptr VkIndirectCommandsTokenNVX 
                             , vkMaxSequencesCount :: Word32 
                             , vkTargetCommandBuffer :: VkCommandBuffer 
                             , vkSequencesCountBuffer :: VkBuffer 
                             , vkSequencesCountOffset :: VkDeviceSize 
                             , vkSequencesIndexBuffer :: VkBuffer 
                             , vkSequencesIndexOffset :: VkDeviceSize 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkCmdProcessCommandsInfoNVX where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkCmdProcessCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
                                         <*> peek (ptr `plusPtr` 40)
                                         <*> peek (ptr `plusPtr` 48)
                                         <*> peek (ptr `plusPtr` 56)
                                         <*> peek (ptr `plusPtr` 64)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkIndirectCommandsTokenCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPIndirectCommandsTokens (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxSequencesCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkTargetCommandBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkSequencesCountBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 72) (vkSequencesCountOffset (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 80) (vkSequencesIndexBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 88) (vkSequencesIndexOffset (poked :: VkCmdProcessCommandsInfoNVX))
newtype VkObjectTableNVX = VkObjectTableNVX Word64
  deriving (Eq, Ord, Storable, Show)
-- ** VkObjectEntryUsageFlagsNVX
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkObjectEntryUsageFlagBitsNVX
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX

instance Show VkObjectEntryUsageFlagBitsNVX where
  showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
  showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
  
  showsPrec p (VkObjectEntryUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkObjectEntryUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkObjectEntryUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX", pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX)
                             , ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX", pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkObjectEntryUsageFlagBitsNVX v)
                        )
                    )

pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x1

pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x2

data VkObjectTableEntryNVX =
  VkObjectTableEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                       , vkFlags :: VkObjectEntryUsageFlagsNVX 
                       }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTableEntryNVX where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkObjectTableEntryNVX <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableEntryNVX))

data VkDeviceGeneratedCommandsLimitsNVX =
  VkDeviceGeneratedCommandsLimitsNVX{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkMaxIndirectCommandsLayoutTokenCount :: Word32 
                                    , vkMaxObjectEntryCounts :: Word32 
                                    , vkMinSequenceCountBufferOffsetAlignment :: Word32 
                                    , vkMinSequenceIndexBufferOffsetAlignment :: Word32 
                                    , vkMinCommandsTokenBufferOffsetAlignment :: Word32 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGeneratedCommandsLimitsNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsLimitsNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 16) (vkMaxIndirectCommandsLayoutTokenCount (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 20) (vkMaxObjectEntryCounts (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 24) (vkMinSequenceCountBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 28) (vkMinSequenceIndexBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 32) (vkMinCommandsTokenBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))

data VkObjectTableCreateInfoNVX =
  VkObjectTableCreateInfoNVX{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkObjectCount :: Word32 
                            , vkPObjectEntryTypes :: Ptr VkObjectEntryTypeNVX 
                            , vkPObjectEntryCounts :: Ptr Word32 
                            , vkPObjectEntryUsageFlags :: Ptr VkObjectEntryUsageFlagsNVX 
                            , vkMaxUniformBuffersPerDescriptor :: Word32 
                            , vkMaxStorageBuffersPerDescriptor :: Word32 
                            , vkMaxStorageImagesPerDescriptor :: Word32 
                            , vkMaxSampledImagesPerDescriptor :: Word32 
                            , vkMaxPipelineLayouts :: Word32 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTableCreateInfoNVX where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkObjectTableCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
                                        <*> peek (ptr `plusPtr` 48)
                                        <*> peek (ptr `plusPtr` 52)
                                        <*> peek (ptr `plusPtr` 56)
                                        <*> peek (ptr `plusPtr` 60)
                                        <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectCount (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkPObjectEntryTypes (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPObjectEntryCounts (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPObjectEntryUsageFlags (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxUniformBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 52) (vkMaxStorageBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkMaxStorageImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 60) (vkMaxSampledImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkMaxPipelineLayouts (poked :: VkObjectTableCreateInfoNVX))

data VkIndirectCommandsLayoutTokenNVX =
  VkIndirectCommandsLayoutTokenNVX{ vkTokenType :: VkIndirectCommandsTokenTypeNVX 
                                  , vkBindingUnit :: Word32 
                                  , vkDynamicCount :: Word32 
                                  , vkDivisor :: Word32 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkIndirectCommandsLayoutTokenNVX where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkIndirectCommandsLayoutTokenNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 4) (vkBindingUnit (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkDynamicCount (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 12) (vkDivisor (poked :: VkIndirectCommandsLayoutTokenNVX))

data VkIndirectCommandsLayoutCreateInfoNVX =
  VkIndirectCommandsLayoutCreateInfoNVX{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkPipelineBindPoint :: VkPipelineBindPoint 
                                       , vkFlags :: VkIndirectCommandsLayoutUsageFlagsNVX 
                                       , vkTokenCount :: Word32 
                                       , vkPTokens :: Ptr VkIndirectCommandsLayoutTokenNVX 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkIndirectCommandsLayoutCreateInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsLayoutCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkPipelineBindPoint (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 20) (vkFlags (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkTokenCount (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPTokens (poked :: VkIndirectCommandsLayoutCreateInfoNVX))

data VkIndirectCommandsTokenNVX =
  VkIndirectCommandsTokenNVX{ vkTokenType :: VkIndirectCommandsTokenTypeNVX 
                            , vkBuffer :: VkBuffer 
                            , vkOffset :: VkDeviceSize 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkIndirectCommandsTokenNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsTokenNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 16) (vkOffset (poked :: VkIndirectCommandsTokenNVX))
-- ** VkIndirectCommandsLayoutUsageFlagsNVX
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkIndirectCommandsLayoutUsageFlagBitsNVX
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX

instance Show VkIndirectCommandsLayoutUsageFlagBitsNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
  
  showsPrec p (VkIndirectCommandsLayoutUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsLayoutUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsLayoutUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsLayoutUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsLayoutUsageFlagBitsNVX v)
                        )
                    )

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x1

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x2

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x4

pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x8
-- ** VkObjectEntryTypeNVX
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkObjectEntryTypeNVX where
  showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
  showsPrec p (VkObjectEntryTypeNVX x) = showParen (p >= 11) (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX", pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX", pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX", pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX", pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX", pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryTypeNVX")
                        v <- step readPrec
                        pure (VkObjectEntryTypeNVX v)
                        )
                    )

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4
-- ** vkDestroyIndirectCommandsLayoutNVX
foreign import ccall "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX ::
  VkDevice ->
  VkIndirectCommandsLayoutNVX -> Ptr VkAllocationCallbacks -> IO ()

data VkDeviceGeneratedCommandsFeaturesNVX =
  VkDeviceGeneratedCommandsFeaturesNVX{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkComputeBindingPointSupport :: VkBool32 
                                      }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsFeaturesNVX <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 16) (vkComputeBindingPointSupport (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
-- ** vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
foreign import ccall "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
  VkPhysicalDevice ->
  Ptr VkDeviceGeneratedCommandsFeaturesNVX ->
    Ptr VkDeviceGeneratedCommandsLimitsNVX -> IO ()

data VkObjectTableDescriptorSetEntryNVX =
  VkObjectTableDescriptorSetEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                                    , vkFlags :: VkObjectEntryUsageFlagsNVX 
                                    , vkPipelineLayout :: VkPipelineLayout 
                                    , vkDescriptorSet :: VkDescriptorSet 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTableDescriptorSetEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableDescriptorSetEntryNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 4)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSet (poked :: VkObjectTableDescriptorSetEntryNVX))

data VkObjectTableVertexBufferEntryNVX =
  VkObjectTableVertexBufferEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                                   , vkFlags :: VkObjectEntryUsageFlagsNVX 
                                   , vkBuffer :: VkBuffer 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTableVertexBufferEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTableVertexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableVertexBufferEntryNVX))
-- ** vkCreateObjectTableNVX
foreign import ccall "vkCreateObjectTableNVX" vkCreateObjectTableNVX ::
  VkDevice ->
  Ptr VkObjectTableCreateInfoNVX ->
    Ptr VkAllocationCallbacks -> Ptr VkObjectTableNVX -> IO VkResult
-- ** VkIndirectCommandsTokenTypeNVX
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkIndirectCommandsTokenTypeNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
  showsPrec p (VkIndirectCommandsTokenTypeNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsTokenTypeNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsTokenTypeNVX v)
                        )
                    )

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = VkIndirectCommandsTokenTypeNVX 0

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = VkIndirectCommandsTokenTypeNVX 1

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 2

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 3

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = VkIndirectCommandsTokenTypeNVX 4

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = VkIndirectCommandsTokenTypeNVX 5

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = VkIndirectCommandsTokenTypeNVX 6

pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = VkIndirectCommandsTokenTypeNVX 7
-- ** vkRegisterObjectsNVX
foreign import ccall "vkRegisterObjectsNVX" vkRegisterObjectsNVX ::
  VkDevice ->
  VkObjectTableNVX ->
    Word32 ->
      Ptr (Ptr VkObjectTableEntryNVX) -> Ptr Word32 -> IO VkResult

data VkObjectTablePipelineEntryNVX =
  VkObjectTablePipelineEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                               , vkFlags :: VkObjectEntryUsageFlagsNVX 
                               , vkPipeline :: VkPipeline 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTablePipelineEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTablePipelineEntryNVX <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipeline (poked :: VkObjectTablePipelineEntryNVX))

data VkObjectTableIndexBufferEntryNVX =
  VkObjectTableIndexBufferEntryNVX{ vkType :: VkObjectEntryTypeNVX 
                                  , vkFlags :: VkObjectEntryUsageFlagsNVX 
                                  , vkBuffer :: VkBuffer 
                                  , vkIndexType :: VkIndexType 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkObjectTableIndexBufferEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableIndexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkIndexType (poked :: VkObjectTableIndexBufferEntryNVX))
