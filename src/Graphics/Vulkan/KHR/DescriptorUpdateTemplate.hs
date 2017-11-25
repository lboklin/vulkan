{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.DescriptorUpdateTemplate where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( VkPipelineBindPoint(..)
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
import Graphics.Vulkan.DescriptorSet( VkDescriptorType(..)
                                    , VkDescriptorSet(..)
                                    , VkDescriptorSetLayout(..)
                                    )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Graphics.Vulkan.EXT.DebugReport( VkDebugReportObjectTypeEXT(..)
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
import Graphics.Vulkan.PipelineLayout( VkPipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetDeviceProcAddr
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize
                      , CSize(..)
                      )

newtype VkDescriptorUpdateTemplateKHR = VkDescriptorUpdateTemplateKHR Word64
  deriving (Eq, Ord, Storable, Show)
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = VkDebugReportObjectTypeEXT 1000085000

data VkDescriptorUpdateTemplateCreateInfoKHR =
  VkDescriptorUpdateTemplateCreateInfoKHR{ vkSType :: VkStructureType 
                                         , vkPNext :: Ptr Void 
                                         , vkFlags :: VkDescriptorUpdateTemplateCreateFlagsKHR 
                                         , vkDescriptorUpdateEntryCount :: Word32 
                                         , vkPDescriptorUpdateEntries :: Ptr VkDescriptorUpdateTemplateEntryKHR 
                                         , vkTemplateType :: VkDescriptorUpdateTemplateTypeKHR 
                                         , vkDescriptorSetLayout :: VkDescriptorSetLayout 
                                         , vkPipelineBindPoint :: VkPipelineBindPoint 
                                         , vkPipelineLayout :: VkPipelineLayout 
                                         , vkSet :: Word32 
                                         }
  deriving (Eq, Ord, Show)
instance Storable VkDescriptorUpdateTemplateCreateInfoKHR where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkDescriptorUpdateTemplateCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 20)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 40)
                                                     <*> peek (ptr `plusPtr` 48)
                                                     <*> peek (ptr `plusPtr` 56)
                                                     <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkDescriptorUpdateEntryCount (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPDescriptorUpdateEntries (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkTemplateType (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkDescriptorSetLayout (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPipelineBindPoint (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPipelineLayout (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkSet (poked :: VkDescriptorUpdateTemplateCreateInfoKHR))
-- ** vkCmdPushDescriptorSetWithTemplateKHR
foreign import ccall "dynamic" mkvkCmdPushDescriptorSetWithTemplateKHR :: FunPtr (VkCommandBuffer ->
  VkDescriptorUpdateTemplateKHR ->
    VkPipelineLayout -> Word32 -> Ptr Void -> IO ()) -> (VkCommandBuffer ->
  VkDescriptorUpdateTemplateKHR ->
    VkPipelineLayout -> Word32 -> Ptr Void -> IO ())
vkCmdPushDescriptorSetWithTemplateKHR :: VkInstance ->
  VkCommandBuffer ->
    VkDescriptorUpdateTemplateKHR ->
      VkPipelineLayout -> Word32 -> Ptr Void -> IO ()
vkCmdPushDescriptorSetWithTemplateKHR i = (mkvkCmdPushDescriptorSetWithTemplateKHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdPushDescriptorSetWithTemplateKHR" $ vkGetInstanceProcAddr i

data VkDescriptorUpdateTemplateEntryKHR =
  VkDescriptorUpdateTemplateEntryKHR{ vkDstBinding :: Word32 
                                    , vkDstArrayElement :: Word32 
                                    , vkDescriptorCount :: Word32 
                                    , vkDescriptorType :: VkDescriptorType 
                                    , vkOffset :: CSize 
                                    , vkStride :: CSize 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkDescriptorUpdateTemplateEntryKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorUpdateTemplateEntryKHR <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 4)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 12)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDstBinding (poked :: VkDescriptorUpdateTemplateEntryKHR))
                *> poke (ptr `plusPtr` 4) (vkDstArrayElement (poked :: VkDescriptorUpdateTemplateEntryKHR))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorUpdateTemplateEntryKHR))
                *> poke (ptr `plusPtr` 12) (vkDescriptorType (poked :: VkDescriptorUpdateTemplateEntryKHR))
                *> poke (ptr `plusPtr` 16) (vkOffset (poked :: VkDescriptorUpdateTemplateEntryKHR))
                *> poke (ptr `plusPtr` 24) (vkStride (poked :: VkDescriptorUpdateTemplateEntryKHR))
-- ** VkDescriptorUpdateTemplateTypeKHR
newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkDescriptorUpdateTemplateTypeKHR where
  showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = showString "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
  showsPrec _ VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = showString "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
  showsPrec p (VkDescriptorUpdateTemplateTypeKHR x) = showParen (p >= 11) (showString "VkDescriptorUpdateTemplateTypeKHR " . showsPrec 11 x)

instance Read VkDescriptorUpdateTemplateTypeKHR where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR", pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR)
                             , ("VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR", pure VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorUpdateTemplateTypeKHR")
                        v <- step readPrec
                        pure (VkDescriptorUpdateTemplateTypeKHR v)
                        )
                    )
-- | Create descriptor update template for descriptor set updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = VkDescriptorUpdateTemplateTypeKHR 0
-- | Create descriptor update template for pushed descriptor updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = VkDescriptorUpdateTemplateTypeKHR 1
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME =  "VK_KHR_descriptor_update_template"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = VkStructureType 1000085000
-- ** vkCreateDescriptorUpdateTemplateKHR
foreign import ccall "dynamic" mkvkCreateDescriptorUpdateTemplateKHR :: FunPtr (VkDevice ->
  Ptr VkDescriptorUpdateTemplateCreateInfoKHR ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDescriptorUpdateTemplateKHR -> IO VkResult) -> (VkDevice ->
  Ptr VkDescriptorUpdateTemplateCreateInfoKHR ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDescriptorUpdateTemplateKHR -> IO VkResult)
vkCreateDescriptorUpdateTemplateKHR :: VkDevice ->
  Ptr VkDescriptorUpdateTemplateCreateInfoKHR ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDescriptorUpdateTemplateKHR -> IO VkResult
vkCreateDescriptorUpdateTemplateKHR d = (mkvkCreateDescriptorUpdateTemplateKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkCreateDescriptorUpdateTemplateKHR" $ vkGetDeviceProcAddr d
-- ** vkDestroyDescriptorUpdateTemplateKHR
foreign import ccall "dynamic" mkvkDestroyDescriptorUpdateTemplateKHR :: FunPtr (VkDevice ->
  VkDescriptorUpdateTemplateKHR -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice ->
  VkDescriptorUpdateTemplateKHR -> Ptr VkAllocationCallbacks -> IO ())
vkDestroyDescriptorUpdateTemplateKHR :: VkDevice ->
  VkDescriptorUpdateTemplateKHR -> Ptr VkAllocationCallbacks -> IO ()
vkDestroyDescriptorUpdateTemplateKHR d = (mkvkDestroyDescriptorUpdateTemplateKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDestroyDescriptorUpdateTemplateKHR" $ vkGetDeviceProcAddr d
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION =  0x1
-- ** vkUpdateDescriptorSetWithTemplateKHR
foreign import ccall "dynamic" mkvkUpdateDescriptorSetWithTemplateKHR :: FunPtr (VkDevice ->
  VkDescriptorSet ->
    VkDescriptorUpdateTemplateKHR -> Ptr Void -> IO ()) -> (VkDevice ->
  VkDescriptorSet ->
    VkDescriptorUpdateTemplateKHR -> Ptr Void -> IO ())
vkUpdateDescriptorSetWithTemplateKHR :: VkDevice ->
  VkDescriptorSet ->
    VkDescriptorUpdateTemplateKHR -> Ptr Void -> IO ()
vkUpdateDescriptorSetWithTemplateKHR d = (mkvkUpdateDescriptorSetWithTemplateKHR $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkUpdateDescriptorSetWithTemplateKHR" $ vkGetDeviceProcAddr d
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = VkObjectType 1000085000
-- ** VkDescriptorUpdateTemplateCreateFlagsKHR-- | Opaque flag
newtype VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
