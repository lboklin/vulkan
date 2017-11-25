{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Maintenance2 where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
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
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageAspectFlags(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )

-- ** VkTessellationDomainOriginKHR
newtype VkTessellationDomainOriginKHR = VkTessellationDomainOriginKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkTessellationDomainOriginKHR where
  showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR = showString "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR"
  showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR = showString "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR"
  showsPrec p (VkTessellationDomainOriginKHR x) = showParen (p >= 11) (showString "VkTessellationDomainOriginKHR " . showsPrec 11 x)

instance Read VkTessellationDomainOriginKHR where
  readPrec = parens ( choose [ ("VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR", pure VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR)
                             , ("VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR", pure VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkTessellationDomainOriginKHR")
                        v <- step readPrec
                        pure (VkTessellationDomainOriginKHR v)
                        )
                    )

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR = VkTessellationDomainOriginKHR 0

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR = VkTessellationDomainOriginKHR 1

data VkPipelineTessellationDomainOriginStateCreateInfoKHR =
  VkPipelineTessellationDomainOriginStateCreateInfoKHR{ vkSType :: VkStructureType 
                                                      , vkPNext :: Ptr Void 
                                                      , vkDomainOrigin :: VkTessellationDomainOriginKHR 
                                                      }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineTessellationDomainOriginStateCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineTessellationDomainOriginStateCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                                  <*> peek (ptr `plusPtr` 8)
                                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineTessellationDomainOriginStateCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineTessellationDomainOriginStateCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkDomainOrigin (poked :: VkPipelineTessellationDomainOriginStateCreateInfoKHR))

data VkInputAttachmentAspectReferenceKHR =
  VkInputAttachmentAspectReferenceKHR{ vkSubpass :: Word32 
                                     , vkInputAttachmentIndex :: Word32 
                                     , vkAspectMask :: VkImageAspectFlags 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkInputAttachmentAspectReferenceKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkInputAttachmentAspectReferenceKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 4)
                                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpass (poked :: VkInputAttachmentAspectReferenceKHR))
                *> poke (ptr `plusPtr` 4) (vkInputAttachmentIndex (poked :: VkInputAttachmentAspectReferenceKHR))
                *> poke (ptr `plusPtr` 8) (vkAspectMask (poked :: VkInputAttachmentAspectReferenceKHR))
-- ** VkPointClippingBehaviorKHR
newtype VkPointClippingBehaviorKHR = VkPointClippingBehaviorKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkPointClippingBehaviorKHR where
  showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR = showString "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR"
  showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR = showString "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR"
  showsPrec p (VkPointClippingBehaviorKHR x) = showParen (p >= 11) (showString "VkPointClippingBehaviorKHR " . showsPrec 11 x)

instance Read VkPointClippingBehaviorKHR where
  readPrec = parens ( choose [ ("VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR", pure VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR)
                             , ("VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR", pure VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPointClippingBehaviorKHR")
                        v <- step readPrec
                        pure (VkPointClippingBehaviorKHR v)
                        )
                    )

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR = VkPointClippingBehaviorKHR 0

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR = VkPointClippingBehaviorKHR 1

data VkPhysicalDevicePointClippingPropertiesKHR =
  VkPhysicalDevicePointClippingPropertiesKHR{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkPointClippingBehavior :: VkPointClippingBehaviorKHR 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDevicePointClippingPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePointClippingPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePointClippingPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePointClippingPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPointClippingBehavior (poked :: VkPhysicalDevicePointClippingPropertiesKHR))

data VkRenderPassInputAttachmentAspectCreateInfoKHR =
  VkRenderPassInputAttachmentAspectCreateInfoKHR{ vkSType :: VkStructureType 
                                                , vkPNext :: Ptr Void 
                                                , vkAspectReferenceCount :: Word32 
                                                , vkPAspectReferences :: Ptr VkInputAttachmentAspectReferenceKHR 
                                                }
  deriving (Eq, Ord, Show)
instance Storable VkRenderPassInputAttachmentAspectCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkRenderPassInputAttachmentAspectCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassInputAttachmentAspectCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassInputAttachmentAspectCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkAspectReferenceCount (poked :: VkRenderPassInputAttachmentAspectCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPAspectReferences (poked :: VkRenderPassInputAttachmentAspectCreateInfoKHR))

data VkImageViewUsageCreateInfoKHR =
  VkImageViewUsageCreateInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkUsage :: VkImageUsageFlags 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkImageViewUsageCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageViewUsageCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewUsageCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewUsageCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkUsage (poked :: VkImageViewUsageCreateInfoKHR))
