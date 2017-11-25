{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.SamplerYcbcrConversion where

import Graphics.Vulkan.Device( VkDevice(..)
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
import Graphics.Vulkan.Memory( VkSystemAllocationScope(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkFreeFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkInternalAllocationType(..)
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Sampler( VkFilter(..)
                              )
import Graphics.Vulkan.Image( VkImageAspectFlagBits(..)
                            )
import Graphics.Vulkan.ImageView( VkComponentSwizzle(..)
                                , VkComponentMapping(..)
                                )
import Graphics.Vulkan.Core( VkFormat(..)
                           , VkBool32(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

newtype VkSamplerYcbcrConversionKHR = VkSamplerYcbcrConversionKHR Word64
  deriving (Eq, Ord, Storable, Show)

data VkSamplerYcbcrConversionInfoKHR =
  VkSamplerYcbcrConversionInfoKHR{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkConversion :: VkSamplerYcbcrConversionKHR 
                                 }
  deriving (Eq, Ord, Show)
instance Storable VkSamplerYcbcrConversionInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionInfoKHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkConversion (poked :: VkSamplerYcbcrConversionInfoKHR))
-- ** VkSamplerYcbcrRangeKHR
newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerYcbcrRangeKHR where
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR = showString "VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR"
  showsPrec _ VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR = showString "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR"
  showsPrec p (VkSamplerYcbcrRangeKHR x) = showParen (p >= 11) (showString "VkSamplerYcbcrRangeKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrRangeKHR where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR", pure VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR)
                             , ("VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR", pure VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrRangeKHR")
                        v <- step readPrec
                        pure (VkSamplerYcbcrRangeKHR v)
                        )
                    )
-- | Luma 0..1 maps to 0..255, chroma -0.5..0.5 to 1..255 (clamped)
pattern VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR = VkSamplerYcbcrRangeKHR 0
-- | Luma 0..1 maps to 16..235, chroma -0.5..0.5 to 16..240
pattern VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR = VkSamplerYcbcrRangeKHR 1
-- ** vkCreateSamplerYcbcrConversionKHR
foreign import ccall "vkCreateSamplerYcbcrConversionKHR" vkCreateSamplerYcbcrConversionKHR ::
  VkDevice ->
  Ptr VkSamplerYcbcrConversionCreateInfoKHR ->
    Ptr VkAllocationCallbacks ->
      Ptr VkSamplerYcbcrConversionKHR -> IO VkResult

data VkSamplerYcbcrConversionImageFormatPropertiesKHR =
  VkSamplerYcbcrConversionImageFormatPropertiesKHR{ vkSType :: VkStructureType 
                                                  , vkPNext :: Ptr Void 
                                                  , vkCombinedImageSamplerDescriptorCount :: Word32 
                                                  }
  deriving (Eq, Ord, Show)
instance Storable VkSamplerYcbcrConversionImageFormatPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionImageFormatPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                              <*> peek (ptr `plusPtr` 8)
                                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionImageFormatPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionImageFormatPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkCombinedImageSamplerDescriptorCount (poked :: VkSamplerYcbcrConversionImageFormatPropertiesKHR))
-- ** VkChromaLocationKHR
newtype VkChromaLocationKHR = VkChromaLocationKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkChromaLocationKHR where
  showsPrec _ VK_CHROMA_LOCATION_COSITED_EVEN_KHR = showString "VK_CHROMA_LOCATION_COSITED_EVEN_KHR"
  showsPrec _ VK_CHROMA_LOCATION_MIDPOINT_KHR = showString "VK_CHROMA_LOCATION_MIDPOINT_KHR"
  showsPrec p (VkChromaLocationKHR x) = showParen (p >= 11) (showString "VkChromaLocationKHR " . showsPrec 11 x)

instance Read VkChromaLocationKHR where
  readPrec = parens ( choose [ ("VK_CHROMA_LOCATION_COSITED_EVEN_KHR", pure VK_CHROMA_LOCATION_COSITED_EVEN_KHR)
                             , ("VK_CHROMA_LOCATION_MIDPOINT_KHR", pure VK_CHROMA_LOCATION_MIDPOINT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkChromaLocationKHR")
                        v <- step readPrec
                        pure (VkChromaLocationKHR v)
                        )
                    )

pattern VK_CHROMA_LOCATION_COSITED_EVEN_KHR = VkChromaLocationKHR 0

pattern VK_CHROMA_LOCATION_MIDPOINT_KHR = VkChromaLocationKHR 1

data VkSamplerYcbcrConversionCreateInfoKHR =
  VkSamplerYcbcrConversionCreateInfoKHR{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkFormat :: VkFormat 
                                       , vkYcbcrModel :: VkSamplerYcbcrModelConversionKHR 
                                       , vkYcbcrRange :: VkSamplerYcbcrRangeKHR 
                                       , vkComponents :: VkComponentMapping 
                                       , vkXChromaOffset :: VkChromaLocationKHR 
                                       , vkYChromaOffset :: VkChromaLocationKHR 
                                       , vkChromaFilter :: VkFilter 
                                       , vkForceExplicitReconstruction :: VkBool32 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkSamplerYcbcrConversionCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkSamplerYcbcrConversionCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 28)
                                                   <*> peek (ptr `plusPtr` 44)
                                                   <*> peek (ptr `plusPtr` 48)
                                                   <*> peek (ptr `plusPtr` 52)
                                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkYcbcrModel (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkYcbcrRange (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkComponents (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkXChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkYChromaOffset (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkChromaFilter (poked :: VkSamplerYcbcrConversionCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkForceExplicitReconstruction (poked :: VkSamplerYcbcrConversionCreateInfoKHR))

data VkBindImagePlaneMemoryInfoKHR =
  VkBindImagePlaneMemoryInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkPlaneAspect :: VkImageAspectFlagBits 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkBindImagePlaneMemoryInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBindImagePlaneMemoryInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImagePlaneMemoryInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImagePlaneMemoryInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkBindImagePlaneMemoryInfoKHR))
-- ** vkDestroySamplerYcbcrConversionKHR
foreign import ccall "vkDestroySamplerYcbcrConversionKHR" vkDestroySamplerYcbcrConversionKHR ::
  VkDevice ->
  VkSamplerYcbcrConversionKHR -> Ptr VkAllocationCallbacks -> IO ()

data VkImagePlaneMemoryRequirementsInfoKHR =
  VkImagePlaneMemoryRequirementsInfoKHR{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkPlaneAspect :: VkImageAspectFlagBits 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkImagePlaneMemoryRequirementsInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImagePlaneMemoryRequirementsInfoKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImagePlaneMemoryRequirementsInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImagePlaneMemoryRequirementsInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPlaneAspect (poked :: VkImagePlaneMemoryRequirementsInfoKHR))
-- ** VkSamplerYcbcrModelConversionKHR
newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR Int32
  deriving (Eq, Ord, Storable)

instance Show VkSamplerYcbcrModelConversionKHR where
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR"
  showsPrec _ VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR = showString "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR"
  showsPrec p (VkSamplerYcbcrModelConversionKHR x) = showParen (p >= 11) (showString "VkSamplerYcbcrModelConversionKHR " . showsPrec 11 x)

instance Read VkSamplerYcbcrModelConversionKHR where
  readPrec = parens ( choose [ ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR)
                             , ("VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR", pure VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSamplerYcbcrModelConversionKHR")
                        v <- step readPrec
                        pure (VkSamplerYcbcrModelConversionKHR v)
                        )
                    )

pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR = VkSamplerYcbcrModelConversionKHR 0
-- | just range expansion
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR = VkSamplerYcbcrModelConversionKHR 1
-- | aka HD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR = VkSamplerYcbcrModelConversionKHR 2
-- | aka SD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR = VkSamplerYcbcrModelConversionKHR 3
-- | aka UHD YUV
pattern VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR = VkSamplerYcbcrModelConversionKHR 4

data VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR =
  VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR{ vkSType :: VkStructureType 
                                                   , vkPNext :: Ptr Void 
                                                   , vkSamplerYcbcrConversion :: VkBool32 
                                                   }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkSamplerYcbcrConversion (poked :: VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR))
