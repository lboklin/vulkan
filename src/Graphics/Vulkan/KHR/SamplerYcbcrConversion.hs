{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.Sampler( VkFilter(..)
                              )
import Graphics.Vulkan.Image( VkImageCreateFlagBits(..)
                            , VkImageAspectFlagBits(..)
                            )
import Graphics.Vulkan.ImageView( VkComponentSwizzle(..)
                                , VkComponentMapping(..)
                                )
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( VkFormatFeatureFlagBits(..)
                                           )
import Graphics.Vulkan.Core( VkFormat(..)
                           , VkBool32(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR = VkStructureType 1000156005
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR = VkStructureType 1000156001
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
pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR = VkFormat 1000156006
pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME =  "VK_KHR_sampler_ycbcr_conversion"
pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR = VkFormat 1000156028
pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR = VkFormat 1000156027
pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR = VkFormatFeatureFlagBits 0x400000
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
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT = VkDebugReportObjectTypeEXT 1000156000
pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR = VkImageCreateFlagBits 0x200
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR = VkFormat 1000156015
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR = VkStructureType 1000156004
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR = VkFormat 1000156025
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR = VkFormat 1000156022
pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR = VkImageAspectFlagBits 0x40
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR = VkFormat 1000156012
pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR = VkFormat 1000156032
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
pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR = VkFormat 1000156005
pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR = VkStructureType 1000156000

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
pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR = VkFormat 1000156029
pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR = VkImageAspectFlagBits 0x10
pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR = VkFormat 1000156003

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
pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR = VkImageAspectFlagBits 0x20
pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR = VkFormat 1000156019
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR = VkFormatFeatureFlagBits 0x800000
pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR = VkFormat 1000156009
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
pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR = VkFormat 1000156013
pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR = VkFormat 1000156023
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR = VkStructureType 1000156002
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR = VkFormatFeatureFlagBits 0x200000
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR = VkFormat 1000156016
pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR = VkFormat 1000156014
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR = VkFormatFeatureFlagBits 0x20000
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR = VkFormat 1000156026
pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR = VkFormat 1000156024
pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR = VkFormat 1000156002
pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR = VkFormat 1000156010
pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR = VkFormat 1000156011
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR = VkFormatFeatureFlagBits 0x100000
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
pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR = VkFormat 1000156021
pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR = VkFormat 1000156030
pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR = VkFormat 1000156020
pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR = VkFormat 1000156033
pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR = VkStructureType 1000156003
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR = VkFormatFeatureFlagBits 0x80000
pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR = VkFormat 1000156004
pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR = VkFormat 1000156031
pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR = VkFormat 1000156000
pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR = VkFormat 1000156001
pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR = VkFormat 1000156008
pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR = VkFormat 1000156017
pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION =  0x1

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
pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR = VkFormat 1000156007
pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR = VkFormat 1000156018
pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR = VkObjectType 1000156000
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR = VkFormatFeatureFlagBits 0x40000
