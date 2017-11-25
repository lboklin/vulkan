{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.EXT.SampleLocations where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
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
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              , VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( VkImageCreateFlagBits(..)
                            )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      )


data VkMultisamplePropertiesEXT =
  VkMultisamplePropertiesEXT{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkMaxSampleLocationGridSize :: VkExtent2D 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkMultisamplePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMultisamplePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxSampleLocationGridSize (poked :: VkMultisamplePropertiesEXT))
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VkImageCreateFlagBits 0x1000

data VkRenderPassSampleLocationsBeginInfoEXT =
  VkRenderPassSampleLocationsBeginInfoEXT{ vkSType :: VkStructureType 
                                         , vkPNext :: Ptr Void 
                                         , vkAttachmentInitialSampleLocationsCount :: Word32 
                                         , vkPAttachmentInitialSampleLocations :: Ptr VkAttachmentSampleLocationsEXT 
                                         , vkPostSubpassSampleLocationsCount :: Word32 
                                         , vkPPostSubpassSampleLocations :: Ptr VkSubpassSampleLocationsEXT 
                                         }
  deriving (Eq, Ord, Show)
instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkRenderPassSampleLocationsBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkAttachmentInitialSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPAttachmentInitialSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPostSubpassSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPPostSubpassSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT = VkStructureType 1000143003
-- ** vkGetPhysicalDeviceMultisamplePropertiesEXT
foreign import ccall "dynamic" mkvkGetPhysicalDeviceMultisamplePropertiesEXT :: FunPtr (VkPhysicalDevice ->
  VkSampleCountFlagBits -> Ptr VkMultisamplePropertiesEXT -> IO ()) -> (VkPhysicalDevice ->
  VkSampleCountFlagBits -> Ptr VkMultisamplePropertiesEXT -> IO ())
vkGetPhysicalDeviceMultisamplePropertiesEXT :: VkInstance ->
  VkPhysicalDevice ->
    VkSampleCountFlagBits -> Ptr VkMultisamplePropertiesEXT -> IO ()
vkGetPhysicalDeviceMultisamplePropertiesEXT i = (mkvkGetPhysicalDeviceMultisamplePropertiesEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceMultisamplePropertiesEXT" $ vkGetInstanceProcAddr i

data VkSampleLocationsInfoEXT =
  VkSampleLocationsInfoEXT{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkSampleLocationsPerPixel :: VkSampleCountFlagBits 
                          , vkSampleLocationGridSize :: VkExtent2D 
                          , vkSampleLocationsCount :: Word32 
                          , vkPSampleLocations :: Ptr VkSampleLocationEXT 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkSampleLocationsInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSampleLocationsInfoEXT <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsPerPixel (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkSampleLocationGridSize (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationsCount (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPSampleLocations (poked :: VkSampleLocationsInfoEXT))

data VkPhysicalDeviceSampleLocationsPropertiesEXT =
  VkPhysicalDeviceSampleLocationsPropertiesEXT{ vkSType :: VkStructureType 
                                              , vkPNext :: Ptr Void 
                                              , vkSampleLocationSampleCounts :: VkSampleCountFlags 
                                              , vkMaxSampleLocationGridSize :: VkExtent2D 
                                              , vkSampleLocationCoordinateRange :: Vector 2 CFloat 
                                              , vkSampleLocationSubPixelBits :: Word32 
                                              , vkVariableSampleLocations :: VkBool32 
                                              }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceSampleLocationsPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSampleLocationsPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 28)
                                                          <*> peek (ptr `plusPtr` 36)
                                                          <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationSampleCounts (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxSampleLocationGridSize (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationCoordinateRange (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkSampleLocationSubPixelBits (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkVariableSampleLocations (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT = VkStructureType 1000143000

data VkSubpassSampleLocationsEXT =
  VkSubpassSampleLocationsEXT{ vkSubpassIndex :: Word32 
                             , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkSubpassSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSubpassSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpassIndex (poked :: VkSubpassSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkSubpassSampleLocationsEXT))

data VkAttachmentSampleLocationsEXT =
  VkAttachmentSampleLocationsEXT{ vkAttachmentIndex :: Word32 
                                , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkAttachmentSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAttachmentSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachmentIndex (poked :: VkAttachmentSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkAttachmentSampleLocationsEXT))
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION =  0x1

data VkPipelineSampleLocationsStateCreateInfoEXT =
  VkPipelineSampleLocationsStateCreateInfoEXT{ vkSType :: VkStructureType 
                                             , vkPNext :: Ptr Void 
                                             , vkSampleLocationsEnable :: VkBool32 
                                             , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT 
                                             }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPipelineSampleLocationsStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsEnable (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkSampleLocationsInfo (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT = VkStructureType 1000143002
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT = VkStructureType 1000143001
-- ** vkCmdSetSampleLocationsEXT
foreign import ccall "dynamic" mkvkCmdSetSampleLocationsEXT :: FunPtr (VkCommandBuffer -> Ptr VkSampleLocationsInfoEXT -> IO ()) -> (VkCommandBuffer -> Ptr VkSampleLocationsInfoEXT -> IO ())
vkCmdSetSampleLocationsEXT :: VkInstance ->
  VkCommandBuffer -> Ptr VkSampleLocationsInfoEXT -> IO ()
vkCmdSetSampleLocationsEXT i = (mkvkCmdSetSampleLocationsEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdSetSampleLocationsEXT" $ vkGetInstanceProcAddr i
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = VkDynamicState 1000143000
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME =  "VK_EXT_sample_locations"

data VkSampleLocationEXT =
  VkSampleLocationEXT{ vkX :: CFloat 
                     , vkY :: CFloat 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkSampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSampleLocationEXT <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkSampleLocationEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkSampleLocationEXT))
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT = VkStructureType 1000143004
