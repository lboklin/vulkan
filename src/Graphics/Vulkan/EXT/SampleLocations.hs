{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.EXT.SampleLocations where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
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
-- ** vkGetPhysicalDeviceMultisamplePropertiesEXT
foreign import ccall "vkGetPhysicalDeviceMultisamplePropertiesEXT" vkGetPhysicalDeviceMultisamplePropertiesEXT ::
  VkPhysicalDevice ->
  VkSampleCountFlagBits -> Ptr VkMultisamplePropertiesEXT -> IO ()

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
-- ** vkCmdSetSampleLocationsEXT
foreign import ccall "vkCmdSetSampleLocationsEXT" vkCmdSetSampleLocationsEXT ::
  VkCommandBuffer -> Ptr VkSampleLocationsInfoEXT -> IO ()

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
