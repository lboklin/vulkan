{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.DisplayControl where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
                                    )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
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
import Data.Int( Int32
               )
import Graphics.Vulkan.KHR.Display( VkDisplayKHR(..)
                                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
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
import Graphics.Vulkan.DeviceInitialization( vkGetDeviceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Graphics.Vulkan.EXT.DisplaySurfaceCounter( VkSurfaceCounterFlagBitsEXT(..)
                                                , VkSurfaceCounterFlagsEXT(..)
                                                )
import Foreign.C.Types( CSize(..)
                      )


data VkSwapchainCounterCreateInfoEXT =
  VkSwapchainCounterCreateInfoEXT{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkSurfaceCounters :: VkSurfaceCounterFlagsEXT 
                                 }
  deriving (Eq, Ord, Show)
instance Storable VkSwapchainCounterCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSwapchainCounterCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCounterCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSurfaceCounters (poked :: VkSwapchainCounterCreateInfoEXT))

data VkDisplayEventInfoEXT =
  VkDisplayEventInfoEXT{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkDisplayEvent :: VkDisplayEventTypeEXT 
                       }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDisplayEvent (poked :: VkDisplayEventInfoEXT))
pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME =  "VK_EXT_display_control"

data VkDeviceEventInfoEXT =
  VkDeviceEventInfoEXT{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkDeviceEvent :: VkDeviceEventTypeEXT 
                      }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceEventInfoEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceEventInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDeviceEvent (poked :: VkDeviceEventInfoEXT))
-- ** vkDisplayPowerControlEXT
foreign import ccall "dynamic" mkvkDisplayPowerControlEXT :: FunPtr (VkDevice ->
  VkDisplayKHR -> Ptr VkDisplayPowerInfoEXT -> IO VkResult) -> (VkDevice ->
  VkDisplayKHR -> Ptr VkDisplayPowerInfoEXT -> IO VkResult)
vkDisplayPowerControlEXT :: VkDevice ->
  VkDisplayKHR -> Ptr VkDisplayPowerInfoEXT -> IO VkResult
vkDisplayPowerControlEXT d = (mkvkDisplayPowerControlEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDisplayPowerControlEXT" $ vkGetDeviceProcAddr d
pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION =  0x1
-- ** vkGetSwapchainCounterEXT
foreign import ccall "dynamic" mkvkGetSwapchainCounterEXT :: FunPtr (VkDevice ->
  VkSwapchainKHR ->
    VkSurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO VkResult) -> (VkDevice ->
  VkSwapchainKHR ->
    VkSurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO VkResult)
vkGetSwapchainCounterEXT :: VkDevice ->
  VkSwapchainKHR ->
    VkSurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO VkResult
vkGetSwapchainCounterEXT d = (mkvkGetSwapchainCounterEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetSwapchainCounterEXT" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT = VkStructureType 1000091002
-- ** VkDisplayEventTypeEXT
newtype VkDisplayEventTypeEXT = VkDisplayEventTypeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDisplayEventTypeEXT where
  showsPrec _ VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = showString "VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
  showsPrec p (VkDisplayEventTypeEXT x) = showParen (p >= 11) (showString "VkDisplayEventTypeEXT " . showsPrec 11 x)

instance Read VkDisplayEventTypeEXT where
  readPrec = parens ( choose [ ("VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT", pure VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayEventTypeEXT")
                        v <- step readPrec
                        pure (VkDisplayEventTypeEXT v)
                        )
                    )

pattern VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = VkDisplayEventTypeEXT 0
-- ** vkRegisterDisplayEventEXT
foreign import ccall "dynamic" mkvkRegisterDisplayEventEXT :: FunPtr (VkDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayEventInfoEXT ->
      Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult) -> (VkDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayEventInfoEXT ->
      Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult)
vkRegisterDisplayEventEXT :: VkDevice ->
  VkDisplayKHR ->
    Ptr VkDisplayEventInfoEXT ->
      Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult
vkRegisterDisplayEventEXT d = (mkvkRegisterDisplayEventEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkRegisterDisplayEventEXT" $ vkGetDeviceProcAddr d
-- ** VkDisplayPowerStateEXT
newtype VkDisplayPowerStateEXT = VkDisplayPowerStateEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDisplayPowerStateEXT where
  showsPrec _ VK_DISPLAY_POWER_STATE_OFF_EXT = showString "VK_DISPLAY_POWER_STATE_OFF_EXT"
  showsPrec _ VK_DISPLAY_POWER_STATE_SUSPEND_EXT = showString "VK_DISPLAY_POWER_STATE_SUSPEND_EXT"
  showsPrec _ VK_DISPLAY_POWER_STATE_ON_EXT = showString "VK_DISPLAY_POWER_STATE_ON_EXT"
  showsPrec p (VkDisplayPowerStateEXT x) = showParen (p >= 11) (showString "VkDisplayPowerStateEXT " . showsPrec 11 x)

instance Read VkDisplayPowerStateEXT where
  readPrec = parens ( choose [ ("VK_DISPLAY_POWER_STATE_OFF_EXT", pure VK_DISPLAY_POWER_STATE_OFF_EXT)
                             , ("VK_DISPLAY_POWER_STATE_SUSPEND_EXT", pure VK_DISPLAY_POWER_STATE_SUSPEND_EXT)
                             , ("VK_DISPLAY_POWER_STATE_ON_EXT", pure VK_DISPLAY_POWER_STATE_ON_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPowerStateEXT")
                        v <- step readPrec
                        pure (VkDisplayPowerStateEXT v)
                        )
                    )

pattern VK_DISPLAY_POWER_STATE_OFF_EXT = VkDisplayPowerStateEXT 0

pattern VK_DISPLAY_POWER_STATE_SUSPEND_EXT = VkDisplayPowerStateEXT 1

pattern VK_DISPLAY_POWER_STATE_ON_EXT = VkDisplayPowerStateEXT 2

data VkDisplayPowerInfoEXT =
  VkDisplayPowerInfoEXT{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkPowerState :: VkDisplayPowerStateEXT 
                       }
  deriving (Eq, Ord, Show)
instance Storable VkDisplayPowerInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayPowerInfoEXT <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPowerInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPowerState (poked :: VkDisplayPowerInfoEXT))
pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT = VkStructureType 1000091001
-- ** VkDeviceEventTypeEXT
newtype VkDeviceEventTypeEXT = VkDeviceEventTypeEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkDeviceEventTypeEXT where
  showsPrec _ VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = showString "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
  showsPrec p (VkDeviceEventTypeEXT x) = showParen (p >= 11) (showString "VkDeviceEventTypeEXT " . showsPrec 11 x)

instance Read VkDeviceEventTypeEXT where
  readPrec = parens ( choose [ ("VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT", pure VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceEventTypeEXT")
                        v <- step readPrec
                        pure (VkDeviceEventTypeEXT v)
                        )
                    )

pattern VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = VkDeviceEventTypeEXT 0
pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT = VkStructureType 1000091000
-- ** vkRegisterDeviceEventEXT
foreign import ccall "dynamic" mkvkRegisterDeviceEventEXT :: FunPtr (VkDevice ->
  Ptr VkDeviceEventInfoEXT ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult) -> (VkDevice ->
  Ptr VkDeviceEventInfoEXT ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult)
vkRegisterDeviceEventEXT :: VkDevice ->
  Ptr VkDeviceEventInfoEXT ->
    Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult
vkRegisterDeviceEventEXT d = (mkvkRegisterDeviceEventEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkRegisterDeviceEventEXT" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT = VkStructureType 1000091003
