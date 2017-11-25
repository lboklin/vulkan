{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.ValidationCache where

import Graphics.Vulkan.Device( VkDevice(..)
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
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( (+++)
                                      , step
                                      , prec
                                      )
import Graphics.Vulkan.OtherTypes( VkObjectType(..)
                                 )
import Graphics.Vulkan.DeviceInitialization( vkGetDeviceProcAddr
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

-- ** vkMergeValidationCachesEXT
foreign import ccall "dynamic" mkvkMergeValidationCachesEXT :: FunPtr (VkDevice ->
  VkValidationCacheEXT ->
    Word32 -> Ptr VkValidationCacheEXT -> IO VkResult) -> (VkDevice ->
  VkValidationCacheEXT ->
    Word32 -> Ptr VkValidationCacheEXT -> IO VkResult)
vkMergeValidationCachesEXT :: VkDevice ->
  VkValidationCacheEXT ->
    Word32 -> Ptr VkValidationCacheEXT -> IO VkResult
vkMergeValidationCachesEXT d = (mkvkMergeValidationCachesEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkMergeValidationCachesEXT" $ vkGetDeviceProcAddr d
-- ** vkCreateValidationCacheEXT
foreign import ccall "dynamic" mkvkCreateValidationCacheEXT :: FunPtr (VkDevice ->
  Ptr VkValidationCacheCreateInfoEXT ->
    Ptr VkAllocationCallbacks ->
      Ptr VkValidationCacheEXT -> IO VkResult) -> (VkDevice ->
  Ptr VkValidationCacheCreateInfoEXT ->
    Ptr VkAllocationCallbacks ->
      Ptr VkValidationCacheEXT -> IO VkResult)
vkCreateValidationCacheEXT :: VkDevice ->
  Ptr VkValidationCacheCreateInfoEXT ->
    Ptr VkAllocationCallbacks ->
      Ptr VkValidationCacheEXT -> IO VkResult
vkCreateValidationCacheEXT d = (mkvkCreateValidationCacheEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkCreateValidationCacheEXT" $ vkGetDeviceProcAddr d

data VkValidationCacheCreateInfoEXT =
  VkValidationCacheCreateInfoEXT{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkFlags :: VkValidationCacheCreateFlagsEXT 
                                , vkInitialDataSize :: CSize 
                                , vkPInitialData :: Ptr Void 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkValidationCacheCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
                                            <*> peek (ptr `plusPtr` 24)
                                            <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkInitialDataSize (poked :: VkValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPInitialData (poked :: VkValidationCacheCreateInfoEXT))
pattern VK_OBJECT_TYPE_VALIDATION_CACHE_EXT = VkObjectType 1000160000
-- ** vkGetValidationCacheDataEXT
foreign import ccall "dynamic" mkvkGetValidationCacheDataEXT :: FunPtr (VkDevice ->
  VkValidationCacheEXT -> Ptr CSize -> Ptr Void -> IO VkResult) -> (VkDevice ->
  VkValidationCacheEXT -> Ptr CSize -> Ptr Void -> IO VkResult)
vkGetValidationCacheDataEXT :: VkDevice ->
  VkValidationCacheEXT -> Ptr CSize -> Ptr Void -> IO VkResult
vkGetValidationCacheDataEXT d = (mkvkGetValidationCacheDataEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkGetValidationCacheDataEXT" $ vkGetDeviceProcAddr d
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160001
newtype VkValidationCacheEXT = VkValidationCacheEXT Word64
  deriving (Eq, Ord, Storable, Show)

data VkShaderModuleValidationCacheCreateInfoEXT =
  VkShaderModuleValidationCacheCreateInfoEXT{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkValidationCache :: VkValidationCacheEXT 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkShaderModuleValidationCacheCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkValidationCache (poked :: VkShaderModuleValidationCacheCreateInfoEXT))
pattern VK_EXT_VALIDATION_CACHE_EXTENSION_NAME =  "VK_EXT_validation_cache"
pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT = VkStructureType 1000160000
-- ** vkDestroyValidationCacheEXT
foreign import ccall "dynamic" mkvkDestroyValidationCacheEXT :: FunPtr (VkDevice ->
  VkValidationCacheEXT -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice ->
  VkValidationCacheEXT -> Ptr VkAllocationCallbacks -> IO ())
vkDestroyValidationCacheEXT :: VkDevice ->
  VkValidationCacheEXT -> Ptr VkAllocationCallbacks -> IO ()
vkDestroyValidationCacheEXT d = (mkvkDestroyValidationCacheEXT $ castFunPtr $ procAddr) d
  where procAddr = unsafePerformIO $ withCString "vkDestroyValidationCacheEXT" $ vkGetDeviceProcAddr d
pattern VK_EXT_VALIDATION_CACHE_SPEC_VERSION =  0x1
-- ** VkValidationCacheHeaderVersionEXT
newtype VkValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkValidationCacheHeaderVersionEXT where
  showsPrec _ VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = showString "VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"
  showsPrec p (VkValidationCacheHeaderVersionEXT x) = showParen (p >= 11) (showString "VkValidationCacheHeaderVersionEXT " . showsPrec 11 x)

instance Read VkValidationCacheHeaderVersionEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT", pure VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCacheHeaderVersionEXT")
                        v <- step readPrec
                        pure (VkValidationCacheHeaderVersionEXT v)
                        )
                    )

pattern VK_VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = VkValidationCacheHeaderVersionEXT 1
-- ** VkValidationCacheCreateFlagsEXT-- | Opaque flag
newtype VkValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
