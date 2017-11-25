{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.ExternalSemaphore where

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
import Graphics.Vulkan.KHR.ExternalSemaphoreCapabilities( VkExternalSemaphoreHandleTypeFlagsKHR(..)
                                                        , VkExternalSemaphoreHandleTypeFlagBitsKHR(..)
                                                        )
import Data.Bits( Bits
                , FiniteBits
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
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )

-- ** VkSemaphoreImportFlagsKHR
newtype VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkSemaphoreImportFlagBitsKHR
type VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlagBitsKHR

instance Show VkSemaphoreImportFlagBitsKHR where
  showsPrec _ VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = showString "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
  
  showsPrec p (VkSemaphoreImportFlagBitsKHR x) = showParen (p >= 11) (showString "VkSemaphoreImportFlagBitsKHR " . showsPrec 11 x)

instance Read VkSemaphoreImportFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR", pure VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSemaphoreImportFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSemaphoreImportFlagBitsKHR v)
                        )
                    )

pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = VkSemaphoreImportFlagBitsKHR 0x1

data VkExportSemaphoreCreateInfoKHR =
  VkExportSemaphoreCreateInfoKHR{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkHandleTypes :: VkExternalSemaphoreHandleTypeFlagsKHR 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkExportSemaphoreCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportSemaphoreCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportSemaphoreCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportSemaphoreCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportSemaphoreCreateInfoKHR))
