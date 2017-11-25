{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.ExternalFence where

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
import Graphics.Vulkan.KHR.ExternalFenceCapabilities( VkExternalFenceHandleTypeFlagBitsKHR(..)
                                                    , VkExternalFenceHandleTypeFlagsKHR(..)
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

-- ** VkFenceImportFlagsKHR
newtype VkFenceImportFlagBitsKHR = VkFenceImportFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for VkFenceImportFlagBitsKHR
type VkFenceImportFlagsKHR = VkFenceImportFlagBitsKHR

instance Show VkFenceImportFlagBitsKHR where
  showsPrec _ VK_FENCE_IMPORT_TEMPORARY_BIT_KHR = showString "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
  
  showsPrec p (VkFenceImportFlagBitsKHR x) = showParen (p >= 11) (showString "VkFenceImportFlagBitsKHR " . showsPrec 11 x)

instance Read VkFenceImportFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_FENCE_IMPORT_TEMPORARY_BIT_KHR", pure VK_FENCE_IMPORT_TEMPORARY_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceImportFlagBitsKHR")
                        v <- step readPrec
                        pure (VkFenceImportFlagBitsKHR v)
                        )
                    )

pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR = VkFenceImportFlagBitsKHR 0x1

data VkExportFenceCreateInfoKHR =
  VkExportFenceCreateInfoKHR{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkHandleTypes :: VkExternalFenceHandleTypeFlagsKHR 
                            }
  deriving (Eq, Ord, Show)
instance Storable VkExportFenceCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportFenceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportFenceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportFenceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportFenceCreateInfoKHR))
