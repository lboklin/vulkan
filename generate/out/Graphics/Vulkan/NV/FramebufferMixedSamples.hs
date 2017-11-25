{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NV.FramebufferMixedSamples where

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
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CFloat
                      )

-- ** VkCoverageModulationModeNV
newtype VkCoverageModulationModeNV = VkCoverageModulationModeNV Int32
  deriving (Eq, Ord, Storable)

instance Show VkCoverageModulationModeNV where
  showsPrec _ VK_COVERAGE_MODULATION_MODE_NONE_NV = showString "VK_COVERAGE_MODULATION_MODE_NONE_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_RGB_NV = showString "VK_COVERAGE_MODULATION_MODE_RGB_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_ALPHA_NV = showString "VK_COVERAGE_MODULATION_MODE_ALPHA_NV"
  showsPrec _ VK_COVERAGE_MODULATION_MODE_RGBA_NV = showString "VK_COVERAGE_MODULATION_MODE_RGBA_NV"
  showsPrec p (VkCoverageModulationModeNV x) = showParen (p >= 11) (showString "VkCoverageModulationModeNV " . showsPrec 11 x)

instance Read VkCoverageModulationModeNV where
  readPrec = parens ( choose [ ("VK_COVERAGE_MODULATION_MODE_NONE_NV", pure VK_COVERAGE_MODULATION_MODE_NONE_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_RGB_NV", pure VK_COVERAGE_MODULATION_MODE_RGB_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_ALPHA_NV", pure VK_COVERAGE_MODULATION_MODE_ALPHA_NV)
                             , ("VK_COVERAGE_MODULATION_MODE_RGBA_NV", pure VK_COVERAGE_MODULATION_MODE_RGBA_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCoverageModulationModeNV")
                        v <- step readPrec
                        pure (VkCoverageModulationModeNV v)
                        )
                    )

pattern VK_COVERAGE_MODULATION_MODE_NONE_NV = VkCoverageModulationModeNV 0

pattern VK_COVERAGE_MODULATION_MODE_RGB_NV = VkCoverageModulationModeNV 1

pattern VK_COVERAGE_MODULATION_MODE_ALPHA_NV = VkCoverageModulationModeNV 2

pattern VK_COVERAGE_MODULATION_MODE_RGBA_NV = VkCoverageModulationModeNV 3

data VkPipelineCoverageModulationStateCreateInfoNV =
  VkPipelineCoverageModulationStateCreateInfoNV{ vkSType :: VkStructureType 
                                               , vkPNext :: Ptr Void 
                                               , vkFlags :: VkPipelineCoverageModulationStateCreateFlagsNV 
                                               , vkCoverageModulationMode :: VkCoverageModulationModeNV 
                                               , vkCoverageModulationTableEnable :: VkBool32 
                                               , vkCoverageModulationTableCount :: Word32 
                                               , vkPCoverageModulationTable :: Ptr CFloat 
                                               }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineCoverageModulationStateCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCoverageModulationStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkCoverageModulationMode (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkCoverageModulationTableEnable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 28) (vkCoverageModulationTableCount (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
                *> poke (ptr `plusPtr` 32) (vkPCoverageModulationTable (poked :: VkPipelineCoverageModulationStateCreateInfoNV))
-- ** VkPipelineCoverageModulationStateCreateFlagsNV-- | Opaque flag
newtype VkPipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
