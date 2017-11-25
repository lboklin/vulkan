{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.NV.ViewportSwizzle where

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
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )


data VkViewportSwizzleNV =
  VkViewportSwizzleNV{ vkX :: VkViewportCoordinateSwizzleNV 
                     , vkY :: VkViewportCoordinateSwizzleNV 
                     , vkZ :: VkViewportCoordinateSwizzleNV 
                     , vkW :: VkViewportCoordinateSwizzleNV 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkViewportSwizzleNV where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkViewportSwizzleNV <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkViewportSwizzleNV))
                *> poke (ptr `plusPtr` 12) (vkW (poked :: VkViewportSwizzleNV))

data VkPipelineViewportSwizzleStateCreateInfoNV =
  VkPipelineViewportSwizzleStateCreateInfoNV{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkFlags :: VkPipelineViewportSwizzleStateCreateFlagsNV 
                                            , vkViewportCount :: Word32 
                                            , vkPViewportSwizzles :: Ptr VkViewportSwizzleNV 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkPipelineViewportSwizzleStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportSwizzleStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportSwizzles (poked :: VkPipelineViewportSwizzleStateCreateInfoNV))
-- ** VkViewportCoordinateSwizzleNV
newtype VkViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV Int32
  deriving (Eq, Ord, Storable)

instance Show VkViewportCoordinateSwizzleNV where
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV"
  showsPrec _ VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = showString "VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV"
  showsPrec p (VkViewportCoordinateSwizzleNV x) = showParen (p >= 11) (showString "VkViewportCoordinateSwizzleNV " . showsPrec 11 x)

instance Read VkViewportCoordinateSwizzleNV where
  readPrec = parens ( choose [ ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV)
                             , ("VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV", pure VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkViewportCoordinateSwizzleNV")
                        v <- step readPrec
                        pure (VkViewportCoordinateSwizzleNV v)
                        )
                    )

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_X_NV = VkViewportCoordinateSwizzleNV 0

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_X_NV = VkViewportCoordinateSwizzleNV 1

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Y_NV = VkViewportCoordinateSwizzleNV 2

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Y_NV = VkViewportCoordinateSwizzleNV 3

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_Z_NV = VkViewportCoordinateSwizzleNV 4

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_Z_NV = VkViewportCoordinateSwizzleNV 5

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_POSITIVE_W_NV = VkViewportCoordinateSwizzleNV 6

pattern VK_VIEWPORT_COORDINATE_SWIZZLE_NEGATIVE_W_NV = VkViewportCoordinateSwizzleNV 7
-- ** VkPipelineViewportSwizzleStateCreateFlagsNV-- | Opaque flag
newtype VkPipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
