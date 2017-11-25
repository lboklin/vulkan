{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.EXT.ValidationFlags where

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
import Graphics.Vulkan.Core( VkStructureType(..)
                           )


data VkValidationFlagsEXT =
  VkValidationFlagsEXT{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkDisabledValidationCheckCount :: Word32 
                      , vkPDisabledValidationChecks :: Ptr VkValidationCheckEXT 
                      }
  deriving (Eq, Ord, Show)
instance Storable VkValidationFlagsEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkValidationFlagsEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkValidationFlagsEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkValidationFlagsEXT))
                *> poke (ptr `plusPtr` 16) (vkDisabledValidationCheckCount (poked :: VkValidationFlagsEXT))
                *> poke (ptr `plusPtr` 24) (vkPDisabledValidationChecks (poked :: VkValidationFlagsEXT))
-- ** VkValidationCheckEXT
newtype VkValidationCheckEXT = VkValidationCheckEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkValidationCheckEXT where
  showsPrec _ VK_VALIDATION_CHECK_ALL_EXT = showString "VK_VALIDATION_CHECK_ALL_EXT"
  showsPrec _ VK_VALIDATION_CHECK_SHADERS_EXT = showString "VK_VALIDATION_CHECK_SHADERS_EXT"
  showsPrec p (VkValidationCheckEXT x) = showParen (p >= 11) (showString "VkValidationCheckEXT " . showsPrec 11 x)

instance Read VkValidationCheckEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_CHECK_ALL_EXT", pure VK_VALIDATION_CHECK_ALL_EXT)
                             , ("VK_VALIDATION_CHECK_SHADERS_EXT", pure VK_VALIDATION_CHECK_SHADERS_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationCheckEXT")
                        v <- step readPrec
                        pure (VkValidationCheckEXT v)
                        )
                    )

pattern VK_VALIDATION_CHECK_ALL_EXT = VkValidationCheckEXT 0

pattern VK_VALIDATION_CHECK_SHADERS_EXT = VkValidationCheckEXT 1
