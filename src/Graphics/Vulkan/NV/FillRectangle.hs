{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NV.FillRectangle where

import Graphics.Vulkan.Pipeline( VkPolygonMode(..)
                               )

pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME =  "VK_NV_fill_rectangle"
pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION =  0x1
pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV = VkPolygonMode 1000153000
