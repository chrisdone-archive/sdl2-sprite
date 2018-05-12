-- | Handle sprites in a simple way.

module SDL.Sprite
  ( load
  , animate
  , render
  , Sprite
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Foreign.C.Types
import qualified SDL.Image
import SDL.Vect
import SDL.Video.Renderer

-- | A loaded sprite.
data Sprite = Sprite
  { spriteDimensions :: !(V2 CInt)
    -- ^ Render dimensions.
  , spriteTexture :: !Texture
    -- ^ The texture to render from.
  , spriteFrame :: !CInt
    -- ^ The current frame index.
  , spriteTextureInfo :: !TextureInfo
    -- ^ Info about the sprite texture.
  , spriteRenderer :: !Renderer
    -- ^ The renderer.
  }

-- | Get the current source rectangle of the sprite's frame.
spriteRectangle :: Sprite -> Rectangle CInt
spriteRectangle s = Rectangle (P (V2 x 0)) (V2 w h)
  where (V2 w h) = spriteDimensions s
        x = spriteFrame s * w

-- | Load a sprite from file.
load :: MonadIO m => Renderer -> FilePath -> V2 CInt -> m Sprite
load ren fp dimensions =
  liftIO
    (do texture <- SDL.Image.loadTexture ren fp
        textInfo <- queryTexture texture
        evaluate
          (Sprite
           { spriteDimensions = dimensions
           , spriteTexture = texture
           , spriteFrame = 0
           , spriteTextureInfo = textInfo
           , spriteRenderer = ren
           }))

-- | Advance sprite to the next frame.
animate :: Sprite -> Sprite
animate s =
  s
  { spriteFrame =
      if (spriteFrame s + 1) * w < textureWidth (spriteTextureInfo s)
        then spriteFrame s + 1
        else 0
  }
  where
    (V2 w _) = spriteDimensions s
{-# INLINE animate #-}

-- | Render the sprite at the given coordinates.
render :: MonadIO m => Sprite -> V2 CInt -> m ()
render s xy =
  copy
    (spriteRenderer s)
    (spriteTexture s)
    (Just (spriteRectangle s))
    (Just (Rectangle (P xy) (spriteDimensions s)))
{-# INLINE render #-}
