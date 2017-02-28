module Config where

-- Contains some constants for drawing, such as window sizes, bounds and
-- standard sizes for objects.

aspectRatio
  , defaultVerticalResolution
  , defaultHorizontalResolution
  :: Float
aspectRatio = 16 / 9
defaultHorizontalResolution = 768
defaultVerticalResolution = defaultHorizontalResolution / aspectRatio

-- | Horizontal Resolution Range
-- Horizontal range of points that fall within the playing window
constHResRange
  , constVResRange :: (Float, Float)
constHResRange = (-0.5 * defaultHorizontalResolution,
                 0.5 * defaultHorizontalResolution)
constVResRange = (-0.5 * defaultVerticalResolution,
                 0.5 * defaultVerticalResolution)
