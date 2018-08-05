{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Bimonad.Relative
  ( Relative(..)
  ) where

-- | Relative Bimonads.
--
-- Laws:
--
-- [//]
--   @'bireturn' jab `bibind` k = k jab@
-- [//]
--   @m `bibind` 'bireturn' = m@
-- [//]
--   @m `bibind` (\jab -> k jab `bibind` h) = (m `bibind` k) `bibind` h@
class Relative j m where
  bireturn :: j a b -> m a b
  bibind :: m a b -> (j a b -> m c d) -> m c d

