{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Bimonad
  ( Bimonad(..)
  ) where

import Data.Bifunctor

class (Bifunctor l, Bifunctor r) => Bimonad l r where
  {-# MINIMAL bireturnl,bireturnr,bijoinl,bijoinr #-} 
  
  bireturnl :: forall a b. a -> l a b
  bireturnr :: forall a b. b -> r a b
  bijoinl :: forall a b. l (l a b) (r a b) -> l a b
  bijoinr :: forall a b. r (l a b) (r a b) -> r a b
  
  bibindl :: forall a b c d. l a b -> (a -> l c d) -> (b -> r c d) -> l c d
  bibindl lab l r = bijoinl (bimap l r lab)

  bibindr :: forall a b c d. r a b -> (a -> l c d) -> (b -> r c d) -> r c d
  bibindr rab l r = bijoinr (bimap l r rab)

