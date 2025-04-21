{-# LANGUAGE FieldSelectors #-}

module Bluefin.Free (
  Free,
  free,
  runFree,
)
where

import Ourlude

import Bluefin.Compound (Handle (..), makeOp, useImpl, useImplIn, useImplUnder)
import Bluefin.Eff (Eff, (:&), (:>))

newtype Free f e = Free {runFreeImpl :: forall e' a. f a -> Eff (e' :& e) a}

instance Handle (Free f) where
  mapHandle e = Free (useImplUnder <<< (runFreeImpl e))

free :: (e :> es) => Free f e -> f a -> Eff es a
free e x = useImpl (makeOp (runFreeImpl e x))

runFree ::
  (forall r. f r -> Eff es r) ->
  (forall e'. Free f e' -> Eff (e' :& es) a) ->
  Eff es a
runFree handler k =
  useImplIn k (Free (useImpl <<< handler))
