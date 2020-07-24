-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Shaped.MatMul(matMul, convolve) where
import Data.Array.Convert
import qualified Data.Array.Dynamic.MatMul as D
import Data.Array.Shaped
import Data.Array.Internal.Shape
import GHC.TypeLits

-- | Simple (and slow) matrix multiply.
matMul :: (Num a, KnownNat m, KnownNat n, KnownNat o) =>
          Array [m, n] a -> Array [n, o] a -> Array [m, o] a
matMul x y = convert (D.matMul (convert x) (convert y))

-- | Convolve the /n/ outer dimensions with the given kernel.
-- There is no padding nor striding.
-- The input has shape /spatialSh/ ++ /channelSh/,
-- the kernel has shape /spatialKernelSh/ ++ /channelSh/ ++ /featureSh/,
-- and the result has shape /spatialOutSh/ ++ /featureSh/.
-- The /n/ gives the rank of the /spatialSh/.
--
-- Example:
-- @
--  i :: Array [20,30,3] T  -- 20x30 image with 3 channels
--  k :: Array [5,5,3,8] T  -- 5x5 kernel with 8 output features
--  convolve @2 i k :: Array [16,26,8] T
-- @
convolve :: forall (n :: Nat) ish ksh osh wsh a ksc ksf i ws isp iwc .
            ( i ~ Rank ish                       -- input rank
            , ws ~ Take n ksh                    -- window size
            , Window ws ish wsh, KnownNat (Rank ws)
            , ksc ~ Size (Take i ksh)            -- spatial + channels
            , ksf ~ Size (Drop i ksh)            -- features
            , isp ~ Size (Take n wsh)            -- spatial
            , iwc ~ Size (Drop n wsh)            -- kernel + channels
            , iwc ~ ksc
            , osh ~ (Take n wsh ++ Drop i ksh)
            , Size wsh ~ (isp * iwc)
            , Size ksh ~ (ksc * ksf)
            , Size osh ~ (isp * ksf)
            , Shape wsh, Shape ksh, Shape osh
            , KnownNat ksc, KnownNat isp, KnownNat ksf
            , Num a
            ) =>
            Array ish a -> Array ksh a -> Array osh a
convolve i k =
  let iw :: Array wsh a
      iw = window @ws i
      ir :: Array [isp, iwc] a
      ir = reshape iw
      kr :: Array [ksc, ksf] a
      kr = reshape k
      m  :: Array [isp, ksf] a
      m  = matMul ir kr
      r  :: Array osh a
      r  = reshape m
  in  r

_example :: Array [20,30,3] Int -> Array [5,5,3,8] Int -> Array [16,26,8] Int
_example = convolve @2
