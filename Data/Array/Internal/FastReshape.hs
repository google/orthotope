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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
--
-- This module contains specialized implementations for
-- reshape and toVector.  If the vector elements are Float
-- or Double we use carefull crafted code to flatten the
-- input array.  This code speeds up tests that do evaluation
-- by a factor of 3.
module CoreCompiler.ArrayReshape(
  reshape,
  toVector,
  debugShow,
  ) where
import Control.Monad.ST(runST)
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Storable(sizeOf)
import GHC.Stack(HasCallStack)
import Utils.Misc(assert)

import Data.Array.DynamicS as Arr hiding(reshape, toVector)
import qualified Data.Array.Internal as I
import qualified Data.Array.Internal.DynamicG as G
import qualified Data.Array.Internal.DynamicS as S

import CoreCompiler.Error

-- Copied from Data.Array.Internal.DynamicS and Data.Array.Internal.
reshape :: (HasCallStack, Unbox a) => ShapeL -> Array a -> Array a
reshape sh (S.A (G.A sh' t@(I.T ost oo v)))
  | n /= n' = internalError $ "reshape: size mismatch " ++ show (sh, sh')
  | I.vLength v == 1 = S.A $ G.A sh $ I.T (map (const 0) sh) 0 v
  | Just nst <- I.simpleReshape ost sh' sh = S.A $ G.A sh $ I.T nst oo v

  -- These are the specialized flattening operations.
  | sizeOf (v U.! 0) == sizeOf (0::Double) =
      S.A $ G.A sh $ I.T st 0 $ flattenVectorViaDouble sh' ost oo v
  | sizeOf (v U.! 0) == sizeOf (0::Float) =
      S.A $ G.A sh $ I.T st 0 $ flattenVectorViaFloat  sh' ost oo v

  | otherwise = S.A $ G.A sh $ I.T st 0 $ I.toVectorT sh' t
  where n : st = I.getStridesT sh
        !n' = product sh'

-- Copied from Data.Array.Internal.DynamicS and Data.Array.Internal.
toVector :: (Unbox a) => Array a -> U.Vector a
toVector (S.A (G.A sh t@(I.T ost oo v))) =
  let l : st = I.getStridesT sh
  in  if ost == st then
        -- All strides are normal, return entire slice of input vector.
        U.slice oo l v
      else
        if sizeOf (v U.! 0) == sizeOf (0::Double) then
          flattenVectorViaDouble sh ost oo v
        else if sizeOf (v U.! 0) == sizeOf (0::Float) then
          flattenVectorViaFloat sh ost oo v
        else
          I.toVectorT sh t

--- Only called when sizeOf a == sizeOf Double
--- It works on all 64 bit items on any sane architecture.
flattenVectorViaDouble :: (Unbox a) => [Int] -> [Int] -> Int -> U.Vector a -> U.Vector a
flattenVectorViaDouble [] _ o v = U.singleton $ v U.! o
flattenVectorViaDouble sh ist o v =
  assert (sizeOf (v U.! 0) == sizeOf (0 :: Double)) "flattenVectorDouble: size" $
  U.unsafeCast $ flattenVector sh ist o (U.unsafeCast v :: U.Vector Double)

--- Only called when sizeOf a == sizeOf Float
--- It works on all 32 bit items on any sane architecture.
flattenVectorViaFloat :: (Unbox a) => [Int] -> [Int] -> Int -> U.Vector a -> U.Vector a
flattenVectorViaFloat [] _ o v = U.singleton $ v U.! o
flattenVectorViaFloat sh ist o v =
  assert (sizeOf (v U.! 0) == sizeOf (0 :: Float)) "flattenVectorFloat" $
  U.unsafeCast $ flattenVector sh ist o (U.unsafeCast v :: U.Vector Float)

-- Verify that the given params will not index out of bounds of the
-- vector.
validParams :: (Unbox a) => [Int] -> [Int] -> Int -> U.Vector a -> Bool
validParams sh st o v =
  let maxIndex = sum $ zipWith (\ s t -> if t < 0 then 0 else (s-1) * t) sh st
      minIndex = sum $ zipWith (\ s t -> if t > 0 then 0 else (s-1) * t) sh st
  in  minIndex + o >= 0 && maxIndex + o < U.length v

-- This is carefully crafted code to match the performance of the coresponding
-- C code.  Some bangs may look unneeded, but they actually help.
-- Looking at the Core all loops use unboxed Ints as the loop variables,
-- and no bounds checking.
--
-- The input arguments are
--  sh:   The input array shape, i.e., the sizes of all dimensions.
--  ist:  The input strides, i.e., for each dimension, how much to step
--        the index in the input vector when moving to the next element.
--  oo:   Offset into the input vector to start from.
--  v:    The input vector.
--
--
-- In C terms, we execute a number of nested loops:
--
-- nv = alloc(...)
-- for (i0 = 0, i0 < sh[0], i0++) {
--   for (i1 = 0, i1 < sh[1], i1++) {
--     ...
--       for (in = 0, in < sh[n], in++) {
--         i = i0 * ist[0] + i1 * ist[1] + ...
--             in * ist[n];
--         *nv++ = v[i];
--       }
--     ...
--   }
-- }
--

-- The unsafe reads and writes are safe, if called with valid arguments.
-- Note, this is only efficient when specialized to some particular type a.
-- This is accomplished by inlining in the specialized functions above.
{-# INLINE flattenVector #-}
flattenVector :: (Unbox a) => [Int] -> [Int] -> Int -> U.Vector a -> U.Vector a
flattenVector [] _ o v = U.singleton $ v U.! o
flattenVector sh ist oo v =
  assert (validParams sh ist oo v) "flattenVector: params" $
  runST $ do
  -- The total size is n, and the output strides are ost.
  let n : ost = I.getStridesT sh
  nv <- M.unsafeNew n  -- We will fill the entire vector, so don't zero it.
  -- The current offset in the input vector is ioffs, and ooffs in the output vector.
  let flatten [d] (is:_) _ !ioffs !ooffs =
        -- This is the innermost dimension, so do some actual copying.
        copyLoop is d ioffs ooffs
      flatten (d:ds) (ais:iss) (aos:oss) !aioffs !aooffs = do
        let !is = ais
            !os = aos
            recLoop 0 _ _ = return ()
            recLoop !k !ioffs !ooffs = do
              flatten ds iss oss ioffs ooffs
              recLoop (k-1) (ioffs+is) (ooffs+os)
        recLoop d aioffs aooffs
      flatten _ _ _ _ _ = cannotHappen "impossible"

      copyLoop _    0  _  _ = return ()
      copyLoop !is !d !i !o = do
        x <- U.unsafeIndexM v i  -- Be strict in the vector
        M.unsafeWrite nv o x
        copyLoop is (d-1) (i + is) (o + 1)

  flatten sh ist ost oo 0
  U.unsafeFreeze nv

-------

debugShow :: Array a -> String
debugShow (S.A (G.A sh (I.T ss o _))) =
  "(A " ++ show sh ++ " (T " ++ show ss ++ " " ++ show o ++ " _))"
