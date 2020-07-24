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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Array.Convert(Convert(..)) where
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import GHC.TypeLits(KnownNat)

import qualified Data.Array.Internal as I
import qualified Data.Array.Internal.Dynamic as D
import qualified Data.Array.Internal.DynamicG as DG
import qualified Data.Array.Internal.DynamicS as DS
import qualified Data.Array.Internal.DynamicU as DU
import qualified Data.Array.Internal.Ranked as R
import qualified Data.Array.Internal.RankedG as RG
import qualified Data.Array.Internal.RankedS as RS
import qualified Data.Array.Internal.RankedU as RU
import qualified Data.Array.Internal.Shaped as S
import qualified Data.Array.Internal.ShapedG as SG
import qualified Data.Array.Internal.ShapedS as SS
import qualified Data.Array.Internal.ShapedU as SU
import Data.Array.Internal.Shape(Shape(..))

class Convert a b where
  -- | Convert between two array types.
  -- If both arrays have the same flavor of boxing this is a O(1)
  -- operation. Conversion between different boxing is a O(n) operation.
  convert  :: a -> b
  -- | Convert between two array types if possible, or return error message.
  convertE :: a -> Either String b
  {-# INLINE convert #-}
  convert = either error id . convertE
  {-# INLINE convertE #-}
  convertE = Right . convert
  {-# MINIMAL convert | convertE #-}

-- We write these instances like "instance (a ~ b) => Convert (X a) (Y b)" so
-- that instance resolution will commit to them as soon as it knows the array
-- types, and then introduce an equality constraint on the element types.  This
-- way, if you call 'convert' on an array, type inference can propagate the
-- element type across the conversion.  If we wrote them like  "instance
-- Convert (X a) (Y a)" instead, they'd not be selected until the element types
-- of both arrays are determined to be the same by other means, which would
-- lead to unnecessary ambiguity errors.

-----

instance (a ~ b, DU.Unbox a) => Convert (D.Array a) (DU.Array b) where
  convert (D.A (DG.A sh (I.T s o v))) = DU.A (DG.A sh (I.T s o (V.convert v)))

instance (a ~ b, DU.Unbox a) => Convert (DU.Array a) (D.Array b) where
  convert (DU.A (DG.A sh (I.T s o v))) = D.A (DG.A sh (I.T s o (V.convert v)))

-----

instance (a ~ b, DS.Unbox a) => Convert (D.Array a) (DS.Array b) where
  convert (D.A (DG.A sh (I.T s o v))) = DS.A (DG.A sh (I.T s o (V.convert v)))

instance (a ~ b, DS.Unbox a) => Convert (DS.Array a) (D.Array b) where
  convert (DS.A (DG.A sh (I.T s o v))) = D.A (DG.A sh (I.T s o (V.convert v)))

-----

-- As above with the element types, we arrange to select the Convert instance
-- before the ranks are known to be equal, then constrain them to be equal.

instance (a ~ b, n ~ m, RU.Unbox a) => Convert (R.Array n a) (RU.Array m b) where
  convert (R.A (RG.A sh (I.T s o v))) = RU.A (RG.A sh (I.T s o (V.convert v)))

instance (a ~ b, n ~ m, RU.Unbox a) => Convert (RU.Array n a) (R.Array m b) where
  convert (RU.A (RG.A sh (I.T s o v))) = R.A (RG.A sh (I.T s o (V.convert v)))

-----

instance (a ~ b, n ~ m, RS.Unbox a) => Convert (R.Array n a) (RS.Array m b) where
  convert (R.A (RG.A sh (I.T s o v))) = RS.A (RG.A sh (I.T s o (V.convert v)))

instance (a ~ b, n ~ m, RS.Unbox a) => Convert (RS.Array n a) (R.Array m b) where
  convert (RS.A (RG.A sh (I.T s o v))) = R.A (RG.A sh (I.T s o (V.convert v)))

-----

instance (a ~ b, n ~ m, SU.Unbox a) => Convert (S.Array n a) (SU.Array m b) where
  convert (S.A (SG.A (I.T s o v))) = SU.A (SG.A (I.T s o (V.convert v)))

instance (a ~ b, n ~ m, SU.Unbox a) => Convert (SU.Array n a) (S.Array m b) where
  convert (SU.A (SG.A (I.T s o v))) = S.A (SG.A (I.T s o (V.convert v)))

-----

instance (a ~ b, n ~ m, SS.Unbox a) => Convert (S.Array n a) (SS.Array m b) where
  convert (S.A (SG.A (I.T s o v))) = SS.A (SG.A (I.T s o (V.convert v)))

instance (a ~ b, n ~ m, SS.Unbox a) => Convert (SS.Array n a) (S.Array m b) where
  convert (SS.A (SG.A (I.T s o v))) = S.A (SG.A (I.T s o (V.convert v)))

-----

instance (a ~ b) => Convert (R.Array n a) (D.Array b) where
  convert (R.A (RG.A sh t)) = D.A (DG.A sh t)

instance (a ~ b, KnownNat n) => Convert (D.Array a) (R.Array n b) where
  convertE (D.A (DG.A sh t)) | length sh /= I.valueOf @n = Left "rank mismatch"
                             | otherwise = Right $ R.A (RG.A sh t)

instance (a ~ b, S.Shape sh) => Convert (S.Array sh a) (D.Array b) where
  convert (S.A a@(SG.A t)) = D.A (DG.A (SG.shapeL a) t)

instance (a ~ b, S.Rank sh ~ n, S.Shape sh) => Convert (S.Array sh a) (R.Array n b) where
  convert (S.A a@(SG.A t)) = R.A (RG.A (SG.shapeL a) t)

instance (a ~ b, S.Shape sh) => Convert (D.Array a) (S.Array sh b) where
  convertE (D.A (DG.A sh t)) | sh == shapeP (Proxy :: Proxy sh) = Right $ S.A (SG.A t)
  convertE _ = Left "shape mismatch"

instance (a ~ b, S.Rank sh ~ n, S.Shape sh) => Convert (R.Array n a) (S.Array sh b) where
  convertE (R.A (RG.A sh t)) | sh == shapeP (Proxy :: Proxy sh) = Right $ S.A (SG.A t)
  convertE _ = Left "shape mismatch"

------

instance (a ~ b, KnownNat n) => Convert (DS.Array a) (RS.Array n b) where
  convertE (DS.A (DG.A sh t)) | length sh /= I.valueOf @n = Left "rank mismatch"
                              | otherwise = Right $ RS.A (RG.A sh t)

instance (a ~ b, SS.Rank sh ~ n, SS.Shape sh) => Convert (RS.Array n a) (SS.Array sh b) where
  convertE (RS.A (RG.A sh t)) | sh == shapeP (Proxy :: Proxy sh) = Right $ SS.A (SG.A t)
  convertE _ = Left "shape mismatch"

instance (a ~ b, SS.Shape sh) => Convert (DS.Array a) (SS.Array sh b) where
  convertE (DS.A (DG.A sh t)) | sh == sh' = Right $ SS.A (SG.A t)
                              | otherwise = Left $ "shape mismatch: " ++ show (sh, sh')
                              where sh' = shapeP (Proxy :: Proxy sh)

instance (a ~ b, SS.Shape sh) => Convert (SS.Array sh a) (DS.Array b) where
  convert (SS.A a@(SG.A t)) = DS.A (DG.A (SG.shapeL a) t)

------

instance (a ~ b) => Convert (D.Array a) (DG.Array V.Vector b) where
  convert (D.A a) = a

instance (a ~ b) => Convert (DU.Array a) (DG.Array VU.Vector b) where
  convert (DU.A a) = a

instance (a ~ b) => Convert (DS.Array a) (DG.Array VS.Vector b) where
  convert (DS.A a) = a

------

instance (a ~ b, n ~ m) => Convert (R.Array n a) (RG.Array m V.Vector b) where
  convert (R.A a) = a

instance (a ~ b, n ~ m) => Convert (RU.Array n a) (RG.Array m VU.Vector b) where
  convert (RU.A a) = a

instance (a ~ b, n ~ m) => Convert (RS.Array n a) (RG.Array m VS.Vector b) where
  convert (RS.A a) = a

------

instance (a ~ b, s ~ t) => Convert (S.Array s a) (SG.Array t V.Vector b) where
  convert (S.A a) = a

instance (a ~ b, s ~ t) => Convert (SU.Array s a) (SG.Array t VU.Vector b) where
  convert (SU.A a) = a

instance (a ~ b, s ~ t) => Convert (SS.Array s a) (SG.Array t VS.Vector b) where
  convert (SS.A a) = a

------

-- XXX need more conversions.
