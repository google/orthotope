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

{-# LANGUAGE ScopedTypeVariables #-}
module DynamicUTest(test) where

import Control.DeepSeq
import Control.Exception
import Data.Array.DynamicU
import qualified Data.Vector.Unboxed as V
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, assertFailure, Assertion)

assertThrows :: (NFData a) => String -> a -> Assertion
assertThrows s a = catch (deepseq a $ assertFailure s) (\ (_ :: ErrorCall) -> return ())

test :: Test
test = testGroup "DynamicU" $
  let a1, a2 :: Array Int
      a1 = fromList [2,3] [1..6]
      a2 = transpose [1,0] a1
      show_1 = assertEqual "1" "fromList [2,3] [1,2,3,4,5,6]" (show a1)
      show_2 = assertEqual "2" "fromList [3,2] [1,4,2,5,3,6]" (show a2)
      eq_1 = assertEqual "1" True (a1 == a1)
      eq_2 = assertEqual "2" False (a1 == a2)
      ord_1 = assertEqual "1" EQ (a1 `compare` a1)
      ord_2 = assertEqual "2" LT (a1 `compare` a2)
      shapeL_1 = assertEqual "1" [2,3] (shapeL a1)
      shapeL_2 = assertEqual "2" [3,2] (shapeL a2)
      rank_1 = assertEqual "1" 2 (rank a1)
      rank_2 = assertEqual "2" 2 (rank a2)
      index_1 = assertEqual "1" (fromList [3] [1,2,3]) (index a1 0)
      index_2 = assertEqual "2" (fromList [2] [1,4]) (index a2 0)
      index_3 = assertEqual "3" (fromList [] [4]) (a2 `index` 0 `index` 1)
      index_4 = assertThrows "<0" (index a1 (-1))
      index_5 = assertThrows ">" (index a1 2)
      toList_1 = assertEqual "1" [1,2,3,4,5,6] (toList a1)
      toList_2 = assertEqual "2" [1,4,2,5,3,6] (toList a2)
      toVector_1 = assertEqual "1" (V.fromList [1,2,3,4,5,6]) (toVector a1)
      toVector_2 = assertEqual "2" (V.fromList [1,4,2,5,3,6]) (toVector a2)
      fromList_1 = assertThrows "sh" (fromList [] [1,2::Int])
      fromList_2 = assertThrows "sh" (fromList [4,5] [1,2::Int])
      fromVector_1 = assertEqual "1" a1 (fromVector [2,3] $ V.fromList [1..6])
      normalize_1 = assertEqual "1" a1 (normalize a1)
      reshape_1 = assertEqual "1" (fromList [6] [1..6]) (reshape [6] a1)
      reshape_2 = assertEqual "1" (fromList [1,2,3,1] [1,4,2,5,3,6]) (reshape [1,2,3,1] a2)
      a3, a4 :: Array Int
      a3 = fromList [1] [5]
      a4 = fromList [] [5]
      stretch_1 = assertEqual "1" (fromList [3] [5,5,5]) (stretch [3] a3)
      stretch_2 = assertEqual "2" (fromList [2,2,3,2] [1,1,2,2,3,3,4,4,5,5,6,6,1,1,2,2,3,3,4,4,5,5,6,6])
                                  (stretch [2,2,3,2] $ reshape [1,2,3,1] a1)
      stretch_3 = assertThrows "3" (stretch [1,2] a3)
      stretch_4 = assertThrows "4" (stretch [4,3] a1)
      scalar_1 = assertEqual "1" a4 (scalar 5)
      unScalar_1 = assertEqual "1" 5 (unScalar a4)
      unScalar_2 = assertThrows "2" (unScalar a3)
      constant_1 = assertEqual "1" (fromList [2,3] [1,1,1,1,1,1]) (constant [2,3] (1::Int))
      mapA_1 = assertEqual "1" (fromList [2,3] [2..7]) (mapA succ a1)
      mapA_2 = assertEqual "1" (fromList [3,2] [2,5,3,6,4,7]) (mapA succ a2)
      zipWithA_1 = assertEqual "1" (fromList [2,3] [2,4..12]) (zipWithA (+) a1 a1)
      zipWithA_2 = assertThrows "2" (zipWithA (+) a1 a2)
      zipWith3A_1 = assertEqual "1" (fromList [2,3] [2,6,12,20,30,42]) (zipWith3A (\ x y z -> x*y+z) a1 a1 a1)
      pad_1 = assertEqual "1" (fromList [5,10] [9,9,9,9,9,9,9,9,9,9,
                                                9,9,9,1,2,3,9,9,9,9,
                                                9,9,9,4,5,6,9,9,9,9,
                                                9,9,9,9,9,9,9,9,9,9,
                                                9,9,9,9,9,9,9,9,9,9])
                              (pad [(1,2),(3,4)] 9 a1)
      pad_2 = assertThrows "2" (pad [(1,1),(1,1),(1,1)] 0 a1)
      a5 :: Array Int
      a5 = fromList [2,3,4] [1..24]
      transpose_1 = assertEqual "1" (fromList [2,3,4] [1,2,3,4,
                                                       5,6,7,8,
                                                       9,10,11,12,

                                                       13,14,15,16,
                                                       17,18,19,20,
                                                       21,22,23,24])
                                    (transpose [0,1,2] a5)
      transpose_2 = assertEqual "2" (fromList [2,4,3] [1,5,9,
                                                       2,6,10,
                                                       3,7,11,
                                                       4,8,12,

                                                       13,17,21,
                                                       14,18,22,
                                                       15,19,23,
                                                       16,20,24])
                                    (transpose [0,2,1] a5)
      transpose_3 = assertEqual "3" (fromList [3,2,4] [1,2,3,4,
                                                       13,14,15,16,

                                                       5,6,7,8,
                                                       17,18,19,20,

                                                       9,10,11,12,
                                                       21,22,23,24])
                                    (transpose [1,0,2] a5)
      transpose_4 = assertEqual "4" (fromList [3,4,2] [1,13,
                                                       2,14,
                                                       3,15,
                                                       4,16,

                                                       5,17,
                                                       6,18,
                                                       7,19,
                                                       8,20,

                                                       9,21,
                                                       10,22,
                                                       11,23,
                                                       12,24])
                                    (transpose [1,2,0] a5)
      transpose_5 = assertEqual "5" (fromList [4,2,3] [1,5,9,
                                                       13,17,21,

                                                       2,6,10,

                                                       14,18,22,

                                                       3,7,11,
                                                       15,19,23,

                                                       4,8,12,
                                                       16,20,24])
                                    (transpose [2,0,1] a5)
      transpose_6 = assertEqual "6" (fromList [4,3,2] [1,13,
                                                       5,17,
                                                       9,21,

                                                       2,14,
                                                       6,18,
                                                       10,22,

                                                       3,15,
                                                       7,19,
                                                       11,23,

                                                       4,16,
                                                       8,20,
                                                       12,24])
                                    (transpose [2,1,0] a5)
      transpose_7 = assertThrows "7" (transpose [0,1,2,3] a5)
      transpose_8 = assertThrows "7" (transpose [0,1,3] a5)
      transpose_9 = assertEqual "9" (fromList [3,2,4] [1,2,3,4,
                                                       13,14,15,16,

                                                       5,6,7,8,
                                                       17,18,19,20,

                                                       9,10,11,12,
                                                       21,22,23,24])
                                    (transpose [1,0] a5)
      append_1 = assertEqual "1" (fromList [3,3] [1..9])
                                 (append a1 (fromList [1,3] [7,8,9]))
      concatOuter_1 = assertEqual "1" (fromList [6,3] [1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6])
                                      (concatOuter [a1, concatOuter [a1,a1]])
      concatOuter_2 = assertThrows "2" (concatOuter [a1, a2])
      a6 :: Array Int
      a6 = fromList [4,5] [1..20]
      window_1 = assertEqual "1" (fromList [2,3,3,3] [1,2,3,
                                                      6,7,8,
                                                      11,12,13,

                                                      2,3,4,
                                                      7,8,9,
                                                      12,13,14,

                                                      3,4,5,
                                                      8,9,10,
                                                      13,14,15,


                                                      6,7,8,
                                                      11,12,13,
                                                      16,17,18,

                                                      7,8,9,
                                                      12,13,14,
                                                      17,18,19,

                                                      8,9,10,
                                                      13,14,15,
                                                      18,19,20])
                                 (window [3,3] a6)
      window_2 = assertThrows "2" (window [3,6] a6)
      window_3 = assertThrows "3" (window [3,3,3] a6)
      stride_1 = assertEqual "1" (fromList [2,2,2] [1,3,
                                                    9,11,

                                                    13,15,
                                                    21,23])
                                 (stride [1,2,2] a5)
      stride_2 = assertThrows "2" (stride [1,2,2] a1)
      slice_1 = assertEqual "1" (fromList [2,2,1] [8,12,20,24])
                                (slice [(0,2),(1,2),(3,1)] a5)
      slice_2 = assertThrows "2" (slice [(0,0)] a4)
      slice_3 = assertThrows "3" (slice [(-1,1)] a5)
      slice_4 = assertThrows "4" (slice [(10,0)] a5)
      slice_5 = assertThrows "5" (slice [(0,3)] a5)
      a7 = mapA succ a5
      dot x y = reduce (+) 0 $ zipWithA (*) x y
      rerank2_1 = assertEqual "1" (fromList [2,3] [40,200,488,904,1448,2120])
                                  (rerank2 2 dot a5 a7)
      rev_1 = assertEqual "1" (fromList [2,3] [3,2,1,6,5,4])
                              (rev [1] a1)
      rev_2 = assertEqual "2" (fromList [2,3] [6,5,4,3,2,1])
                              (rev [0,1] a1)
      rev_3 = assertThrows "3" (rev [2] a1)
      reduce_1 = assertEqual "1" (scalar 720) (reduce (*) 1 a1)
      reduce_2 = assertEqual "2" (fromList [2] [6,120]) (rerank 1 (reduce (*) 1) a1)
      reduce_3 = assertEqual "3" (fromList [3] [4,10,18]) (rerank 1 (reduce (*) 1) a2)

      tests =
        [ testCase "show_1" show_1
        , testCase "show_2" show_2
        , testCase "eq_1" eq_1
        , testCase "eq_2" eq_2
        , testCase "ord_1" ord_1
        , testCase "ord_2" ord_2
        , testCase "shapeL_1" shapeL_1
        , testCase "shapeL_2" shapeL_2
        , testCase "rank_1" rank_1
        , testCase "rank_2" rank_2
        , testCase "index_1" index_1
        , testCase "index_2" index_2
        , testCase "index_3" index_3
        , testCase "index_4" index_4
        , testCase "index_5" index_5
        , testCase "toList_1" toList_1
        , testCase "toList_2" toList_2
        , testCase "toVector_1" toVector_1
        , testCase "toVector_2" toVector_2
        , testCase "fromList_1" fromList_1
        , testCase "fromList_2" fromList_2
        , testCase "fromVector_1" fromVector_1
        , testCase "normalize_1" normalize_1
        , testCase "reshape_1" reshape_1
        , testCase "reshape_2" reshape_2
        , testCase "stretch_1" stretch_1
        , testCase "stretch_2" stretch_2
        , testCase "stretch_3" stretch_3
        , testCase "stretch_4" stretch_4
        , testCase "scalar_1" scalar_1
        , testCase "unScalar_1" unScalar_1
        , testCase "unScalar_2" unScalar_2
        , testCase "constant_1" constant_1
        , testCase "mapA_1" mapA_1
        , testCase "mapA_2" mapA_2
        , testCase "zipWithA_1" zipWithA_1
        , testCase "zipWithA_2" zipWithA_2
        , testCase "zipWith3A_1" zipWith3A_1
        , testCase "pad_1" pad_1
        , testCase "pad_2" pad_2
        , testCase "transpose_1" transpose_1
        , testCase "transpose_2" transpose_2
        , testCase "transpose_3" transpose_3
        , testCase "transpose_4" transpose_4
        , testCase "transpose_5" transpose_5
        , testCase "transpose_6" transpose_6
        , testCase "transpose_7" transpose_7
        , testCase "transpose_8" transpose_8
        , testCase "transpose_9" transpose_9
        , testCase "append_1" append_1
        , testCase "concatOuter_1" concatOuter_1
        , testCase "concatOuter_2" concatOuter_2
        , testCase "window_1" window_1
        , testCase "window_2" window_2
        , testCase "window_3" window_3
        , testCase "stride_1" stride_1
        , testCase "stride_2" stride_2
        , testCase "slice_1" slice_1
        , testCase "slice_2" slice_2
        , testCase "slice_3" slice_3
        , testCase "slice_4" slice_4
        , testCase "slice_5" slice_5
        , testCase "rerank2_1" rerank2_1
        , testCase "rev_1" rev_1
        , testCase "rev_2" rev_2
        , testCase "rev_3" rev_3
        , testCase "reduce_1" reduce_1
        , testCase "reduce_2" reduce_2
        , testCase "reduce_3" reduce_3
        ]
  in  tests
