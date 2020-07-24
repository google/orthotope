# Summary
This is a library for multi-dimensional arrays inspired by APL.

# Multi-dimnsional array
Each array has a number of elements of the same type, and a /shape/.
The shape can be described by a list of integers that gices the size
foir each of the dimensions.  E.g. the array shape `[2,3]` is a 2x3
matrix, and the shape `[]` is a single value (a scalar).  The number
of dimensions is called the /rank/ of the array.

The shape may or may not be part of the type, depending on which
version of the API you use.


# API variants
The API comes in many variants, depending on how strongly typed it is
and what the underlying storage is.

## Types

 * `Dynamic`, the shape is not part of the type, but is checked at
 runtime.  E.g., `Array Float` is an array of `Float` which can have
 any shape.

 * `Ranked`, the rank of the array is part of the type, but the actual
   sizes of the dimensions are checked at runtime.  E.g., `Array 2
   Float` is the type of 2-dimensional arrays (i.e., matrices) of `Float`.

 * `Shaped`, the shape of the array is part of the type and is checked
   statically.  E.g., `Array [2,3] Float` is the type of 2x3 arrays of
   `Float`.

Converting between these types is cheap since they all share the same
underlying representation.

## Storage

Each of the type variants has several storage variants, indicated by a
suffix of the module names.

 * `G` The generic array type where you can provide your own storage.

 * `S` Uses `Data.Vector.Storable` for storage.

 * `U` Uses `Data.Vector.Unboxed` for storage.

 * ` ` (empty suffix)  Uses `Data.Vector` for storage.

Conversion between different storage types requires copying the data,
so it is not a cheap operation.
