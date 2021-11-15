# Clone augustss/orthtope instead.  This fork is not maintained.

# Orthotope

## Disclaimer

This is not an officially supported Google product.

## Summary

This is a library for multi-dimensional arrays inspired by APL.

## Multi-dimensional arrays

Each array has a number of elements of the same type, and a *shape*. The shape
can be described by a list of integers that gives the size for each of the
dimensions. E.g. the array shape `[2,3]` is a 2x3 matrix (2 rows, 3
columns), and the shape `[]` is a single value (a scalar).
The number of dimensions is called the *rank* of the array.

The shape may or may not be part of the type, depending on which version of the
API you use.

## API variants

The API comes in many variants, depending on how strongly typed it is and what
the underlying storage is.

### Types

*   `Dynamic`, the shape is not part of the type, but is checked at runtime.
    E.g., `Array Float` is an array of `Float` which can have any shape.

*   `Ranked`, the rank of the array is part of the type, but the actual sizes of
    the dimensions are checked at runtime. E.g., `Array 2 Float` is the type of
    2-dimensional arrays (i.e., matrices) of `Float`.

*   `Shaped`, the shape of the array is part of the type and is checked
    statically. E.g., `Array [2,3] Float` is the type of 2x3 arrays of `Float`.

Converting between these types is cheap since they all share the same underlying
trepresentation.

### Storage

Each of the type variants has several storage variants, indicated by a suffix of
the module names.

*   `G` The generic array type where you can provide your own storage.

*   `S` Uses `Data.Vector.Storable` for storage.

*   `U` Uses `Data.Vector.Unboxed` for storage.

*   ` ` (empty suffix) Uses `Data.Vector` for storage.

Conversion between different storage types requires copying the data, so it is
not a cheap operation.

## API

The library API is mostly structural operations, i.e., operations that
treat the elements in a uniform way.  For more algorithmic operations,
e.g., matrix multiplication, we suggest using a different library,
like `hmatrix`.

### Examples using `Dynamic`

Some preliminaries:

```
> import Data.Array.Dynamic
> import Text.PrettyPrint.HughesPJClass
> pp = putStrLn . prettyShow
```

An easy way to create an array from a list is to use `fromList`;
the first argument is the shape of the array.

```
> m = fromList [2,3] [1..6]
> m
fromList [2,3] [1,2,3,4,5,6]
> shapeL m
[2,3]
> size m
6
```

Arrays can be pretty printed.  They are shown in the APL way:
The innermost dimension on a line, the next dimension vertically,
the next dimension vertically with an empty line in betwee, and so on.

```
> pp m
1 2 3
4 5 6
```

We can have an arbitrary number of dimensions.

```
> s = fromList [] [42]
> v = fromList [3] [7,8,9]
> a = fromList [2,3,4] [1..24]
> pp s
42
> pp v
7 8 9
> pp a
 1  2  3  4
 5  6  7  8
 9 10 11 12

13 14 15 16
17 18 19 20
21 22 23 24
```
