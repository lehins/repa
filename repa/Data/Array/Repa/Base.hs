
module Data.Array.Repa.Base
        ( Array
        , Repr (..), (!)
        , Load (..)
        , deepSeqArrays)
where
import Data.Array.Repa.Shape


-- | Arrays with a representation tag, shape, and element type.
data family Array r sh e


-- | Class of supported array representations.
--
--   These operators are used to read the physical data,
--   or to compute it for the delayed and cursored representations.
--
class Repr r e where
 -- | Take the extent of an array.
 extent       :: Shape sh => Array r sh e -> sh

 -- | Shape polymorphic indexing
 index, unsafeIndex
        :: Shape sh => Array r sh e -> sh -> e

 {-# INLINE index #-}
 index arr ix           = arr `linearIndex`       toIndex (extent arr) ix

 {-# INLINE unsafeIndex #-}
 unsafeIndex arr ix     = arr `unsafeLinearIndex` toIndex (extent arr) ix

 -- | Linear indexing into underlying representation
 linearIndex, unsafeLinearIndex
        :: Shape sh => Array r sh e -> Int -> e

 {-# INLINE unsafeLinearIndex #-}
 unsafeLinearIndex      = linearIndex

 -- | Ensure an array's data structure is fully evaluated.
 deepSeqArray :: Shape sh => Array r sh e -> b -> b


-- | Alias for `index`
(!) :: (Repr r e, Shape sh) => Array r sh e -> sh -> e
(!) = index


-- | Load array data between representations.
--
--   * Loading a delayed array to a manifest representation invokes
--     parallel computation.
--
--   * Loading between arrays of the same manifest representation is a no-op.
--
--   * Loading between manifest representations can be constant time or require a
--     parallel copy, depending on whether the two representations can be easily
--     converted.
--
--   * To turn a manifest array back into a delayed array use 
--     `delay` or `makeCursored`.
--
class Shape sh => Load r1 r2 sh e where
 load :: Array r1 sh e  -> Array r2 sh e

deepSeqArrays 
        :: (Shape sh, Repr r e)
        => [Array r sh e] -> b -> b
{-# INLINE deepSeqArrays #-}
deepSeqArrays arrs x
 = case arrs of
        []              -> x

        [a1]
         -> a1 `deepSeqArray` x

        [a1, a2]
         -> a1 `deepSeqArray` a2 `deepSeqArray` x

        [a1, a2, a3]
         -> a1 `deepSeqArray` a2 `deepSeqArray` a3 `deepSeqArray` x

        [a1, a2, a3, a4]
         -> a1 `deepSeqArray` a2 `deepSeqArray` a3 `deepSeqArray` a4 `deepSeqArray` x

        _ -> error "deepSeqArrays: only works for up to four arrays"
