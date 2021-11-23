module Glenside (Frame, DimId, DimIdSet, Tensor, AccessPattern, c, d, tensorToAccessPattern, access, activations, dim, weights, cartProd, a, b, dims, aTimesB, cTimesD) where

import Data.HashMap.Strict (difference, filterWithKey, fromList)
import qualified Data.HashMap.Strict as HashMap (HashMap, empty, filterWithKey, fromList, union)
import qualified Data.HashSet as HashSet (HashSet, fromList, member)
import GHC.Base (assert)
import GHC.IO.Exception (stackOverflow)
import Numeric.Natural (Natural)

-- | Dimension identifiers are currently just strings.
type DimId = String

-- | A set of dimension identifiers.
type DimIdSet = HashSet.HashSet DimId

dims :: [DimId] -> DimIdSet
dims = HashSet.fromList

dim :: DimId -> DimIdSet
dim d = HashSet.fromList [d]

-- | Our core abstraction, the access pattern, will conceptually be a stack,
--  whose stack frames are maps, mapping dimension identifiers to dimension
--  sizes.
type Frame = HashMap.HashMap DimId Natural

-- | A tensor is just an unordered set of dimensions. Layouts (i.e. a dimension
--  orderings, for laying out data in memory) are imposed later.
type Tensor = Frame

-- | An access pattern is a stack of dimension sets. The topmost dimension set
--  represents the shape being processed.
type AccessPattern = [Frame]

-- | We can convert a tensor directly into a simple access pattern.
tensorToAccessPattern :: Tensor -> AccessPattern
tensorToAccessPattern = return

-- | Access the access pattern at the given dimensions.
access :: AccessPattern -> DimIdSet -> AccessPattern
access (top : rest) s =
  let newtop = filterWithKey (\k _ -> HashSet.member k s) top
      newbot = difference top newtop
   in newtop : newbot : rest
access _ _ = undefined

-- |
-- Top frames must match.
-- Second frames don't need to match. They will be combined.
-- Rest of frames after that need to match.
cartProd :: AccessPattern -> AccessPattern -> AccessPattern
cartProd (top : next : rest) (top' : next' : rest')
  | top == top' && rest == rest' = top : HashMap.fromList [("T", 2)] : HashMap.union next next' : rest
cartProd [top] [top']
  | top == top' = [top, HashMap.fromList [("T", 2)]]
cartProd _ _ = undefined

dotProd :: AccessPattern -> AccessPattern
dotProd (top : tuple : rest) = rest
dotProd _ = []

toTensor :: AccessPattern -> Tensor
toTensor = foldr HashMap.union HashMap.empty

activations = tensorToAccessPattern $ fromList [("N", 1), ("C", 3), ("H", 32), ("W", 32)]

weights = tensorToAccessPattern $ fromList [("O", 8), ("I", 3), ("kH", 3), ("kW", 3)]

a = tensorToAccessPattern $ fromList [("M", 32), ("N", 16)]

b = tensorToAccessPattern $ fromList [("N", 16), ("O", 64)]

-- Matrix multiplication.
aTimesB = toTensor $ dotProd $ cartProd a' b'
  where
    a' = access a (dims ["N"])
    b' = access b (dims ["N"])

c = tensorToAccessPattern $ fromList [("batch", 3), ("M", 32), ("N", 16)]

d :: AccessPattern
d = tensorToAccessPattern $ fromList [("batch", 3), ("N", 16), ("O", 64)]

-- Batched matrix multiplication.
cTimesD = toTensor $ dotProd $ cartProd c' d'
  where
    c' = access (access c (dims ["M", "N"])) (dim "N")
    d' = access (access d (dims ["N", "O"])) (dim "N")
