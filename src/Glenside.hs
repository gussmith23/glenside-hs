module Glenside (Frame, DimId, DimIdSet, Tensor, AccessPattern, tensorToAccessPattern, access, activations, weights, cartProd, a, b, dims) where

import Data.Map.Strict (Map, fromList, partitionWithKey, singleton)
import qualified Data.Map.Strict as Data.Set
import Data.Set as Set (Set, fromList, member)
import Data.Stack (Stack, stackNew, stackPop, stackPush)
import GHC.Base (assert)
import GHC.IO.Exception (stackOverflow)
import Numeric.Natural (Natural)

-- | Dimension identifiers are currently just strings.
type DimId = String

-- | A set of dimension identifiers.
type DimIdSet = Set DimId

dims :: [DimId] -> DimIdSet
dims = Set.fromList

-- | Our core abstraction, the access pattern, will conceptually be a stack,
--  whose stack frames are maps, mapping dimension identifiers to dimension
--  sizes.
type Frame = Map DimId Natural

-- | A tensor is just an unordered set of dimensions. Layouts (i.e. a dimension
--  orderings, for laying out data in memory) are imposed later.
type Tensor = Frame

-- | An access pattern is a stack of dimension sets. The topmost dimension set
--  represents the shape being processed.
type AccessPattern = Stack Frame

-- | We can convert a tensor directly into a simple access pattern.
tensorToAccessPattern :: Tensor -> AccessPattern
tensorToAccessPattern = stackPush stackNew

-- | Access the access pattern at the given dimensions.
access :: AccessPattern -> DimIdSet -> AccessPattern
access a s = case stackPop a of
  Just (rest, top) ->
    let (l, r) = partitionWithKey (\k _ -> Set.member k s) top
     in (stackPush (stackPush rest r) l)
  _ -> error ""

-- Pop all of s0 into s1.
popStackIntoStack :: Stack a -> Stack a -> Stack a
popStackIntoStack s0 s1 = case stackPop s0 of
  Just (rest, top) -> popStackIntoStack rest (stackPush s1 top)
  Nothing -> s1

reverseStack :: Stack a -> Stack a
reverseStack s =
  popStackIntoStack s stackNew

-- | Stack stack s1 onto s0.
stackStacks :: Stack a -> Stack a -> Stack a
stackStacks s0 s1 =
  popStackIntoStack (reverseStack s1) s0

-- |
cartProd :: AccessPattern -> AccessPattern -> AccessPattern
cartProd a0 a1 = case (stackPop a0, stackPop a1) of
  (Just (rest0, top0), Just (rest1, top1)) ->
    if top0 == top1
      then stackPush (stackPush (stackStacks rest0 rest1) (singleton "T" 2)) top0
      else error ""
  _ -> error ""

activations = tensorToAccessPattern $ Data.Map.Strict.fromList [("N", 1), ("C", 3), ("H", 32), ("W", 32)]

weights = tensorToAccessPattern $ Data.Map.Strict.fromList [("O", 8), ("I", 3), ("kH", 3), ("kW", 3)]

a = tensorToAccessPattern $ Data.Map.Strict.fromList [("M", 32), ("N", 16)]

b = tensorToAccessPattern $ Data.Map.Strict.fromList [("N", 16), ("O", 64)]
