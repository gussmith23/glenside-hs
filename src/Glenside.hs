module Glenside (DimId, DimIdSet, Tensor, AccessPattern, tensorToAccessPattern, access) where

import Data.Set as Set
import Data.Stack as Stack
import Data.Map.Strict
import Numeric.Natural

-- |Dimension identifiers are currently just strings.
type DimId = String

-- |A set of dimension identifiers.
type DimIdSet = Set DimId

-- |Our core abstraction, the access pattern, will conceptually be a stack,
-- whose stack frames are maps, mapping dimension identifiers to dimension
-- sizes.
type Frame = Map DimId Natural

-- |A tensor is just an unordered set of dimensions. Layouts (i.e. a dimension
-- orderings, for laying out data in memory) are imposed later.
type Tensor = Frame

-- |An access pattern is a stack of dimension sets. The topmost dimension set
-- represents the shape being processed.
type AccessPattern = Stack Frame

-- |We can convert a tensor directly into a simple access pattern.
tensorToAccessPattern :: Tensor -> AccessPattern
tensorToAccessPattern tensor = stackPush stackNew tensor

-- |Access the access pattern at the given dimensions.
access :: AccessPattern -> DimIdSet -> AccessPattern
access a s = case stackPop a of 
  Just (rest, top) -> 
    let (l, r) = partitionWithKey (\k _ -> Set.member k s) top
    in (stackPush (stackPush rest r) l)
  _ -> error ""
