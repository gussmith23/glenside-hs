module MyLib (Dim) where

import Data.Set as Set
import Data.Stack as Stack

-- |Dimensions are represented with identifiers, which are currently just
-- strings.
type Dim = String

-- |A set of dimensions will be a basic building block.
type DimSet = Set Dim

-- |A tensor is just an unordered set of dimensions. Layouts (i.e. a dimension
-- orderings, for laying out data in memory) are imposed later.
type Tensor = DimSet

-- |An access pattern is a stack of dimension sets. The topmost dimension set
-- represents the shape being processed.
type AccessPattern = Stack DimSet

-- |We can convert a tensor directly into a simple access pattern.
tensorToAccessPattern :: Tensor -> AccessPattern
tensorToAccessPattern tensor = stackNew