module ModuleStub where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic,from)

-- | module names
type MName = String

-- | type constructor names
type TCName = String

-- | data constructor names
type DCName = String

-- | A Module has a name, a list of imports, a list of declarations,
--   and a set of constructor names (which affect parsing).
data MModule decl = Module
  { moduleName :: MName,
    moduleImports :: [ModuleImport],
    moduleEntries :: [decl] ,
    moduleConstructors :: ConstructorNames
  }
  deriving (Show, Generic, Typeable)

-- | References to other modules (brings declarations and definitions into scope)
newtype ModuleImport = ModuleImport MName
  deriving (Show, Eq, Generic, Typeable)


-- | The names of type/data constructors used in the module
data ConstructorNames = ConstructorNames
  { tconNames :: Set TCName,
    dconNames :: Set DCName
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

-------------------------------------------------------------------
-- Prelude declarations for datatypes

initialTCNames :: Set TCName
initialTCNames = Set.fromList [sigmaName, boolName, tyUnitName]
initialDCNames :: Set DCName
initialDCNames = Set.fromList [prodName, trueName, falseName, litUnitName]

-- | prelude names for built-in datatypes
sigmaName :: TCName
sigmaName = "Sigma"
prodName :: DCName
prodName = "Prod"
boolName :: TCName
boolName = "Bool"
trueName :: DCName
trueName = "True"
falseName :: DCName
falseName = "False"
tyUnitName :: TCName
tyUnitName = "Unit"
litUnitName :: DCName
litUnitName = "()"

-- * Auxiliary functions on syntax
-- | empty set of constructor names
emptyConstructorNames :: ConstructorNames
emptyConstructorNames = ConstructorNames initialTCNames initialDCNames
