{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Description where

import Servant.API
import GHC.Generics (D1, Meta (..), Rep)
import GHC.TypeLits (AppendSymbol, Symbol)

type family TypeName (x :: *) :: Symbol where
    TypeName Int  = "Int"
    TypeName String = "String"
    TypeName x    = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: *) :: Symbol where
    GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))