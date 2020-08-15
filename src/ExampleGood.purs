module ExampleGood where

import Prelude

import Data.Tuple.Nested ((/\))
import React.Basic.Hooks (Render, useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)

unsafeCoerceHook
  :: forall startingHooks oldEndingHooks newEndingHooks a
   . Render startingHooks oldEndingHooks a
  -> Render startingHooks newEndingHooks a
unsafeCoerceHook = unsafeCoerce

foreign import data UseFoo :: Type -> Type

useFoo :: forall hooks. Render hooks (UseFoo hooks) Int
useFoo = unsafeCoerceHook React.do
  hello /\ setHello <- useState 0
  bye /\ setBye <- useState ""
  pure 100

foreign import data UseBar :: Type -> Type

useBar :: forall hooks. Render hooks (UseBar hooks) Int
useBar = unsafeCoerceHook React.do
  hello /\ setHello <- useState 0
  bye /\ setBye <- useState ""
  pure 100

-- Note that the following code does not work.  If we used _ in the above
-- definitions, then this would work.  Preventing this is a good reason to use
-- unsafeCoerceHook.
--
-- useExample = React.do
--   if true
--     then useFoo
--     else useBar
--
--
-- Note that even though `unsafeCoerceHook` is "unsafe", code like the
-- following is still rejected by the compiler
--
-- foreign import data UseExample :: Type -> Type
--
-- useExample2 :: forall hooks. Render hooks (UseExample hooks) Int
-- useExample2 = unsafeCoerceHook
--   if true
--     then React.do
--       _ <- useState 0
--       pure 100
--     else React.do
--       _ <- useState "hello"
--       pure 100
