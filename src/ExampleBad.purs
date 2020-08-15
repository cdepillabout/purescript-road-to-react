module ExampleBad where

import Prelude

import Data.Tuple.Nested ((/\))
import React.Basic.Hooks (Render, useState)
import React.Basic.Hooks as React

useFoo :: Render _ _ Int
useFoo = React.do
  hello /\ setHello <- useState 0
  bye /\ setBye <- useState ""
  pure 100

useBar :: Render _ _ Int
useBar = React.do
  hello /\ setHello <- useState 0
  bye /\ setBye <- useState ""
  pure 100

-- Note that the following code does compile.  However, this should be an
-- error, since useFoo and useBar are logically different Hooks.  They should
-- not be interchanged in different conditional branches.
useExample = React.do
  if true
    then useFoo
    else useBar
