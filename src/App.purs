-- This is a PureScript implementation of
-- https://codesandbox.io/s/github/the-road-to-learn-react/hacker-stories/tree/hs/Inline-Handler-in-JSX?file=/src/App.js

module App where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat (json)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Prisms (_Array, _Number, _Object, _String)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Either (Either(Left, Right))
import Data.Int (fromString)
import Data.Lens (Fold', (^?), (^.), (^..), to, traversed)
import Data.Lens.Index (ix)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Nullable (null, toMaybe)
import Record (merge, nub, union)
import Data.String as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Timer (setTimeout)
import Foreign.Object (Object)
import Prim.Row (class Lacks, class Nub, class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_, targetValue)
import React.Basic.Hooks (Hook, JSX, ReactChildren, ReactComponent, UseEffect, UseState, coerceHook, component, element, fragment, keyed, mkReducer, reactChildrenFromArray, reactChildrenToArray, reactComponent, reactComponentWithChildren, readRefMaybe, useEffect, useReducer, useRef, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Web.DOM.Node (Node)
import Web.HTML.HTMLElement (focus, fromNode)

-------------
-- Helpers --
-------------

class (Union inputProps optProps x, Nub x allProps) <=
  InputProps inputProps optProps allProps x

instance inputPropsC :: (Union inputProps optProps x, Nub x allProps) =>
  InputProps inputProps optProps allProps x

liftMaybe :: forall m a. Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT <<< pure

---------
-- App --
---------

type Story = { title :: String, url :: String, author :: String, objectId :: Int }

jsonToStory :: Json -> Maybe Story
jsonToStory storyJson = do
  storyObj <- storyJson ^? _Object
  title <- storyObj ^? ix "title" <<< _String
  url <- storyObj ^? ix "url" <<< _String
  author <- storyObj ^? ix "author" <<< _String
  maybeObjectId <- storyObj ^? ix "objectID" <<< _String <<< to fromString
  objectId <- maybeObjectId
  pure ({ title, url, author, objectId } :: Story)

apiEndpoint :: String
apiEndpoint = "https://hn.algolia.com/api/v1/search?query="

newtype UseSemiPersistentState hooks
    = UseSemiPersistentState (UseEffect (String /\ String) (UseState String hooks))

derive instance newtypeUseSemiPersistentState :: Newtype (UseSemiPersistentState hooks) _

useSemiPersistentState
  :: String
  -> String
  -> Hook
      UseSemiPersistentState
      (String /\ ((String -> String) -> Effect Unit))
useSemiPersistentState key initialState = coerceHook React.do
  value /\ setValue <- useState initialState

  useEffect (value /\ key) $ do
    pure mempty

  pure (value /\ setValue)

data StoriesAction
  = StoriesFetchInit
  | StoriesFetchSuccess (Array Story)
  | StoriesFetchFailure
  | RemoveStory Story

storiesReducerFun
  :: { data :: Array Story, isLoading :: Boolean, isError :: Boolean }
  -> StoriesAction
  -> { data :: Array Story, isLoading :: Boolean, isError :: Boolean }
storiesReducerFun state =
  case _ of
    StoriesFetchInit -> state { isLoading = true, isError = false }
    StoriesFetchSuccess newStories ->
      state { isLoading = false, isError = false, data = newStories }
    StoriesFetchFailure -> state { isLoading = false, isError = true }
    RemoveStory storyToRemove ->
      state
        { data =
            Array.filter
              (\story -> storyToRemove.objectId /= story.objectId)
              state.data
        }

app :: Effect (ReactComponent {})
app = do
  inputWithLabel <- makeInputWithLabel
  list <- makeList
  storiesReducer <- mkReducer storiesReducerFun

  reactComponent "App" \props -> React.do
    searchTerm /\ setSearchTerm <- useSemiPersistentState "search" "Re"
    stories /\ dispatchStories <-
      useReducer
        { data: [], isLoading: false, isError: false }
        storiesReducer

    useAff unit $ do
      liftEffect $ dispatchStories StoriesFetchInit
      eitherResp <- get json (apiEndpoint <> "react")
      case eitherResp of
        Right resp -> do
          let maybeObj = resp.body ^? _Object
              (maybeStories :: List (Maybe Story)) =
                maybeObj ^.. traversed <<< ix "hits" <<< _Array <<< traversed <<< to jsonToStory
              stories' = mapMaybe identity $ toUnfoldable maybeStories
          liftEffect $ dispatchStories (StoriesFetchSuccess stories')
        Left _ ->
          liftEffect $ dispatchStories StoriesFetchFailure

    let handleRemoveStory story =
          dispatchStories (RemoveStory story)

        handleSearch eventTargetValue =
          setSearchTerm \_ -> fromMaybe "" eventTargetValue

        searchedStories =
          Array.filter
            (\story -> String.contains (Pattern searchTerm) story.title)
            stories.data

    pure $
      R.div_
        [ R.h1_ [ R.text "My Hacker Stories" ]
        , inputWithLabel
            { children:
                  [ R.text "Search:" ]
            , id: "search"
            , value: searchTerm
            , onInputChange: handleSearch
            , isFocused: true
            }
        , R.hr {}
        , if stories.isError then R.p_ [ R.text "Something went wrong..." ] else mempty
        , if stories.isLoading
            then R.p_ [R.text "Loading ..."]
            else
              list
                { list: searchedStories
                , onRemoveItem: handleRemoveStory
                }
        ]

type PropsInputWithLabel =
  ( children :: Array JSX
  , id :: String
  , isFocused :: Boolean
  , onInputChange :: Maybe String -> Effect Unit
  , value :: String
  | PropsInputWithLabelOpt
  )
type PropsInputWithLabelOpt = (type_ :: String)

makeInputWithLabel
  :: forall props x
   . InputProps props PropsInputWithLabelOpt PropsInputWithLabel x
  => Effect (Record props -> JSX)
makeInputWithLabel =
  component "InputWithLabel" \props_ -> React.do
    let def = { type_: "text" }
        { children, id, isFocused, onInputChange, type_, value } = merge props_ def

    inputRef <- useRef null

    useEffect isFocused $
      guard isFocused $ do
        void $ runMaybeT $ do
          (node :: Node) <- MaybeT $ readRefMaybe inputRef
          htmlElem <- liftMaybe $ fromNode node
          lift (focus htmlElem)
        pure (pure unit)

    pure $
      fragment
        [ R.label
            { htmlFor: id
            , children: children
            }
        , R.text "&nbsp;"
        , R.input
            { id
            , onChange: capture targetValue onInputChange
            , ref: inputRef
            , type: type_
            , value
            }
        ]

type PropsList =
  ( list :: Array Story
  , onRemoveItem :: Story -> Effect Unit
  )

makeList :: Effect (Record PropsList -> JSX)
makeList = do
  item <- makeItem
  component "List" \{list, onRemoveItem} -> React.do
    let items =
          map
            (\story ->
              keyed
                (show story.objectId)
                (item { item: story, onRemoveItem })
            )
            list
    pure (fragment items)

type PropsItem =
  ( item :: Story
  , onRemoveItem :: Story -> Effect Unit
  )

makeItem :: Effect (Record PropsItem -> JSX)
makeItem = do
  component "Item" \{item, onRemoveItem} -> React.do
    pure $
      R.div_
        [ R.span_
            [ R.a
                { href: item.url
                , children: [ R.text item.title ]
                }
            ]
        , R.span_ [ R.text item.author ]
        , R.span_
            [ R.button
                { type: "button"
                , onClick: capture_ (onRemoveItem item)
                , children: [ R.text "Dismiss" ]
                }
            ]
        ]
