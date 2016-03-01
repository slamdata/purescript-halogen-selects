module SlamData.Halogen.Select.Cascade.Component
  (
    cascadeSelect
  , Query(..)
  , State()
  , Option(..)
  , SelectKey(..)
  , initialState
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.MonadPlus (guard)

import Data.Functor (($>))
import Data.Lens (LensP(), lens, (%~))
import Data.Foldable as F
import Data.Map as Map
import Data.Tuple as Tpl
import Data.Maybe as M
import Data.Set as Set
import Data.List as L

import Halogen hiding (Prop())
import Halogen.HTML.Core (className, ClassName())
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3 as B

cascadeFormClass :: ClassName
cascadeFormClass = className "cascade-form"

cascadeNewSelectClass :: ClassName
cascadeNewSelectClass = className "cascade-new-select"

cascadeSelectClass :: ClassName
cascadeSelectClass = className "cascade-select"

newtype SelectKey = SelectKey String
runSelectKey :: SelectKey -> String
runSelectKey (SelectKey s) = s

instance eqSelectKey :: Eq SelectKey where
  eq (SelectKey s) (SelectKey s') = s == s'

instance ordSelectKey :: Ord SelectKey where
  compare (SelectKey s) (SelectKey s') = compare s s'

instance showSelectkey :: Show SelectKey where
  show (SelectKey s) = "(SelectKey " <> s <> ")"

newtype Option v =
  Option { key :: SelectKey
         , value :: v
         }

runOption :: forall v. Option v -> { key :: SelectKey, value :: v }
runOption (Option r) = r

instance eqOption :: Eq (Option v) where
  eq (Option {key = k}) (Option {key = k'}) = k == k'

instance ordOption :: Ord (Option v) where
  compare (Option {key = k}) (Option {key = k'}) = compare k k'

instance showOption :: (Show v) => Show (Option v) where
  show (Option {key, value}) =
    "Option {key = " <> show key <> ", value = " <> show value <> "}"

type State v =
  {
    options :: Set.Set (Option v)
  , selected :: Map.Map Int (Option v)
  , keyMap :: Map.Map SelectKey (Option v)
  }

_options :: forall a r. LensP {options :: a|r} a
_options = lens _.options _{options = _}

_selected :: forall a r. LensP {selected :: a|r} a
_selected = lens _.selected _{selected = _}

initialState :: forall v. Array v -> (v -> SelectKey) -> State v
initialState vs mkKey =
  {
    selected: Map.empty
  , options: Set.fromFoldable opts
  , keyMap: Map.fromFoldable $ map optToTuple opts
  }
  where
  mkOpt :: v -> Option v
  mkOpt v = Option {value: v, key: mkKey v}

  opts :: Array (Option v)
  opts = map mkOpt vs

  optToTuple :: Option v -> Tpl.Tuple SelectKey (Option v)
  optToTuple opt@(Option {key}) = Tpl.Tuple key opt

data Query v a
  = Selected Int String a
  | Remove Int a
  | GetSelected (Array v -> a)

type CascadeSelectDSL v e = ComponentDSL (State v) (Query v) (Aff e)

type CascadeSelectConfig =
  {
    inviteMessage :: String
  }

cascadeSelect
  :: forall v e
   . CascadeSelectConfig
  -> Component (State v) (Query v) (Aff e)
cascadeSelect cfg = component (render cfg) eval

type RenderAccum v =
  {
    selectable :: Set.Set (Option v)
  , html :: Array (ComponentHTML (Query v))
  }

initialAccum :: forall v. State v -> RenderAccum v
initialAccum state =
  {
    selectable: state.options
  , html: [ ]
  }


render
  :: forall v
   . CascadeSelectConfig
  -> State v
  -> ComponentHTML (Query v)
render cfg state =
  H.form [ P.classes [ cascadeFormClass ] ]
    (
      htmlAndSelectable.html
      <> newSelect
    )
  where
  htmlAndSelectable :: RenderAccum v
  htmlAndSelectable =
    F.foldl foldFn (initialAccum state)
      $ Map.toList state.selected

  foldFn :: RenderAccum v -> Tpl.Tuple Int (Option v) -> RenderAccum v
  foldFn {selectable, html} (Tpl.Tuple inx opt) =
    { html: html <> renderSelect inx opt selectable
    , selectable: Set.delete opt selectable
    }

  renderSelect
    :: Int -> Option v -> Set.Set (Option v) -> Array (ComponentHTML (Query v))
  renderSelect inx selected choices =
    [ H.div [ P.classes [ B.inputGroup, cascadeSelectClass ] ]
      [
        H.select [ P.classes [ B.formControl ]
                 , E.onValueChange (E.input (Selected inx))
                 ]
        $ F.foldMap (oneOption $ M.Just selected)
          $ L.sort $ Set.toList choices
      , H.div [ P.classes [ B.inputGroupBtn ] ]
        [
          H.button [ E.onClick (E.input_ (Remove inx))
                   , P.buttonType P.ButtonButton
                   , P.classes [ B.btn, B.btnDefault ]
                   ]
          [ H.i [ P.classes [ B.glyphicon, B.glyphiconRemove ] ] [ ] ]
        ]
      ]
    ]

  oneOption :: M.Maybe (Option v) -> Option v -> Array (ComponentHTML (Query v))
  oneOption mbSelected (Option choice) =
    let
      isSelected =
        mbSelected
          <#> runOption
          <#> _.key
          <#> eq choice.key
          # M.fromMaybe false
    in
      [ H.option [ P.selected isSelected ]
        [ H.text $ runSelectKey choice.key ]
      ]

  newSelect :: Array (ComponentHTML (Query v))
  newSelect =
    guard (not $ Set.isEmpty htmlAndSelectable.selectable)
    $> (H.select [ E.onValueChange (E.input (Selected nextInx))
                 , P.classes [ B.formControl, cascadeNewSelectClass ]
                 ]
        $ [ H.option_ [H.text cfg.inviteMessage ] ]
        <> (F.foldMap (oneOption M.Nothing)
              $ L.sort $ Set.toList htmlAndSelectable.selectable))

  nextInx :: Int
  nextInx =
    M.fromMaybe zero $ add one $ F.maximum $ Map.keys state.selected


eval :: forall v e. Natural (Query v) (CascadeSelectDSL v e)
eval (Selected inx str next) = do
  state <- get
  F.for_ (Map.lookup (SelectKey str) state.keyMap) \opt -> do
    modify (_selected %~
            Map.fromList
            <<< F.foldMap (\tpl -> guard (tpl # Tpl.snd # eq opt # not) $> tpl)
            <<< Map.toList
            )
    modify (_selected %~ Map.insert inx opt)
  pure next
eval (Remove inx next) =
  modify (_selected %~ Map.delete inx) $> next
eval (GetSelected continue) =
  gets
    $ _.selected
    >>> Map.values
    >>> F.foldMap (runOption >>> _.value >>> pure)
    >>> continue
