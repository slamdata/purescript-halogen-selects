module SlamData.Halogen.Select.Rotary.Component
  (
    rotarySelect
  , RotarySelectorConfig
  , module S
  , module SlamData.Halogen.Select.Rotary.Component.Query
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Maybe.Trans as Mt
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR

import Data.Array as Arr
import Data.Circular as Cr
import Data.Date (Now)
import Data.Either as E
import Data.Foldable as F
import Data.Functor (($>))
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.Maybe as M
import Data.NaturalTransformation (Natural)
import Data.StrMap as Sm

import CSS.Display (display, inlineBlock, position, relative)
import CSS.Geometry (width, left, marginLeft)
import CSS.Overflow (overflow, hidden)
import CSS.Selector
  (Selector, Predicate(AttrVal), Refinement(..), (**), (##))
import CSS.Size (px)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, (?))
import CSS.TextAlign (textAlign, center)

import Halogen as H
import Halogen.HTML.Core (Prop(..), attrName, className, ClassName)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties.Indexed (IProp)
import Halogen.Query.EventSource (EventSource(..))

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp

import SlamData.Halogen.Select.Utils.Random (genKey)
import SlamData.Halogen.Select.Utils.DOM
  (getComputedStyle, getClientRects, getScreen)

import SlamData.Halogen.Select.Rotary.Component.State as S
import SlamData.Halogen.Select.Rotary.Component.Query (Query(..))

type AffSel e =
  Aff ( dom :: DOM
      , random :: RANDOM
      , now :: Now
      , avar :: AVAR |e)
type RotarySelectorDSL r e = H.ComponentDSL (S.State r) (Query r) (AffSel e)

wrapperClass :: ClassName
wrapperClass = className "rotary-selector-wrapper"

draggedClass :: ClassName
draggedClass = className "rotary-selector-dragged"

itemClass :: ClassName
itemClass = className "rotary-selector-item"

dataRotaryKey :: forall i r. String -> IProp r i
dataRotaryKey = Unsafe.Coerce.unsafeCoerce nonIndexed
  where
  nonIndexed :: String -> Prop i
  nonIndexed = Attr M.Nothing (attrName "data-rotarykey")

draggableScreens :: Int
draggableScreens = 3

type RotarySelectorConfig r =
  {
    itemRender :: M.Maybe (S.Option r -> H.ComponentHTML (Query r))
  , itemWidth :: Number
  , visibleItemCount :: M.Maybe Number
  }

rotarySelect
  :: forall r e
   . RotarySelectorConfig r
  -> H.Component (S.State r) (Query r) (AffSel e)
rotarySelect cfg =
  H.lifecycleComponent
    { render: render cfg
    , eval: eval cfg
    , initializer: M.Just (H.action Init)
    , finalizer: M.Nothing
    }

render
  :: forall r
   . RotarySelectorConfig r
  -> (S.State r)
  -> H.ComponentHTML (Query r)
render cfg state =
  HH.div wrapperAttrs
    [ HH.div
        [ HE.onMouseDown \evt ->
            HEH.preventDefault $> M.Just (H.action $ StartDragging evt.clientX)
        , HP.ref (H.action <<< SetElement)
        , HP.classes [ draggedClass ]
        ]
        $ stls <> content
    ]
  where
  wrapperAttrs =
    [ HP.classes [ wrapperClass ] ]
    <> F.foldMap (dataRotaryKey >>> Arr.singleton) state.key

  content :: Array (H.ComponentHTML (Query r))
  content =
    map itemRender state.displayedItems

  itemRender :: S.Option r -> H.ComponentHTML (Query r)
  itemRender s =
    HH.div [ HP.classes [ itemClass ] ]
      $ pure
      $ case cfg.itemRender of
        M.Just fn -> fn s
        M.Nothing -> (S.runOption >>> _.label >>> HH.text) s

  stls :: Array (H.ComponentHTML (Query r))
  stls =
    F.foldMap (Arr.singleton <<< CSS.stylesheet <<< mkStylesheet) state.key

  mkStylesheet :: String -> CSS
  mkStylesheet k = do
    state.constStyles
    (draggedSelector k) ? state.styles

wrapperSelector :: String -> Selector
wrapperSelector k =
  (fromString ".rotary-selector-wrapper")
  ## (Refinement [ AttrVal "data-rotarykey" k ])

draggedSelector :: String -> Selector
draggedSelector k =
  (wrapperSelector k)
  ** (fromString ".rotary-selector-dragged")

itemSelector :: String -> Selector
itemSelector k =
  (draggedSelector k)
  ** (fromString ".rotary-selector-item")

getCurrentX
  :: forall e r
   . RotarySelectorDSL r e Number
getCurrentX =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ H.gets _.element
    st <- H.fromEff $ getComputedStyle el
    Mt.MaybeT
      $ pure
      $ Sm.lookup "marginLeft" st
      <#> Global.readFloat

getElementOffset
  :: forall e r
   . RotarySelectorDSL r e Number
getElementOffset =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ H.gets _.element
    rlst <- H.fromEff $ getClientRects el
    hd <- Mt.MaybeT $ pure $ Arr.head rlst
    pure hd.left

setDisplayedItems
  :: forall r e
   . RotarySelectorConfig r
  -> Cr.Circular Array (S.Option r)
  -> RotarySelectorDSL r e Unit
setDisplayedItems cfg circ = do
  screenWidth <- H.fromEff $ getScreen <#> _.width
  let
    iwidth = Int.floor cfg.itemWidth
    ilen = Cr.lengthCircular circ
    itemsOnScreen = screenWidth / iwidth + one
    repeats = draggableScreens * itemsOnScreen / ilen
  H.modify $ S._displayedItems .~ Cr.samples repeats circ


eval
  :: forall r e
   . RotarySelectorConfig r
  -> Natural (Query r) (RotarySelectorDSL r e)
eval cfg (SetElement el next) = do
  H.modify (S._element .~ el)
  pure next
eval cfg (Init next) = do
  key <- genKey
  H.modify $ S._key ?~ key
  state <- H.get
  case state.element of
    M.Nothing -> pure unit
    M.Just el -> do
      setDisplayedItems cfg state.items
      docTarget <-
        H.fromEff
        $ window
        >>= Win.document
        <#> Ht.htmlDocumentToEventTarget
      offset <- getElementOffset
      let
        evntify :: forall a. a -> { clientX :: Number, clientY :: Number }
        evntify = Unsafe.Coerce.unsafeCoerce
        attachMouseUp f =
          Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
        attachMouseMove f =
          Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget
        attachAnimated f =
          Etr.addEventListener Etp.animationend (Etr.eventListener f) false
          $ Ht.htmlElementToEventTarget el
        handleMouseUp e =
          pure $ H.action $ StopDragging
        handleMouseMove e =
          pure $ H.action $ ChangePosition (evntify e).clientX
        handleAnimated e =
          pure $ H.action $ Animated
      H.subscribe $ H.eventSource attachMouseUp handleMouseUp
      H.subscribe $ H.eventSource attachMouseMove handleMouseMove
      H.subscribe $ H.eventSource attachAnimated handleAnimated
      screenWidth <- H.fromEff $ getScreen <#> _.width
      H.modify $ S._constStyles .~ do
        let visibleCount = M.fromMaybe 2.0 cfg.visibleItemCount
        (wrapperSelector key) ? do
          width $ px $ visibleCount * cfg.itemWidth
          overflow hidden
          position relative
        (draggedSelector key) ? do
          let
            iwidth = Int.floor cfg.itemWidth
            ilen = Cr.lengthCircular state.items
            itemsOnScreen =
              screenWidth / iwidth + one
            draggedWidth =
              cfg.itemWidth * Int.toNumber (itemsOnScreen * draggableScreens)
            itemsOnLeftSideCount =
              draggableScreens * (itemsOnScreen / 2 / ilen + one)
            halfWidth =
              cfg.itemWidth * Int.toNumber (ilen * itemsOnLeftSideCount)
            leftPosition =
              (visibleCount - one) / 2.0 * cfg.itemWidth - halfWidth
          position relative
          left $ px leftPosition
          marginLeft $ px 0.0
          width $ px $ draggedWidth
        (itemSelector key) ? do
          width $ px cfg.itemWidth
          display inlineBlock
          textAlign center
  pure next
eval cfg (StartDragging startedAt next) = do
  mL <- getCurrentX
  offset <- getElementOffset
  H.modify $ S._visualState .~ (S.Dragging $ startedAt - mL)
  H.modify $ S._position .~ startedAt
  H.modify S.updateStyles
  pure next
eval cfg (StopDragging next) = do
  visualState <- H.gets _.visualState
  case visualState of
    S.Dragging startedAt -> do
      mL <- getCurrentX
      position <- H.gets _.position
      let
        diff = position - startedAt
        pos =
          Int.floor
          $ ((if diff > 0.0 then 1.0 else -1.0) * cfg.itemWidth * 0.5)
          + diff
        finalMargin =
          Int.toNumber (pos / Int.floor cfg.itemWidth)  * cfg.itemWidth

      H.modify $ S._visualState .~ S.Animating mL finalMargin
      H.modify S.updateStyles
    _ -> pure unit
  pure next
eval cfg (ChangePosition pos next) = do
  dragged <- H.gets S.isDragged
  when dragged do
    H.modify $ S._position .~ pos
    H.modify S.updateStyles
  pure next
eval cfg (Animated next) = do
  state <- H.get
  H.modify $ S._visualState .~ S.Staying
  curX <- getCurrentX
  H.modify $ S._position .~ curX
  let
    items =
      Cr.shift (-1 * (Int.floor (curX / cfg.itemWidth))) state.items
  setDisplayedItems cfg items
  H.modify $ S._items .~ items
  H.modify $ S._position .~ zero
  H.modify S.updateStyles
  H.subscribe
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      emit $ E.Left $ H.action $ Selected $ Cr.pointed items
      emit $ E.Right unit
  pure next
eval cfg (GetSelected continue) = do
  state <- H.get
  pure $ continue $ Cr.pointed state.items
eval cfg (Selected _ next) = pure next
