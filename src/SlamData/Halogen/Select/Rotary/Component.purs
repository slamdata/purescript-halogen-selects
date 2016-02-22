module SlamData.Halogen.Select.Rotary.Component
  (
    rotarySelect
  , module SlamData.Halogen.Select.Rotary.Component.State
  , module SlamData.Halogen.Select.Rotary.Component.Query
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Maybe.Trans as Mt
import Control.Coroutine.Aff (produce)
import Control.Coroutine.Stalling as SCR

import Data.Array as Arr
import Data.Circular as Cr
import Data.Either as E
import Data.Date (Now())
import Data.Functor (($>))
import Data.Functor.Eff (liftEff)
import Data.Lens ((.~), (?~))
import Data.Maybe as M
import Data.Foldable as F
import Data.StrMap as Sm
import Data.Int as Int

import CSS.Display (display, inlineBlock)
import CSS.Display (position, relative)
import CSS.Geometry (width, left, marginLeft)
import CSS.Overflow (overflow, hidden)
import CSS.Selector
  (Selector(), Predicate(AttrVal), Refinement(..), (**), (##))
import CSS.Size (px)
import CSS.String (fromString)
import CSS.Stylesheet (CSS(), (?))
import CSS.TextAlign (textAlign, center)

import Halogen hiding (Prop())
import Halogen.HTML.Core (Prop(..), attrName, className, ClassName())
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties.Indexed (IProp())
import Halogen.Query.EventSource (EventSource(..))

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Window as Win
import DOM.HTML.Types as Ht
import DOM.Event.EventTarget as Etr
import DOM.Event.EventTypes as Etp

import SlamData.Halogen.Select.Utils.Random (genKey)
import SlamData.Halogen.Select.Utils.DOM
  (getComputedStyle, getClientRects, getScreen)

import SlamData.Halogen.Select.Rotary.Component.State
import SlamData.Halogen.Select.Rotary.Component.Query

type AffSel e =
  Aff ( dom :: DOM
      , random :: RANDOM
      , now :: Now
      , avar :: AVAR |e)
type RotarySelectorDSL r e = ComponentDSL (State r) (Query r) (AffSel e)

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
    itemRender :: M.Maybe (Option r -> ComponentHTML (Query r))
  , itemWidth :: Number
  , visibleItemCount :: M.Maybe Number
  }

rotarySelect
  :: forall r e
   . RotarySelectorConfig r
  -> Component (State r) (Query r) (AffSel e)
rotarySelect cfg = component (render cfg) (eval cfg)

render
  :: forall r
   . RotarySelectorConfig r
  -> (State r)
  -> ComponentHTML (Query r)
render cfg state =
  H.div wrapperAttrs
    [ H.div [ E.onMouseDown (\evt -> E.preventDefault
                                       $> (action $ StartDragging evt.clientX))
            , P.initializer (\el -> action $ Init el)
            , P.classes [ draggedClass ]
            ]
      ( stls <> content)
    ]
  where
  wrapperAttrs =
    [ P.classes [ wrapperClass ] ]
    <> F.foldMap (dataRotaryKey >>> Arr.singleton) state.key

  content :: Array (ComponentHTML (Query r))
  content =
    map itemRender state.displayedItems

  itemRender :: Option r -> ComponentHTML (Query r)
  itemRender s =
    H.div [ P.classes [ itemClass ] ]
      $ pure
      $ case cfg.itemRender of
        M.Just fn -> fn s
        M.Nothing -> (runOption >>> _.label >>> H.text) s

  stls :: Array (ComponentHTML (Query r))
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
    el <- Mt.MaybeT $ gets _.element
    st <- liftEff $ getComputedStyle el
    Mt.MaybeT
      $ pure
      $ Sm.lookup "marginLeft" st
      <#> Global.readFloat

getElementOffset
  :: forall e r
   . RotarySelectorDSL r e Number
getElementOffset =
  M.fromMaybe zero <$> Mt.runMaybeT do
    el <- Mt.MaybeT $ gets _.element
    rlst <- liftEff $ getClientRects el
    hd <- Mt.MaybeT $ pure $ Arr.head rlst
    pure hd.left

setDisplayedItems
  :: forall r e
   . RotarySelectorConfig r
  -> Cr.Circular Array (Option r)
  -> RotarySelectorDSL r e Unit
setDisplayedItems cfg circ = do
  screenWidth <- liftEff $ getScreen <#> _.width
  let
    iwidth = Int.floor cfg.itemWidth
    ilen = Cr.lengthCircular circ
    itemsOnScreen = screenWidth / iwidth + one
    repeats = draggableScreens * itemsOnScreen / ilen
  modify $ _displayedItems .~ Cr.samples repeats circ


eval
  :: forall r e
   . RotarySelectorConfig r
  -> Natural (Query r) (RotarySelectorDSL r e)
eval cfg (Init el next) = do
  modify $ _element ?~ el
  key <- genKey
  modify $ _key ?~ key
  state <- get
  setDisplayedItems cfg state.items
  docTarget <-
    liftEff
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
      pure $ action $ StopDragging
    handleMouseMove e =
      pure $ action $ ChangePosition (evntify e).clientX
    handleAnimated e =
      pure $ action $ Animated
  subscribe $ eventSource attachMouseUp handleMouseUp
  subscribe $ eventSource attachMouseMove handleMouseMove
  subscribe $ eventSource attachAnimated handleAnimated


  screenWidth <- liftEff $ getScreen <#> _.width
  modify $ _constStyles .~ do
    let visibleCount = M.fromMaybe 2.0 cfg.visibleItemCount
    (wrapperSelector key) ? do
      width $ px $ visibleCount * cfg.itemWidth
      marginLeft $ px (-1.0 * visibleCount * cfg.itemWidth * 0.5)
      overflow hidden
      position relative
    (draggedSelector key) ? do
      let
        iwidth = Int.floor cfg.itemWidth
        ilen = Cr.lengthCircular state.items
        itemsOnScreen = screenWidth / iwidth + one
        draggedWidth =
          cfg.itemWidth * Int.toNumber (itemsOnScreen * draggableScreens)
        itemsOnLeftSideCount =
          draggableScreens * (itemsOnScreen / 2 / ilen)
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
  modify $ _visualState .~ (Dragging $ startedAt - mL)
  modify $ _position .~ startedAt
  modify updateStyles
  pure next
eval cfg (StopDragging next) = do
  visualState <- gets _.visualState
  case visualState of
    Dragging startedAt -> do
      mL <- getCurrentX
      position <- gets _.position
      let
        diff = position - startedAt
        pos =
          Int.floor
          $ ((if diff > 0.0 then 1.0 else -1.0) * cfg.itemWidth * 0.5)
          + diff
        finalMargin =
          Int.toNumber (pos / Int.floor cfg.itemWidth)  * cfg.itemWidth

      modify $ _visualState .~ Animating mL finalMargin
      modify updateStyles
    _ -> pure unit
  pure next
eval cfg (ChangePosition pos next) = do
  dragged <- gets isDragged
  when dragged do
    modify $ _position .~ pos
    modify updateStyles
  pure next
eval cfg (Animated next) = do
  state <- get
  modify $ _visualState .~ Staying
  curX <- getCurrentX
  modify $ _position .~ curX
  let
    items =
      Cr.shift (-1 * (Int.floor (curX / cfg.itemWidth))) state.items
  setDisplayedItems cfg items
  modify $ _items .~ items
  modify $ _position .~ zero
  modify updateStyles
  subscribe
    $ EventSource
    $ SCR.producerToStallingProducer
    $ produce \emit -> do
      emit $ E.Left $ action $ Selected $ Cr.pointed items
      emit $ E.Right unit
  pure next
eval cfg (GetSelected continue) = do
  state <- get
  pure $ continue $ Cr.pointed state.items
eval cfg (Selected _ next) = pure next
