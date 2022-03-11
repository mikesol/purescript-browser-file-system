module Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj, match)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, parallel, sequential)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Foreign.Object (values)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Storybook (Stories, runStorybook, proxy)
import Simple.JSON (writeJSON)
import Type.Proxy (Proxy(..))
import Web.File.File (toBlob)
import Web.File.FileReader (FileReader, fileReader, readAsText, result)
import Web.HTML.Window.FileSystem (FD(..), entries, getFile, showDirectoryPicker, showOpenFilePicker)

foreign import waitTillDone_
  :: (Error -> Effect Unit)
  -> (Unit -> Effect Unit)
  -> FileReader
  -> Effect Unit

waitTillDone :: FileReader -> Aff Unit
waitTillDone fr = makeAff \f -> do
  waitTillDone_ (Left >>> f) (Right >>> f) fr
  mempty

component0
  :: forall query input output m
   . MonadEffect m
  => MonadAff m
  => Boolean
  -> H.Component query input output m
component0 multiple =
  H.mkComponent
    { initialState: \_ -> { file: Nothing }
    , render: \{ file } -> case file of
        Nothing -> HH.button
          [ HE.onClick (const $ inj (Proxy :: _ "selectFile") unit) ]
          [ HH.text "Select file" ]

        Just f -> HH.div_ [ HH.text f ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = match
            { selectFile: \(_ :: Unit) -> do
                txts <- H.liftAff do
                  fsh <- showOpenFilePicker { multiple }
                  sequential
                    ( fsh # traverse \ff -> parallel do
                        f <- getFile ff
                        r <- liftEffect fileReader
                        liftEffect $ readAsText (toBlob f) r
                        waitTillDone r
                        liftEffect (writeJSON <$> result r)
                    )
                H.modify_ _ { file = Just (intercalate "\n" txts) }
            }
        }
    }

component1
  :: forall query input output m
   . MonadEffect m
  => MonadAff m
  => H.Component query input output m
component1 =
  H.mkComponent
    { initialState: \_ -> { file: Nothing }
    , render: \{ file } -> case file of
        Nothing -> HH.button
          [ HE.onClick (const $ inj (Proxy :: _ "selectDir") unit) ]
          [ HH.text "Select directory" ]

        Just f -> HH.div_ [ HH.text f ]
    , eval: H.mkEval $ H.defaultEval
        { handleAction = match
            { selectDir: \(_ :: Unit) -> do
                txts <- H.liftAff do
                  fsh <- showDirectoryPicker
                  e <- entries fsh
                  let
                    vals = filterMap
                      ( case _ of
                          File f -> Just f
                          _ -> Nothing
                      )
                      (values e)
                  sequential
                    ( vals # traverse \ff -> parallel do
                        f <- getFile ff
                        r <- liftEffect fileReader
                        liftEffect $ readAsText (toBlob f) r
                        waitTillDone r
                        liftEffect (writeJSON <$> result r)
                    )

                H.modify_ _ { file = Just (intercalate "\n" $ txts) }
            }
        }
    }

stories
  :: forall m. MonadEffect m => MonadAff m => MonadThrow Error m => Stories m
stories = Object.fromFoldable
  [ "pick file" /\ (proxy $ component0 false)
  , "pick files" /\ (proxy $ component0 true)
  , "pick directory" /\ proxy component1
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Just (HH.text "browser-file-system")
      }