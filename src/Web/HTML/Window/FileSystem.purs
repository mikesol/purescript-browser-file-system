module Web.HTML.Window.FileSystem where

import Prelude

import Control.Promise (Promise, toAffE)
import ConvertableOptions (class Defaults, defaults)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Data.Tuple.Nested ((/\), type (/\))
import Foreign.Object (Object, fromFoldable)
import Simple.JSON as JSON
import Web.File.File (File)
import Web.File.FileSystemWritableFileStream (FileSystemWritableFileStream)

data RRW = Read | Readwrite
data Permission = Granted | Denied | Prompt

instance JSON.WriteForeign RRW where
  writeImpl = JSON.writeImpl <<< case _ of
    Read -> "read"
    Readwrite -> "readwrite"

class FileSystemHandle fsh where
  kind :: fsh -> Effect String
  name :: fsh -> Effect String
  isSameEntry :: fsh -> fsh -> Effect Boolean
  queryPermission :: fsh -> { mode :: RRW } -> Aff Permission
  requestPermission :: fsh -> { mode :: RRW } -> Aff Permission

foreign import kind_FileSystemFileHandle
  :: FileSystemFileHandle -> Effect String

foreign import name_FileSystemFileHandle
  :: FileSystemFileHandle -> Effect String

foreign import isSameEntry_FileSystemFileHandle
  :: FileSystemFileHandle -> FileSystemFileHandle -> Effect Boolean

foreign import queryPermission_FileSystemFileHandle
  :: FileSystemFileHandle -> Foreign -> Effect (Promise String)

foreign import requestPermission_FileSystemFileHandle
  :: FileSystemFileHandle -> Foreign -> Effect (Promise String)

strToPermission :: String -> Effect Permission
strToPermission "granted" = pure Granted
strToPermission "denied" = pure Denied
strToPermission "prompt" = pure Prompt
strToPermission unknown = throw $ "Unknown permission: " <> unknown

instance FileSystemHandle FileSystemFileHandle where
  kind = kind_FileSystemFileHandle
  name = name_FileSystemFileHandle
  isSameEntry = isSameEntry_FileSystemFileHandle
  queryPermission fsh mode =
    (toAffE $ queryPermission_FileSystemFileHandle fsh (JSON.writeImpl mode))
      >>= liftEffect <<< strToPermission
  requestPermission fsh mode =
    (toAffE $ requestPermission_FileSystemFileHandle fsh (JSON.writeImpl mode))
      >>= liftEffect <<< strToPermission

data FileSystemFileHandle

foreign import getFile_ :: FileSystemFileHandle -> Effect (Promise File)

getFile :: FileSystemFileHandle -> Aff File
getFile = toAffE <<< getFile_

foreign import createWritable_
  :: FileSystemFileHandle -> Effect (Promise FileSystemWritableFileStream)

createWritable :: FileSystemFileHandle -> Aff FileSystemWritableFileStream
createWritable = toAffE <<< createWritable_

type ShowOpenFilePickerOptional =
  ( multiple :: Boolean
  , excludeAcceptAllOption :: Boolean
  , types ::
      Array
        { description :: Maybe String
        , accept :: Object (Array String)
        }
  )

type ShowOpenFilePickerAll = (| ShowOpenFilePickerOptional)

defaultShowOpenFilePickerOptions :: { | ShowOpenFilePickerOptional }
defaultShowOpenFilePickerOptions =
  { multiple: false
  , excludeAcceptAllOption: false
  , types: []
  }

foreign import showOpenFilePicker_
  :: Foreign
  -> Effect (Promise (Array FileSystemFileHandle))

showOpenFilePicker
  :: forall provided
   . Defaults { | ShowOpenFilePickerOptional }
       { | provided }
       { | ShowOpenFilePickerAll }
  => { | provided }
  -> Aff (Array FileSystemFileHandle)
showOpenFilePicker provided = toAffE
  $ showOpenFilePicker_ (JSON.writeImpl all)
  where
  all :: { | ShowOpenFilePickerAll }
  all = defaults defaultShowOpenFilePickerOptions provided

--
type ShowSaveFilePickerOptional =
  ( excludeAcceptAllOption :: Boolean
  , suggestedName :: Maybe String
  , types ::
      Array
        { description :: Maybe String
        , accept :: Object (Array String)
        }
  )

type ShowSaveFilePickerAll = (| ShowSaveFilePickerOptional)

defaultShowSaveFilePickerOptions :: { | ShowSaveFilePickerOptional }
defaultShowSaveFilePickerOptions =
  { suggestedName: Nothing
  , excludeAcceptAllOption: false
  , types: []
  }

foreign import showSaveFilePicker_
  :: Foreign
  -> Effect (Promise (Array FileSystemFileHandle))

showSaveFilePicker
  :: forall provided
   . Defaults { | ShowSaveFilePickerOptional }
       { | provided }
       { | ShowSaveFilePickerAll }
  => { | provided }
  -> Aff (Array FileSystemFileHandle)
showSaveFilePicker provided = toAffE
  $ showSaveFilePicker_ (JSON.writeImpl all)
  where
  all :: { | ShowSaveFilePickerAll }
  all = defaults defaultShowSaveFilePickerOptions provided

--

foreign import kind_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle -> Effect String

foreign import name_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle -> Effect String

foreign import isSameEntry_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle
  -> FileSystemDirectoryHandle
  -> Effect Boolean

foreign import queryPermission_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle
  -> Foreign
  -> Effect (Promise String)

foreign import requestPermission_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle -> Foreign -> Effect (Promise String)

instance FileSystemHandle FileSystemDirectoryHandle where
  kind = kind_FileSystemDirectoryHandle
  name = name_FileSystemDirectoryHandle
  isSameEntry = isSameEntry_FileSystemDirectoryHandle
  queryPermission fsh mode =
    ( toAffE $ queryPermission_FileSystemDirectoryHandle fsh
        (JSON.writeImpl mode)
    ) >>= liftEffect <<< strToPermission
  requestPermission fsh mode =
    ( toAffE $ requestPermission_FileSystemDirectoryHandle fsh
        (JSON.writeImpl mode)
    ) >>= liftEffect <<< strToPermission

data FileSystemDirectoryHandle

foreign import showDirectoryPicker_
  :: Effect (Promise (Array FileSystemDirectoryHandle))

showDirectoryPicker :: Aff (Array FileSystemDirectoryHandle)
showDirectoryPicker = toAffE showDirectoryPicker_

data FD = File FileSystemFileHandle | Directory FileSystemDirectoryHandle

foreign import entries_
  :: (FileSystemFileHandle -> FD)
  -> (FileSystemDirectoryHandle -> FD)
  -> (String -> FD -> String /\ FD)
  -> FileSystemDirectoryHandle
  -> Effect (Promise (Array (String /\ FD )))

entries :: FileSystemDirectoryHandle -> Aff (Object FD)
entries = map fromFoldable <<< toAffE <<< entries_ File Directory (/\)

foreign import getFileHandle_
  :: FileSystemDirectoryHandle
  -> String
  -> Foreign
  -> Effect (Promise FileSystemFileHandle)

getFileHandle
  :: FileSystemDirectoryHandle
  -> String
  -> { create :: Boolean }
  -> Aff FileSystemFileHandle
getFileHandle fsh name create =
  toAffE $ getFileHandle_ fsh name (JSON.writeImpl create)

foreign import getDirectoryHandle_
  :: FileSystemDirectoryHandle
  -> String
  -> Foreign
  -> Effect (Promise FileSystemDirectoryHandle)

getDirectoryHandle
  :: FileSystemDirectoryHandle
  -> String
  -> { create :: Boolean }
  -> Aff FileSystemDirectoryHandle
getDirectoryHandle fsh name create =
  toAffE $ getDirectoryHandle_ fsh name (JSON.writeImpl create)