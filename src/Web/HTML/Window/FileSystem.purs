module Web.HTML.Window.FileSystem
  ( FD(..)
  , FileSystemDirectoryHandle(..)
  , FileSystemFileHandle(..)
  , Permission(..)
  , RRW(..)
  , ShowOpenFilePickerAll
  , ShowOpenFilePickerOptional
  , ShowSaveFilePickerAll
  , ShowSaveFilePickerBase
  , ShowSaveFilePickerFull
  , ShowSaveFilePickerOptional
  , class FileSystemHandle
  , class Defaults
  , defaults
  , kind
  , name
  , isSameEntry
  , queryPermission
  , requestPermission
  , createWritable
  , entries
  , getDirectoryHandle
  , getFile
  , getFileHandle
  , showDirectoryPicker
  , showOpenFilePicker
  , showSaveFilePicker
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Exception (Error, try, throw)
import Foreign.Object (Object)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Web.File.File (File)
import Web.File.FileSystemWritableFileStream (FileSystemWritableFileStream)

class
  ConvertOptionsWithDefaults t defaults provided all
  | t -> defaults all where
  convertOptionsWithDefaults :: t -> defaults -> provided -> all

instance convertOptionsWithDefaultsRecord ::
  ( ConvertOptions t { | provided } provided'
  , Defaults { | defaults } provided' { | all }
  ) =>
  ConvertOptionsWithDefaults t { | defaults } { | provided } { | all } where
  convertOptionsWithDefaults t def =
    defaults def <<< convertOptions t

class ConvertOptions t i o | t -> o where
  convertOptions :: t -> i -> o

class ConvertOption t (p :: Symbol) i o | t p -> o where
  convertOption :: t -> Proxy p -> i -> o

class ConvertRecordOptions t (rl :: RowList Type) i o | t rl -> o where
  convertRecordOptions :: t -> Proxy rl -> i -> o

instance convertRecordOptionsNil ::
  ConvertRecordOptions t RowList.Nil { | r } (Builder {} {}) where
  convertRecordOptions _ _ _ = identity

instance convertRecordOptionsCons ::
  ( ConvertRecordOptions t rest { | r } (Builder { | i } { | o' })
  , ConvertOption t sym a b
  , Row.Cons sym a r' r
  , Row.Cons sym b o' o
  , Row.Lacks sym o'
  , IsSymbol sym
  ) =>
  ConvertRecordOptions t
    (RowList.Cons sym a rest)
    { | r }
    (Builder { | i } { | o }) where
  convertRecordOptions t _ r =
    Builder.insert (Proxy :: _ sym)
      (convertOption t (Proxy :: _ sym) (Record.get (Proxy :: _ sym) r))
      <<< convertRecordOptions t (Proxy :: _ rest) r

instance convertOptionsRecord ::
  ( RowToList i rl
  , ConvertRecordOptions t rl { | i } (Builder {} { | o })
  ) =>
  ConvertOptions t { | i } { | o } where
  convertOptions t i = Builder.buildFromScratch $ convertRecordOptions t
    (Proxy :: _ rl)
    i

class Defaults defaults provided all | defaults provided -> all where
  defaults :: defaults -> provided -> all

instance defaultsRecord ::
  ( Row.Union provided defaults all'
  , Row.Nub all' all
  ) =>
  Defaults { | defaults } { | provided } { | all } where
  defaults = flip Record.merge

data RRW = Read | Readwrite
data Permission = Granted | Denied | Prompt

rrwToString :: RRW -> String
rrwToString Read = "read"
rrwToString Readwrite = "readwrite"

class FileSystemHandle fsh where
  kind :: fsh -> Effect String
  name :: fsh -> Effect String
  isSameEntry :: fsh -> fsh -> Effect Boolean
  queryPermission
    :: fsh
    -> { mode :: RRW }
    -> (Error -> Effect Unit)
    -> (Permission -> Effect Unit)
    -> Effect Unit
  requestPermission
    :: fsh
    -> { mode :: RRW }
    -> (Error -> Effect Unit)
    -> (Permission -> Effect Unit)
    -> Effect Unit

foreign import kind_FileSystemFileHandle
  :: FileSystemFileHandle -> Effect String

foreign import name_FileSystemFileHandle
  :: FileSystemFileHandle -> Effect String

foreign import isSameEntry_FileSystemFileHandle
  :: FileSystemFileHandle -> FileSystemFileHandle -> Effect Boolean

foreign import queryPermission_FileSystemFileHandle
  :: FileSystemFileHandle
  -> { mode :: String }
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import requestPermission_FileSystemFileHandle
  :: FileSystemFileHandle
  -> { mode :: String }
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

strToPermission :: String -> Effect Permission
strToPermission "granted" = pure Granted
strToPermission "denied" = pure Denied
strToPermission "prompt" = pure Prompt
strToPermission unknown = throw $ "Unknown permission: " <> unknown

instance FileSystemHandle FileSystemFileHandle where
  kind = kind_FileSystemFileHandle
  name = name_FileSystemFileHandle
  isSameEntry = isSameEntry_FileSystemFileHandle
  queryPermission fsh { mode } fe fa = queryPermission_FileSystemFileHandle fsh
    { mode: rrwToString mode }
    fe
    \s -> do
      p <- try $ strToPermission s
      case p of
        Left e -> fe e
        Right p' -> fa p'
  requestPermission fsh { mode } fe fa = requestPermission_FileSystemFileHandle
    fsh
    { mode: rrwToString mode }
    fe
    \s -> do
      p <- try $ strToPermission s
      case p of
        Left e -> fe e
        Right p' -> fa p'

data FileSystemFileHandle

foreign import getFile
  :: FileSystemFileHandle
  -> (Error -> Effect Unit)
  -> (File -> Effect Unit)
  -> Effect Unit

foreign import createWritable
  :: FileSystemFileHandle
  -> (Error -> Effect Unit)
  -> (FileSystemWritableFileStream -> Effect Unit)
  -> Effect Unit

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
  :: { | ShowOpenFilePickerOptional }
  -> (Error -> Effect Unit)
  -> (Array FileSystemFileHandle -> Effect Unit)
  -> Effect Unit

showOpenFilePicker
  :: forall provided
   . Defaults { | ShowOpenFilePickerOptional }
       { | provided }
       { | ShowOpenFilePickerAll }
  => { | provided }
  -> (Error -> Effect Unit)
  -> (Array FileSystemFileHandle -> Effect Unit)
  -> Effect Unit
showOpenFilePicker provided = showOpenFilePicker_ all
  where
  all :: { | ShowOpenFilePickerAll }
  all = defaults defaultShowOpenFilePickerOptions provided

--
type ShowSaveFilePickerBase =
  ( excludeAcceptAllOption :: Boolean
  , types ::
      Array
        { description :: Maybe String
        , accept :: Object (Array String)
        }
  )

type ShowSaveFilePickerFull =
  ( suggestedName :: String
  | ShowSaveFilePickerBase
  )

type ShowSaveFilePickerOptional =
  ( suggestedName :: Maybe String
  | ShowSaveFilePickerBase
  )

type ShowSaveFilePickerAll = (| ShowSaveFilePickerOptional)

defaultShowSaveFilePickerOptions :: { | ShowSaveFilePickerOptional }
defaultShowSaveFilePickerOptions =
  { suggestedName: Nothing
  , excludeAcceptAllOption: false
  , types: []
  }

foreign import showSaveFilePickerBase_
  :: { | ShowSaveFilePickerBase }
  -> (Error -> Effect Unit)
  -> (Array FileSystemFileHandle -> Effect Unit)
  -> Effect Unit

foreign import showSaveFilePickerFull_
  :: { | ShowSaveFilePickerFull }
  -> (Error -> Effect Unit)
  -> (Array FileSystemFileHandle -> Effect Unit)
  -> Effect Unit

showSaveFilePicker
  :: forall provided
   . Defaults { | ShowSaveFilePickerOptional }
       { | provided }
       { | ShowSaveFilePickerAll }
  => { | provided }
  -> (Error -> Effect Unit)
  -> (Array FileSystemFileHandle -> Effect Unit)
  -> Effect Unit
showSaveFilePicker provided = case all.suggestedName of
  Nothing -> showSaveFilePickerBase_
    { excludeAcceptAllOption: all.excludeAcceptAllOption
    , types: all.types
    }
  Just suggestedName -> showSaveFilePickerFull_
    { excludeAcceptAllOption: all.excludeAcceptAllOption
    , suggestedName
    , types: all.types
    }
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
  -> { mode :: String }
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

foreign import requestPermission_FileSystemDirectoryHandle
  :: FileSystemDirectoryHandle
  -> { mode :: String }
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit

instance FileSystemHandle FileSystemDirectoryHandle where
  kind = kind_FileSystemDirectoryHandle
  name = name_FileSystemDirectoryHandle
  isSameEntry = isSameEntry_FileSystemDirectoryHandle
  queryPermission fsh { mode } fe fa = queryPermission_FileSystemDirectoryHandle
    fsh
    { mode: rrwToString mode }
    fe
    \s -> do
      p <- try $ strToPermission s
      case p of
        Left e -> fe e
        Right p' -> fa p'
  requestPermission fsh { mode } fe fa =
    requestPermission_FileSystemDirectoryHandle fsh
      { mode: rrwToString mode }
      fe
      \s -> do
        p <- try $ strToPermission s
        case p of
          Left e -> fe e
          Right p' -> fa p'

data FileSystemDirectoryHandle

foreign import showDirectoryPicker
  :: (Error -> Effect Unit)
  -> (FileSystemDirectoryHandle -> Effect Unit)
  -> Effect Unit

data FD = File FileSystemFileHandle | Directory FileSystemDirectoryHandle

foreign import entries_
  :: (FileSystemFileHandle -> FD)
  -> (FileSystemDirectoryHandle -> FD)
  -> (String -> FD -> String /\ FD)
  -> FileSystemDirectoryHandle
  -> (Error -> Effect Unit)
  -> (Array (String /\ FD) -> Effect Unit)
  -> Effect Unit

entries
  :: FileSystemDirectoryHandle
  -> (Error -> Effect Unit)
  -> (Array (String /\ FD) -> Effect Unit)
  -> Effect Unit
entries = entries_ File Directory (/\)

foreign import getFileHandle
  :: FileSystemDirectoryHandle
  -> String
  -> { create :: Boolean }
  -> (Error -> Effect Unit)
  -> (FileSystemFileHandle -> Effect Unit)
  -> Effect Unit

foreign import getDirectoryHandle
  :: FileSystemDirectoryHandle
  -> String
  -> { create :: Boolean }
  -> (Error -> Effect Unit)
  -> (FileSystemDirectoryHandle -> Effect Unit)
  -> Effect Unit
