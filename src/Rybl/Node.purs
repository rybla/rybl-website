module Rybl.Node where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as NFSA
import Node.FS.Sync as NFS

type CopyOptions =
  { recursive :: Boolean
  , force :: Boolean
  , errorOnExist :: Boolean
  }

foreign import copy_ :: String -> String -> CopyOptions -> Effect (Promise Unit)

copy' :: String -> String -> CopyOptions -> Aff Unit
copy' source target opts = do toAffE $ copy_ source target opts

copy :: String -> String -> CopyOptions -> Aff Unit
copy source target opts = do
  Console.log $ intercalate " " [ "copy", source, "->", target ]
  copy' source target opts

copyDir :: String -> String -> { errorOnExist :: Boolean } -> Aff Unit
copyDir source target { errorOnExist } = do
  copy source target { recursive: true, force: true, errorOnExist }

initDir :: String -> Aff Unit
initDir target = do
  (NFS.exists target # liftEffect) >>= case _ of
    true -> pure unit
    false -> do
      Console.log $ intercalate " " [ "init", target ]
      NFSA.mkdir target

createDir :: String -> Aff Unit
createDir target = do
  Console.log $ intercalate " " [ "create", target ]
  NFSA.mkdir target

resetDir :: String -> Aff Unit
resetDir target = do
  Console.log $ intercalate " " [ "reset", target ]
  NFSA.rm' target { force: true, recursive: true, maxRetries: 1, retryDelay: 10 }
  NFSA.mkdir target

removeDir :: String -> Aff Unit
removeDir target = do
  Console.log $ intercalate " " [ "remove", target ]
  NFSA.rm' target { force: true, recursive: true, maxRetries: 1, retryDelay: 10 }

replaceDir :: String -> String -> Aff Unit
replaceDir source target = do
  Console.log $ intercalate " " [ "replace", source, "->", target ]
  NFSA.rm' target { force: true, recursive: true, maxRetries: 1, retryDelay: 10 }
  copy' source target { recursive: true, force: true, errorOnExist: false }

writeTextFile :: String -> String -> Aff Unit
writeTextFile target content = do
  Console.log $ intercalate " " [ "write", target ]
  NFSA.writeTextFile UTF8 target content

