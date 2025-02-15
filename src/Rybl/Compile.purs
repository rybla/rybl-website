module Rybl.Compile where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.FoldableWithIndex (traverseWithIndex_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Rybl.Language.Compile.NamedDocs (namedDocs)
import Rybl.Constants (assets_dir, compile_input_dir, compile_output_dir, serve_dir)
import Rybl.Language (RefId(..))
import Rybl.Node (writeTextFile)
import Rybl.Node as Node

main :: Effect Unit
main = launchAff_ do
  Console.log "[compile] begin"

  Node.resetDir compile_output_dir

  Console.log "[compile] index.html"
  Node.copy (compile_input_dir <> "/" <> "index.html") (compile_output_dir <> "/" <> "index.html") { errorOnExist: true, force: false, recursive: false }

  Console.log "[compile] main.css"
  Node.copy (compile_input_dir <> "/" <> "main.css") (compile_output_dir <> "/" <> "main.css") { errorOnExist: true, force: false, recursive: false }

  Console.log "[compile] namedDocs"
  Node.initDir (compile_output_dir <> "/" <> "namedDocs")
  namedDocs >>= traverseWithIndex_ \(RefId x) d -> do
    writeTextFile (compile_output_dir <> "/" <> "namedDocs/" <> x <> ".json") (toJsonString d)

  Console.log "[compile] assets"
  Node.copy "favicon.ico" (compile_output_dir <> "/" <> "favicon.ico") { errorOnExist: true, force: false, recursive: false }
  Node.copyDir assets_dir (compile_output_dir <> "/" <> "assets") { errorOnExist: true }

  Console.log "[compile] finalize"
  Node.replaceDir compile_output_dir serve_dir

  Console.log "[compile] end"

