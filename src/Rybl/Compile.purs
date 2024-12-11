module Rybl.Compile where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Rybl.Compile.NamedDocs (namedDocs)
import Rybl.Constants (assets_dir, compile_output_dir, serve_dir)
import Rybl.Node (writeTextFile)
import Rybl.Node as RyblN

main :: Effect Unit
main = launchAff_ do
  Console.log "[compile] begin"

  RyblN.resetDir compile_output_dir

  Console.log "[compile] index"
  writeTextFile (compile_output_dir <> "/" <> "index.html") index_html_str

  Console.log "[compile] namedDocs"
  RyblN.initDir (compile_output_dir <> "/" <> "namedDocs")
  namedDocs # traverseWithIndex_ \x d -> do
    writeTextFile (compile_output_dir <> "/" <> "namedDocs/" <> x <> ".json") (toJsonString d)

  Console.log "[compile] assets"
  RyblN.copy "favicon.ico" (compile_output_dir <> "/" <> "favicon.ico") { errorOnExist: true, force: false, recursive: false }
  RyblN.copyDir assets_dir (compile_output_dir <> "/" <> "assets") { errorOnExist: true }

  Console.log "[compile] finalize"
  RyblN.replaceDir compile_output_dir serve_dir

  Console.log "[compile] end"

index_html_str :: String
index_html_str = String.trim
  """
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>rybl.net</title>
  <script src="main.js"></script>
</head>

<body></body>

</html>
"""
