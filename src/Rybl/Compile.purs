module Rybl.Compile where

import Prelude

import Data.Argonaut.Encode (toJsonString)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Rybl.Compile.NamedDocs (namedDocs)
import Rybl.Constants (output_dir, serve_dir)
import Rybl.Node (writeTextFile)
import Rybl.Node as RyblN

main :: Effect Unit
main = launchAff_ do
  Console.log "[compile] begin"

  RyblN.initDir output_dir

  Console.log "[compile] index"
  writeTextFile (output_dir <> "/" <> "index.html") index_html_str

  Console.log "[compile] namedDocs"
  RyblN.resetDir (output_dir <> "/" <> "namedDocs/")
  namedDocs # traverseWithIndex_ \x d -> do
    writeTextFile (output_dir <> "/" <> "namedDocs/" <> x <> ".json") (toJsonString d)

  RyblN.replaceDir output_dir serve_dir

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
