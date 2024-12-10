module Rybl.Compile where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Rybl.Node (writeTextFile)
import Rybl.Node as RyblN

main :: Effect Unit
main = launchAff_ do
  Console.log "[compile]"
  writeTextFile "public/index.html" index_html_str
  RyblN.replaceDir "public/" "docs/"
  pure unit

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
