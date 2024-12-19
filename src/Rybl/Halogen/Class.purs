module Rybl.Halogen.Class where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.TypeError (class Warn, Beside, QuoteLabel, Text)
import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

class IsSymbol x <= ValidClass x

-- instance (IsSymbol x, ValidClass x) => ValidClass x
-- else instance (IsSymbol x, UnknownClass x) => ValidClass x

-- class IsSymbol x <= UnknownClass x

-- instance (IsSymbol x, Warn (Beside (Text "unknown class: ") (QuoteLabel x))) => UnknownClass x

-- class IsSymbol x <= ValidClass x

mk :: forall @x. ValidClass x => ClassName
mk = ClassName (reflectSymbol (Proxy @x))

instance ValidClass "app"
else instance ValidClass "doc"
else instance ValidClass "group"
else instance ValidClass "group_column"
else instance ValidClass "group_row"
else instance ValidClass "group_flow"
else instance ValidClass "sidenote_threshold"
else instance ValidClass "sidenote_threshold_body"
else instance ValidClass "sidenote_threshold_sidenotes"
else instance ValidClass "expander"
else instance ValidClass "sidenote"
else instance ValidClass "sidenote_body"
else instance ValidClass "sidenote_label"
else instance ValidClass "sidenote_id"
else instance ValidClass "string"
else instance ValidClass "error"
else instance ValidClass "ref"
else instance ValidClass "ref_missing"
else instance ValidClass "ref_loading"
else instance ValidClass "ref_error"
else instance ValidClass "expander_button"
else instance ValidClass "expander_placeholder_content"
else instance (IsSymbol x, Warn (Beside (Text "unknown class: ") (QuoteLabel x))) => ValidClass x