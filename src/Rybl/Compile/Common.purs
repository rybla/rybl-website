module Rybl.Compile.Common where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Effect.Aff (Aff)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M = ReaderT Ctx (StateT Env (ExceptT String Aff))

type Ctx = {}

type Env = {}

--------------------------------------------------------------------------------
-- Doc builders
--------------------------------------------------------------------------------

