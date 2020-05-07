module StateErrors
  ( unpackState
  ) where

import Control.Monad.State.Lazy (Monad(return, (>>)), mapM, (=<<), State, MonadState(get), runState, modify)
import Data.Either (Either(Left, Right), either)
import Errors (Errors)
import Data.Functor.Identity (Identity)

unpackState :: a -> State a (Either Errors b) -> Identity (Either Errors b)
unpackState arg m = do
  let (res, _) = runState m arg
  return res
