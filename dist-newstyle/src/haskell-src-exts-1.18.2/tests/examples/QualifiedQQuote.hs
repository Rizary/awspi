{-# LANGUAGE QuasiQuotes #-}
import qualified Language.Haskell.TH as T

x :: T.DecsQ
x = [T.d|id|]

