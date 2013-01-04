module Paste where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (stringE)

paste :: QuasiQuoter
paste = QuasiQuoter {
  quoteExp = stringE,
  quotePat = undefined,
  quoteType = undefined,
  quoteDec = undefined
  }
