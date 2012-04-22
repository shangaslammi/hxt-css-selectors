{-# LANGUAGE TemplateHaskell #-}

module Text.XML.HXT.CSS.QQ (css) where

import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.XML.HXT.CSS.Internal.Selector

deriveLift ''Selector

css = QuasiQuoter
    { quoteExp  = mkSelectorQ
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

mkSelectorQ :: String -> ExpQ
mkSelectorQ s = case parseSelector s of
    Nothing  -> error $ "invalid css selector: " ++ s
    Just sel -> [|sel|]
