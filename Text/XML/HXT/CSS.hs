
module Text.XML.HXT.CSS
    ( process
    , select
    , Selector
    , parseSelector
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.CSS.Internal.Selector
import Text.XML.HXT.CSS.Internal.Helpers

select :: (ArrowChoice a, ArrowXml a) => Selector -> a XmlTree XmlTree
select = multi . selectorToArrow

processMany :: ArrowXml a => [(Selector, a XmlTree XmlTree)] -> a XmlTree XmlTree
processMany = undefined

process :: (ArrowChoice a, ArrowXml a) => Selector -> a XmlTree XmlTree -> a XmlTree XmlTree
process sel op = processTopDown $ case sel of
    node `DescendantOf` parent -> process node op `when` selectorToArrow parent
    node `ChildOf` parent -> processChildren (op `when` selectorToArrow node) `when`selectorToArrow parent
    node `PrecededBy` left -> replaceChildren $ processPreceding (selectorToArrow left) (op `when` selectorToArrow node)
    sel -> op `when` selectorToArrow sel

