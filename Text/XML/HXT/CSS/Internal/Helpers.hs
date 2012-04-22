{-# LANGUAGE Arrows #-}

module Text.XML.HXT.CSS.Internal.Helpers where

import Text.XML.HXT.Core

hasClass :: ArrowXml a => String -> a XmlTree XmlTree
hasClass c = hasAttrValue "class" $ (c `elem`) . words

hasId :: ArrowXml a => String -> a XmlTree XmlTree
hasId = hasAttrValue "id" . (==)

processPreceding :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree -> a XmlTree XmlTree -> a XmlTree XmlTree
processPreceding node op = listA getChildren >>> process where
    process = proc lst -> case lst of
        [] -> none -< ()
        (n:ns) -> (|ifA (node -< n)
            (constA n <+> (op <<< constL ns) -<< ())
            (constA n <+> process -<< ns)
            |)

preceding :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree -> a XmlTree XmlTree
preceding node = listA getChildren >>> prune  where
    prune = proc lst -> case lst of
        [] -> none -< ()
        (n:ns) -> (|ifA (node -< n) (constL ns -<< ()) (prune -< ns)|)

adjacent :: (ArrowChoice a, ArrowXml a) => a XmlTree XmlTree -> a XmlTree XmlTree
adjacent node = listA (getChildren >>> isElem) >>> prune  where
    prune = proc lst -> case lst of
        (n:a:ns) -> (|ifA (node -< n) (constA a -<< ()) (prune -< ns)|)
        _ -> none -< ()
