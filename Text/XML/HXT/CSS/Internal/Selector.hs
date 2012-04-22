
module Text.XML.HXT.CSS.Internal.Selector where

import Control.Applicative ((<$>),(*>),(<*))
import Control.Monad
import Control.Monad.Instances
import Data.Char
import Text.Parsec
import Text.XML.HXT.Core

import Text.XML.HXT.CSS.Internal.Helpers

data Selector
    = SelectName String
    | SelectClass String
    | SelectId String
    | Select [Selector]
    | DescendantOf Selector Selector
    | ChildOf Selector Selector
    | SelectAll [Selector]
    | SelectAny
    | PrecededBy Selector Selector
    | AdjacentTo Selector Selector
    | AttrExists String
    deriving (Eq, Show)

selectorToArrow :: (ArrowChoice a, ArrowXml a) => Selector -> a XmlTree XmlTree
selectorToArrow = step where
    step sel = case sel of
        SelectName name          -> hasName name
        SelectClass cls          -> hasClass cls
        SelectId id              -> hasId id
        AttrExists attr          -> hasAttr attr
        Select many              -> seqA $ map step many
        DescendantOf (PrecededBy node left) parent ->
            step parent >>> multi (preceding (step left) >>> step node)
        DescendantOf (AdjacentTo node left) parent ->
            step parent >>> multi (adjacent (step left) >>> step node)
        ChildOf (PrecededBy node left) parent ->
            step parent >>> (preceding (step left) >>> step node)
        ChildOf (AdjacentTo node left) parent ->
            step parent >>> (adjacent (step left) >>> step node)
        DescendantOf node parent -> step parent //> step node
        ChildOf node parent      -> step parent /> step node
        SelectAll many           -> catA $ map step many
        SelectAny                -> isElem
        PrecededBy node left     -> preceding (step left) >>> step node
        AdjacentTo node left     -> adjacent (step left) >>> step node


parseSelector :: String -> Maybe Selector
parseSelector = either (const Nothing) (Just . adjust) . parse selector "" where
    selector = do
        sels <- sepBy1 (chainl1 pattern sep) (spaces >> string "," >> spaces)
        eof
        return $ case sels of
            [single] -> single
            many     -> SelectAll many

    pattern = do
        parts <- many1 $ star <|> cls <|> id' <|> name <|> attr
        return $ case parts of
            [single] -> single
            many     -> Select many

    attrFilter = AttrExists <$> ident

    sep = spaces *> (child <|> sibling <|> adjacent <|> descendant)
    descendant = return (flip DescendantOf)
    child = string ">" *> spaces *> return (flip ChildOf)
    sibling = string "~" *> spaces *> return (flip PrecededBy)
    adjacent = string "+" *> spaces *> return (flip AdjacentTo)

    name = SelectName <$> ident
    cls  = char '.' *> (SelectClass <$> ident)
    id'  = char '#' *> (SelectId <$> ident)
    star = char '*' *> return SelectAny
    attr = char '[' *> (spaces *> attrFilter <* spaces) <* char ']'

    ident = many1 . satisfy . anyOf $ [isAlpha, (`elem` "-_")]

adjust :: Selector -> Selector
adjust sel = case sel of
    a `PrecededBy` (b `DescendantOf` c) -> (a `PrecededBy` b) `DescendantOf` c
    a `AdjacentTo` (b `DescendantOf` c) -> (a `AdjacentTo` b) `DescendantOf` c
    a `PrecededBy` (b `ChildOf` c) -> (a `PrecededBy` b) `ChildOf` c
    a `AdjacentTo` (b `ChildOf` c) -> (a `AdjacentTo` b) `ChildOf` c
    sel -> sel

anyOf :: [a -> Bool] -> (a -> Bool)
anyOf = (.) or . sequence
