{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.HUnit
import Test.Hspec (it, describe, hspec)
import Test.Hspec.HUnit

import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Text.XML.HXT.CSS.Internal.Selector
import Text.XML.HXT.CSS.QQ

newtype HtmlResult = HtmlResult [String] deriving (Eq)

instance Show HtmlResult where
    show (HtmlResult s) = concat s

testHtml :: IO (LA a XmlTree)
testHtml = do
    html <- readFile "test-html/test.html"
    return $ constA html >>> hread

testSel :: Selector -> LA XmlTree b -> IO [b]
testSel sel op = do
    testPage <- testHtml
    return $ runLA (testPage >>> select sel >>> op) undefined

testProc :: FilePath -> Selector -> LA XmlTree XmlTree -> IO ()
testProc fp sel op = do
    testPage <- testHtml
    comparePage <- readFile $ "test-html/" ++ fp ++ ".html"
    let actual   = HtmlResult $ runLA (testPage >>> process sel op >>> writeDocumentToString [withOutputHTML]) undefined
        expected = HtmlResult $ runLA (constA comparePage >>> hread >>> writeDocumentToString [withOutputHTML]) undefined

    actual @?= expected

setText :: ArrowXml a => String -> a XmlTree XmlTree
setText = replaceChildren . txt

main = hspec $ describe "hxt-css-selectors" $ do
    describe "quasi-quotes" $ do
        it "should parse a name-selector" $ do
            [css|body|] @?= SelectName "body"

        it "should parse a class-selector" $ do
            [css|.first|] @?= SelectClass "first"

        it "should parse an id-selector" $ do
            [css|#footer|] @?= SelectId "footer"

        it "should parse a combined name and class selector" $ do
            [css|p.first|] @?= Select [SelectName "p", SelectClass "first"]

        it "should parse a descendant selector" $ do
            [css|#footer a|] @?= SelectName "a" `DescendantOf` SelectId "footer"

        it "should parse direct child selection" $ do
            [css|section > a|] @?= SelectName "a" `ChildOf` SelectName "section"

        it "should parse with correct precedence" $ do
            [css|a b > c d|] @?= SelectName "d" `DescendantOf` (SelectName "c" `ChildOf` (SelectName "b" `DescendantOf` SelectName "a"))

            [css|a > b, c d|] @?= SelectAll [SelectName "b" `ChildOf` SelectName "a", SelectName "d" `DescendantOf` SelectName "c"]

        it "should parse an any-selector" $ do
            [css|a *|] @?= SelectAny `DescendantOf` SelectName "a"

        it "should parse sibling selector" $ do
            [css|a ~ b|] @?= SelectName "b" `PrecededBy` SelectName "a"

        it "should parse adjancent selector" $ do
            [css|a + b|] @?= SelectName "b" `AdjacentTo` SelectName "a"

        it "should parse sibling selector in a descendant" $ do
            [css|a b ~ c|] @?= (SelectName "c" `PrecededBy` SelectName "b") `DescendantOf` SelectName "a"

    describe "selecting" $ do
        it "should select by element name" $ do
            res <- testSel [css|body|] getName
            res @?= ["body"]

        it "should select by class name" $ do
            res <- testSel [css|.first|] getName
            res @?= ["a", "p"]

        it "should select by id" $ do
            res <- testSel [css|#footer|] (getName &&& getAttrValue "class")
            res @?= [("section", "bottom")]

        it "should select by both name and class" $ do
            res <- testSel [css|p.first|] (getName &&& getAttrValue "class")
            res @?= [("p", "first")]

        it "should select direct descendants" $ do
            res <- testSel [css|#footer a|] (getName &&& getAttrValue "class")
            res @?= [("a", "contact")]

        it "should select all descendants" $ do
            res <- testSel [css|body a|] (getName &&& getAttrValue "class")
            res @?= [("a", "first"), ("a", "second"),("a", "contact")]

        it "should only select direct children" $ do
            res <- testSel [css|section > a|] (getName &&& getAttrValue "class")
            res @?= [("a", "contact")]

        it "should select many patterns" $ do
            res <- testSel [css|#header, #content p|] (getName &&& getAttrValue "class")
            res @?= [("section" ,"top"),("p" ,"first"), ("p", "second")]

        it "should match anything with *" $ do
            res <- testSel [css|body > *|] (getName &&& getAttrValue "class")
            res @?= [("section","top"),("section","middle"),("section","bottom")]

        it "should match preceding selector" $ do
            res <- testSel [css|li.one ~ li|] (getName &&& getAttrValue "class")
            res @?= [("li","two"),("li","three")]

        it "should match adjancent selector" $ do
            res <- testSel [css|li.one + li|] (getName &&& getAttrValue "class")
            res @?= [("li","two")]

        it "shoud match preceding selector in a descendant" $ do
            res <- testSel [css|#content li.one ~ li|] (getName &&& getAttrValue "class")
            res @?= [("li","two"),("li","three")]

        it "shoud match preceding selector in a direct descendant" $ do
            res <- testSel [css|ul li.one ~ li|] (getName &&& getAttrValue "class")
            res @?= [("li","two"),("li","three")]

        it "shoud match preceding selector in a child" $ do
            res <- testSel [css|ul > li.one ~ li|] (getName &&& getAttrValue "class")
            res @?= [("li","two"),("li","three")]

        it "should match adjancent selector in a descendant" $ do
            res <- testSel [css|#content li.one + li|] (getName &&& getAttrValue "class")
            res @?= [("li","two")]

        it "should match adjancent selector in a direct descendant" $ do
            res <- testSel [css|ul li.one + li|] (getName &&& getAttrValue "class")
            res @?= [("li","two")]

        it "should match adjancent selector in a child" $ do
            res <- testSel [css|ul > li.one + li|] (getName &&& getAttrValue "class")
            res @?= [("li","two")]

        it "should match descendants in sibling nodes" $ do
            res <- testSel [css|p.first ~ ul li|] (getName &&& getAttrValue "class")
            res @?= [("li","one"),("li","two"),("li","three")]

        it "should match elements that contain an attribute" $ do
            res <- testSel [css|*[id]|] (getName &&& getAttrValue "id")
            res @?= [("section","header"),("section","content"),("section","footer")]

        it "should match elements that contain a specific attribute value" $ do
            res <- testSel [css|*[class="first"]|] (getName &&& getAttrValue "class")
            res @?= [("a","first"),("p","first")]

    describe "processing" $ do
        it "should apply changes to a named node" $ do
            testProc "test-title-change" [css|title|] (setText "Foobar")

        it "should apply changes to all nodes with a class" $ do
            testProc "test-class-proc" [css|.first|] (setText "Edited")

        it "should apply changes to all nodes with specific name and class" $ do
            testProc "test-multi-assign" [css|li.one, li.three|] (setText "Edited")

        it "should apply changes with descendant selector" $ do
            testProc "test-descendant-assign" [css|#header a|] (setText "Edited")

        it "should apply changes with a child selector" $ do
            testProc "test-child-assign" [css|section > a|] (setText "Edited")

        it "should apply changes with a sibling selector" $ do
            testProc "test-sibling-assign" [css|li.one ~ *|] (setText "Edited")
