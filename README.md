
= CSS Selectors for HXT

This package adds css-style selectors to [HXT](http://hackage.haskell.org/package/hxt) that can be used for both querying and for processing sub-trees of HTML/XML documents.

CSS selectors are constructed either with the quasi-quoter `css` which allows for compile time validation of the selector patterns, or by using the function `parseSelector :: String -> Maybe Selector`.


== Examples

```haskell
-- Find each link that is inside a text paragraph in the main content
links = select [css|#content p a|] >>> getAttributeValue "href"

-- Change every unordered list that is a direct child of a div into an ordered list
ulToOl = process [css|div > ul|] $ setElemName (mkName "ol")
```


== Current Status

hxt-css-selectors is a new project that is still mostly incomplete and highly unstable. My current priorities are

1. Complete test suite that covers all the interesting parts of the CSS Level 3 Selectors document (in progress)
2. Implementation that passes all tests (but might be ugly/hacky)
3. Clean and elegant implementation
4. Performance optimizations
