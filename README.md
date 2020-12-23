# Substitute

[![Pursuit](https://pursuit.purescript.org/packages/purescript-substitute/badge)](https://pursuit.purescript.org/packages/purescript-substitute)

Substitute is an advanced string interpolation library that allows you to customize the behaviour of your interpolation function. It includes features such as removing whitespace from multi-line strings so they can be indented like the rest of your code, and preserving indentation levels when inserting multi-line strings. Here is an example showing both of these features.
```purescript
-- str1 = str2

str1 =
  substitute
    """
    ${name} :: Int -> Int -> ${type}
    ${name} a b =
      let
        ${lets}
      in
        c - d
    """
    { name: "myFunction"
    , "type": "Int"
    , lets:
        """
        c = a + b

        d = a * b
        """
    }

str2 =
  """myFunction :: Int -> Int -> Int
myFunction a b =
  let
    c = a + b

    d = a * b
  in
    c - d
"""
```
