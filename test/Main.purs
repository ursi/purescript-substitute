module Test.Main where

import MasonPrelude
import Substitute (defaultOptions, normalize, substitute, createSubstituter)
import Test.Assert (assert)

main :: Effect Unit
main = do
  test "1"
    ( normalize
        """
        hi
        how
        are you
        """
    )
    """hi
how
are you
"""
  test "2"
    ( normalize
        """
        hi
        how
        are you"""
    )
    """hi
how
are you"""
  test "3"
    ( normalize
        """
        hi
         how
          are you
        """
    )
    """hi
 how
  are you
"""
  test "4"
    ( normalize
        """
          hi
         how
        are you
        """
    )
    """  hi
 how
are you
"""
  test "5" (substitute "hi ${name}, how are you" { name: "Mason" })
    "hi Mason, how are you"
  test "6"
    ( substitute
        """
        hi ${name},
        how are you
        """
        { name: "Mason" }
    )
    """hi Mason,
how are you
"""
  test "6"
    ( substitute
        """
        hi
        ${name},
        how are you
        """
        { name: "Mason" }
    )
    """hi
Mason,
how are you
"""
  test "8"
    ( substitute
        """
        hi
          ${name},
        how are you
        """
        { name: "Mason" }
    )
    """hi
  Mason,
how are you
"""
  test "9"
    (substitute "hi \\${name}, how are you" { name: "Mason" })
    "hi ${name}, how are you"
  test "10" (substitute "$" {}) "$"
  test "11" (substitute "${name}" { name: "Mason" }) "Mason"
  test "12" (substitute "${name}" {}) "[MISSING KEY: \"name\"]"
  test "13"
    ( createSubstituter
        (defaultOptions { missing = const "missing" })
        "foo ${name} bar"
        {}
    )
    "missing"
  test "14"
    ( substitute
        """
        hi
            ${test}
        are you
        """
        { test:
            """
              Mason,
              how
              """
        }
    )
    """hi
    Mason,
    how
are you
"""
  test "15"
    ( createSubstituter (defaultOptions { suppress = false })
        """
        hi
            ${test}
        are you
        """
        { test:
            """
              Mason,
              how
              """
        }
    )
    """hi
    Mason,
    how
    
are you
"""
  test "16"
    ( createSubstituter
        ( defaultOptions
            { indent = false
            , suppress = false
            }
        )
        """
        hi
            ${test}
        are you
        """
        { test:
            """
              Mason,
              how
              """
        }
    )
    """hi
    Mason,
how

are you
"""
  test "17"
    ( createSubstituter (defaultOptions { indent = false })
        """
        hi
            ${test}
        are you
        """
        { test:
            """
              Mason,
              how
              """
        }
    )
    """hi
    Mason,
how
are you
"""
  test "18" (normalize "") ""
  test "19"
    ( normalize
        """
        """
    )
    ""
  test "20"
    ( normalize
        """
        
        """
    )
    "\n"
  test "21" (normalize "hi how are you") "hi how are you"
  test "22"
    ( normalize
        """

        """
    )
    "\n"
  test "22"
    ( normalize
        """

        test

        """
    )
    """
test

"""
  test "23"
    ( normalize
        """
        hi

        how

        are you
        """
    )
    """hi

how

are you
"""
  test "24"
    ( createSubstituter
        ( defaultOptions
            { indent = false
            , suppress = false
            , normalizeString = false
            , normalizeSubstitutions = false
            }
        )
        """
        hi
            ${test}
        are you
        """
        { test:
            """
            Mason,
            how
            """
        }
    )
    """
        hi
            
            Mason,
            how
            
        are you
        """
  test "docs: normalize"
    ( normalize
        """
          foo

          bar
             baz
          """
    )
    """foo

bar
   baz
"""
  test "docs: indent"
    ( substitute
        """
        f = do
          ${body}
        """
        { body:
            """
            log foo
            log bar
            log baz
            """
        }
    )
    """f = do
  log foo
  log bar
  log baz
"""
  test "docs: suppress"
    ( createSubstituter (defaultOptions { suppress = false })
        """
        f = do
          ${body}
        """
        { body:
            """
            log foo
            log bar
            log baz
            """
        }
    )
    """f = do
  log foo
  log bar
  log baz
  
"""

test :: String -> String -> String -> Effect Unit
test label s1 s2 =
  if s1 == s2 then
    pure unit
  else do
    log $ "failure: " <> label
    logShow s1
    logShow s2
    assert false