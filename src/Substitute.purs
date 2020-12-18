module Substitute
  ( module Type.Row.Homogeneous
  , normalize
  , createSubstituter
  , Options
  , defaultOptions
  , substitute
  , minimalOptions
  ) where

import MasonPrelude
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as StringC
import Foreign.Object as Obj
import Type.Row.Homogeneous (class Homogeneous)

toLines :: String -> NonEmptyArray String
toLines =
  String.split (Pattern "\n")
    .> ArrayNE.fromArray
    .> fromMaybe (pure "")

-- | Remove whitespace in a way that lets you use multi-line strings independent of indentation, and allows the first line to be lined up with the rest of the string. Single-line strings are unchanged.
-- |```
-- | -- str1 = str2
-- |
-- | str1 =
-- |   normalize
-- |     """
-- |     foo
-- |
-- |     bar
-- |        baz
-- |     """
-- |
-- | str2 =
-- |   """foo
-- |
-- | bar
-- |    baz
-- | """
-- |```
normalize :: String -> String
normalize str =
  ( if String.take 1 str == "\n" then
      String.drop 1 str
    else
      str
  )
    # toLines
    # \lines ->
        let
          blankLineRemoved =
            ArrayNE.unsnoc lines
              # \{ init, last } ->
                  if StringC.countPrefix (eq ' ') last == String.length last then
                    if Array.null init then
                      [ "" ]
                    else
                      init
                  else
                    ArrayNE.toArray lines

          minSpaces =
            foldl
              ( \acc line ->
                  if line == "" then
                    acc
                  else
                    StringC.countPrefix (eq ' ') line
                      # if acc == 0 then
                          identity
                        else
                          min acc
              )
              0
              blankLineRemoved
        in
          foldl
            ( \(acc /\ first) line ->
                ( String.drop minSpaces line
                    # if first then
                        identity
                      else \l -> acc <> "\n" <> l
                )
                  /\ false
            )
            ("" /\ true)
            blankLineRemoved
            # \(s /\ _) ->
                if ArrayNE.length lines == Array.length blankLineRemoved then
                  s
                else
                  s <> "\n"

data State
  = CountingSpaces Int
  | EnteringTemplate
  | GettingKey String
  | Continuing
  | Skipping

type ParseState
  = { leadingSpaces :: Int
    , state :: State
    }

-- | ### marker, open, close :: Char
-- | These three characters are used for detecting places to substitute values. Unless preceded by a `\`, sequences of the form `<marker><open>key<close>` will be replaced by the value at `key` in the substitution record, if it exists. When it doesn't exist, see `missing` below.
-- |
-- | ### missing :: String -> String
-- | When there is no value associated with `key`, the substituter returns the result of passing `key` into `missing`.
-- |
-- | ### normalizeString :: Boolean
-- | Use `normalize` on the string passed to the function.
-- |
-- | ### normalizeSubstitutions :: Boolean
-- | Use `normalize` on the substitutions.
-- |
-- | ### indent :: Boolean
-- | When substituting in multi-line strings, pad with the appropriate whitespace so that all the lines are at the indentation level of the marker. Empty lines are not padded.
-- |
-- |
-- | ```
-- | -- str1 = str2
-- |
-- | str1 =
-- |   substitute
-- |     """
-- |     f = do
-- |       ${body}
-- |     """
-- |     { body:
-- |         """
-- |         log foo
-- |         log bar
-- |         log baz
-- |         """
-- |     }
-- |
-- | str2 =
-- |   """f = do
-- |   log foo
-- |   log bar
-- |   log baz
-- | """
-- | ```
-- |
-- | ### suppress :: Boolean
-- | When substituting in multi-line strings that end in a `\n`, drop the `\n`. With `suppress = false`, `str1` in the example above evaluates to
-- | ```
-- | """f = do
-- |   log foo
-- |   log bar
-- |   log baz
-- |
-- | """
-- | ```
type Options
  = { marker :: Char
    , open :: Char
    , close :: Char
    , missing :: String -> String
    , normalizeString :: Boolean
    , normalizeSubstitutions :: Boolean
    , indent :: Boolean
    , suppress :: Boolean
    }

-- | ```
-- | { marker: '$'
-- | , open: '{'
-- | , close: '}'
-- | , missing: \key -> "[MISSING KEY: \"" <> key <> "\"]"
-- | , normalizeString: true
-- | , normalizeSubstitutions: true
-- | , indent: true
-- | , suppress: true
-- | }
-- | ```
defaultOptions :: Options
defaultOptions =
  { marker: '$'
  , open: '{'
  , close: '}'
  , missing: \key -> "[MISSING KEY: \"" <> key <> "\"]"
  , normalizeString: true
  , normalizeSubstitutions: true
  , indent: true
  , suppress: true
  }

createSubstituter ::
  ∀ r.
  Homogeneous r String =>
  Options ->
  String ->
  Record r ->
  String
createSubstituter { marker
, open
, close
, missing
, normalizeString
, normalizeSubstitutions
, indent
, suppress
} templateStr subsRec =
  let
    subs = Obj.fromHomogeneous subsRec

    chars =
      toCharArray
        $ if normalizeString then
            normalize templateStr
          else
            templateStr
  in
    foldx
      ( \(state /\ str) char ->
          let
            charS = fromChar char
          in
            case state.state, char of
              _, '\n' -> Cont $ state { state = CountingSpaces 0 } /\ (str <> charS)
              Continuing, '\\' -> Cont $ state { state = Skipping } /\ (str <> charS)
              Continuing, _ ->
                if char == marker then
                  Cont $ state { leadingSpaces = 0, state = EnteringTemplate } /\ (str <> charS)
                else
                  Cont $ state { state = Continuing } /\ (str <> charS)
              CountingSpaces n, ' ' -> Cont $ state { state = CountingSpaces $ n + 1 } /\ (str <> charS)
              CountingSpaces n, '\\' -> Cont $ state { leadingSpaces = n, state = Skipping } /\ (str <> charS)
              CountingSpaces n, _ ->
                if char == marker then
                  Cont $ state { leadingSpaces = n, state = EnteringTemplate } /\ (str <> charS)
                else
                  Cont $ state { state = Continuing } /\ (str <> charS)
              EnteringTemplate, _ ->
                if char == open then
                  Cont $ state { state = GettingKey "" } /\ StringC.dropRight 1 str
                else
                  Cont $ state { state = Continuing } /\ (str <> charS)
              GettingKey key, _ ->
                if char == close then case Obj.lookup key subs of
                  Just value ->
                    Cont $ state { state = Continuing }
                      /\ ( str
                            <> ( ( if normalizeSubstitutions then
                                    normalize value
                                  else
                                    identity value
                                )
                                  # ( \v ->
                                        unsnocString v
                                          # maybe v \{ init, last } ->
                                              if last == '\n' && suppress then
                                                init
                                              else
                                                v
                                    )
                                  # \v ->
                                      if indent then
                                        toLines v
                                          # \lines -> case ArrayNE.uncons lines of
                                              { head, tail } ->
                                                head
                                                  <> foldl
                                                      ( \acc line ->
                                                          acc <> "\n"
                                                            <> ( if line == "" then
                                                                  ""
                                                                else
                                                                  rep state.leadingSpaces " "
                                                              )
                                                            <> line
                                                      )
                                                      ""
                                                      tail
                                      else
                                        v
                              )
                        )
                  Nothing -> Stop $ state /\ missing key
                else
                  Cont $ state { state = GettingKey $ key <> charS } /\ str
              Skipping, _ ->
                if char == marker then
                  Cont $ state { state = Continuing } /\ (StringC.dropRight 1 str <> charS)
                else
                  Cont $ state { state = Continuing } /\ (str <> charS)
      )
      ( { leadingSpaces: 0
        , state: CountingSpaces 0
        }
          /\ ""
      )
      chars
      # snd

-- | `createSubstituter defaultOptions`
substitute :: ∀ r. Homogeneous r String => String -> Record r -> String
substitute = createSubstituter defaultOptions

unsnocString :: String -> Maybe { init :: String, last :: Char }
unsnocString s =
  let
    lengthm1 = String.length s - 1
  in
    StringC.charAt lengthm1 s
      <#> { last: _, init: String.take lengthm1 s }

data Fold a
  = Cont a
  | Stop a

foldx :: ∀ a b. (b -> a -> Fold b) -> b -> Array a -> b
foldx f = go
  where
  go :: b -> Array a -> b
  go acc bs = case Array.uncons bs of
    Just { head, tail } -> case f acc head of
      Cont b -> go b tail
      Stop b -> b
    Nothing -> acc

rep :: Int -> String -> String
rep n s
  | n == 0 = ""
  | otherwise = s <> rep (n - 1) s

-- | ```
-- | { marker: '$'
-- | , open: '{'
-- | , close: '}'
-- | , missing: \key -> "[MISSING KEY: \"" <> key <> "\"]"
-- | , normalizeString: false
-- | , normalizeSubstitutions: false
-- | , indent: false
-- | , suppress: false
-- | }
-- | ```
minimalOptions :: Options
minimalOptions =
  { marker: '$'
  , open: '{'
  , close: '}'
  , missing: \key -> "[MISSING KEY: \"" <> key <> "\"]"
  , normalizeString: false
  , normalizeSubstitutions: false
  , indent: false
  , suppress: false
  }
