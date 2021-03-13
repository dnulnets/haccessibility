module Accessibility.Utils.ISO (ISO(..), unwrapISO) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime(..), Date, Millisecond, Time)
import Data.DateTime as DT
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (fromCodePointArray, codePointFromChar, toCodePointArray)
import Data.String (length) as String
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import Data.Traversable (sequence)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

-- | A minimal `DateTime` wrapper that encodes/decodes to/from the *simplified*
-- | extended ISO format ([`ISO 8601`](https://en.wikipedia.org/wiki/ISO_8601)).
-- | Specifically `YYYY-MM-DDTHH:mm:ss[.sss]Z` where hyphens and colons can be omitted.
newtype ISO = ISO DateTime

derive instance newtypeISO :: Newtype ISO _

-- | A monomorphic `unwrap`.
unwrapISO :: ISO -> DateTime
unwrapISO = unwrap

instance showISO :: Show ISO where
    show (ISO (DateTime date time)) = foldl append ""
        [ showInt $ DT.year date
        , "-"
        , padl 2 '0' $ showInt $ DT.month date
        , "-"
        , padl 2 '0' $ showInt $ DT.day date
        , "T"
        , padl 2 '0' $ showInt $ DT.hour time
        , ":"
        , padl 2 '0' $ showInt $ DT.minute time
        , ":"
        , padl 2 '0' $ showInt $ DT.second time
        , "."
        , removeTrailingZeros $ padMilli $ DT.millisecond time
        , "Z"
        ]
        where
            showInt :: forall a. BoundedEnum a => a -> String
            showInt = show <<< fromEnum

-- | Pad an integer from a millisecond value with enough zeros so it is three
-- digits.
padMilli :: Millisecond -> String
padMilli = padl 3 '0' <<< show <<< fromEnum

-- | Remove trailing zeros from a millisecond value.
removeTrailingZeros :: String -> String
removeTrailingZeros "000" = "0"
removeTrailingZeros s =
  fromCodePointArray <<<
  Array.reverse <<<
  Array.dropWhile (_ == codePointFromChar '0') <<<
  Array.reverse $
  toCodePointArray s

instance decodeJsonISO :: DecodeJson ISO where
    decodeJson = decodeJson
             >=> flip P.runParser (parseISO :: P.Parser String ISO)
             >>> lmap (\e -> TypeMismatch $ P.parseErrorMessage e)

instance encodeJsonISO :: EncodeJson ISO where
    encodeJson = show >>> encodeJson

--------------------------------------------------------------------------------

parseISO :: forall s m. Monad m => PS.StringLike s => P.ParserT s m ISO
parseISO = do
    date <- parseISODate
    _ <- PS.char 'T'
    time <- parseISOTime
    pure $ wrap $ DateTime date time

parseISODate :: forall s m. Monad m => PS.StringLike s => P.ParserT s m Date
parseISODate = do
    year  <- parseDigits 4 <#> toEnum >>= maybeFail "bad year"
    _     <- dash
    month <- parseDigits 2 <#> toEnum >>= maybeFail "bad month"
    _     <- dash
    day   <- parseDigits 2 <#> toEnum >>= maybeFail "bad day"
    DT.exactDate year month day # maybeFail "bad date"

    where dash  = PC.optional $ PC.try $ PS.char '-'

parseISOTime :: forall s m. Monad m => PS.StringLike s => P.ParserT s m Time
parseISOTime = do
    hh <- parseDigits 2 <#> toEnum >>= maybeFail "bad hour"
    _  <- colon
    mm <- parseDigits 2 <#> toEnum >>= maybeFail "bad minute"
    _  <- colon
    ss <- parseDigits 2 <#> toEnum >>= maybeFail "bad second"
    -- NOTE: milliseconds may not be present
    ms <- PC.option bottom $
                PC.try (PS.char '.') *> Array.some parseDigit <#> doMilli
    pure $ DT.Time hh mm ss ms
    where
        colon = PC.optional $ PC.try $ PS.char ':'

-- | Parse an array of three integers into a `Millisecond` value.
doMilli :: Array Int -> Millisecond
doMilli ns =
  let padded = Array.take 3 $ ns <> Array.replicate (3 - length ns) 0
  in fromMaybe top $ toEnum $ foldDigits padded

parseDigits :: forall s m. Monad m => PS.StringLike s => Int -> P.ParserT s m Int
parseDigits = map foldDigits <<< sequence <<< flip Array.replicate parseDigit

parseDigit :: forall s m. Monad m => PS.StringLike s => P.ParserT s m Int
parseDigit = PC.choice
    [ PS.char '0' *> pure 0
    , PS.char '1' *> pure 1
    , PS.char '2' *> pure 2
    , PS.char '3' *> pure 3
    , PS.char '4' *> pure 4
    , PS.char '5' *> pure 5
    , PS.char '6' *> pure 6
    , PS.char '7' *> pure 7
    , PS.char '8' *> pure 8
    , PS.char '9' *> pure 9
    ] `PC.withErrorMessage` "expected digit (0-9)"

--------------------------------------------------------------------------------

foldDigits :: forall f. Foldable f => f Int -> Int
foldDigits = foldl (\acc d -> acc * 10 + d) zero

maybeFail :: forall m s a. Monad m => String -> Maybe a -> P.ParserT s m a
maybeFail str = maybe (P.fail str) pure

padl :: Int -> Char -> String -> String
padl n chr str = String.fromCharArray $
    padl' (n - String.length str) chr (String.toCharArray str)

padl' :: Int -> Char -> Array Char -> Array Char
padl' n chr chrs
    | n <= 0 = chrs
    | otherwise = padl' (n - 1) chr (chr `Array.cons` chrs)
