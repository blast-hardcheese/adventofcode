> module Day2 (
>     day2solution
>   ) where

> import Control.Applicative
> import Data.Maybe
> import Text.Trifecta
> import Text.Trifecta.Delta

> newtype Feet = Feet { getFeet :: Integer }
>   deriving (Show, Eq, Ord)
> newtype SquareFeet = SquareFeet { getSquareFeet :: Integer }
>   deriving (Show, Eq, Ord)
> type Length = Feet
> type Width = Feet
> type Height = Feet
> type PaperArea = SquareFeet
> type RibbonArea = Feet

> paperAreaRequired :: Length -> Width -> Height -> PaperArea
> paperAreaRequired (Feet l) (Feet w) (Feet h) = SquareFeet (2 * lw + 2 * wh + 2 * hl + extra)
>   where lw = l * w
>         wh = w * h
>         hl = h * l
>         extra = min (min lw wh) hl

> ribbonAreaRequired :: Length -> Width -> Height -> RibbonArea
> ribbonAreaRequired (Feet l) (Feet w) (Feet h) = Feet (ribbon + bow)
>   where lw = 2 * l + 2 * w
>         wh = 2 * w + 2 * h
>         hl = 2 * h + 2 * l
>         ribbon = min (min lw wh) hl
>         bow = l * w * h

> parseDimensions :: Parser (PaperArea, RibbonArea)
> parseDimensions = (buildPair <$> feet <*> (char 'x' >> feet) <*> (char 'x' >> feet))
>   where feet = Feet <$> integer
>         buildPair :: Length -> Width -> Height -> (PaperArea, RibbonArea)
>         buildPair l w h = (paperAreaRequired l w h, ribbonAreaRequired l w h)

> day2solution path = process <$> parseFromFile (many parseDimensions) path
>   where sumEach (paperAreas, ribbonAreas) = (SquareFeet (sum $ getSquareFeet <$> paperAreas), Feet (sum $ getFeet <$> ribbonAreas))
>         process :: Maybe [(PaperArea, RibbonArea)] -> (PaperArea, RibbonArea)
>         process = sumEach . unzip . (fromMaybe [])
