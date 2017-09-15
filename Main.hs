{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad (guard, forM_, forM)
import System.IO (readFile)
import System.Environment (getArgs)

import qualified Data.Set as Set
import Data.List (sortBy)

import qualified System.Console.ANSI as C

data Role = Titolare | Riserva
          deriving (Show, Eq, Ord)

roleToSGR :: Role -> C.SGR
roleToSGR Titolare = C.SetColor C.Foreground C.Vivid C.White
roleToSGR Riserva  = C.SetColor C.Foreground C.Dull C.White

data Position = P | D | C | A
              deriving(Show, Eq, Ord)

positionFromString :: String -> Position
positionFromString "P" = P
positionFromString "D" = D
positionFromString "C" = C
positionFromString "A" = A

positionToSGR :: Position -> C.SGR
positionToSGR P = C.SetColor C.Foreground C.Dull C.Yellow
positionToSGR D = C.SetColor C.Foreground C.Dull C.Blue
positionToSGR C = C.SetColor C.Foreground C.Dull C.Green
positionToSGR A = C.SetColor C.Foreground C.Dull C.Red

data PlayerInfos = PlayerInfos
                 { name :: String
                 , position :: Position
                 , role :: Role
                 , perc :: String
                 {-, squalificato :: Bool
                 , indisponibile :: String
                 , dubbio :: Bool
                 , diffidato :: Bool -}
                 , matchPlaying :: String
                 }
                 deriving (Show)

orderPlayers :: PlayerInfos -> PlayerInfos -> Ordering
orderPlayers (PlayerInfos n1 p1 r1 _ _) (PlayerInfos n2 p2 r2 _ _)
    | roleOrder == EQ = if posOrder == EQ
                           then compare n1 n2
                           else posOrder

    | otherwise       = roleOrder

    where roleOrder = compare r1 r2
          posOrder  = compare p1 p2


main :: IO ()
main = do
    url    <- head . lines <$> readFile "Sito.config"
    config <- readFile "Rosa.config"
    let rosaMap = makeMap config
    res <- scrapeURL url (myScraper rosaMap)
    case res of
         Nothing -> putStrLn "Errore"
         Just s  -> outputPlayers $ sortBy orderPlayers s


outputPlayers :: [PlayerInfos] -> IO ()
outputPlayers players = forM_ players (\player -> do
    C.setSGR [roleToSGR (Main.role player)]
    putStr $ show (role player)
    C.setSGR [positionToSGR (Main.position player)]
    putStr $ " " ++ show (Main.position player) 
    C.setSGR []
    putStrLn $ " " ++ name player ++ " " ++ perc player ++ " " ++ matchPlaying player)

makeMap :: String -> Set.Set String
makeMap config = Set.fromList $ lines config


-- myScraper :: Scraper String [String]
--myScraper m = chroot ("div" @: ["id" @= "artContainer"] // ("div" @: ["id" @= "sqtab"])) innerScraper
    --where innerScraper = chroots ("div" @: [hasClass "pgroup"]) (player m) -- (Left <$> squalificati <|> Right <$> diffidati)

myScraper :: Set.Set String -> Scraper String [PlayerInfos]
myScraper m = concat <$> (tabellaPartite >>= mapM (playersScraper m))

tabellaPartite :: Scraper String [String]
tabellaPartite = chroot ("ul" @: ["id" @= "mnavtabs2"]) innerScraper
    where innerScraper = filter (not . null) .  map tail <$>
                  chroots (tagSelector "li") (attr "href" (tagSelector "a"))

playersScraper :: Set.Set String -> String -> Scraper String [PlayerInfos]
playersScraper m matchId = chroot ("div" @: ["id" @= matchId]) innerScraper
    where innerScraper = chroots ("div" @: [hasClass "pgroup"]) (player m matchId)

player m matchId = do
    name <- text (tagSelector "a")

    -- filter out players we don't care about
    guard (name `Set.member` m)

    pos <- text ("span" @: [hasClass "role"])
    (role, perc) <- ((,) Titolare <$> text ("span" @: [hasClass "perc"])) <|> ((,) Riserva <$> text ("div" @: [hasClass "is"]))
    return $ PlayerInfos name (positionFromString pos) role perc matchId


altriCalciatori :: String -> Scraper String a -> Scraper String [a]
altriCalciatori section action = do
    content <- text (tagSelector "span")
    guard (content == section)
    firstP <- text (tagSelector "p")
    if firstP == "-"
       then return []
       else chroots (tagSelector "p") action

squalificati = altriCalciatori "SQUALIFICATI" $ text "p"

indisponibili = altriCalciatori "INDISPONIBILI" $ init . init <$> text "span"

inDubbio = altriCalciatori "IN DUBBIO" $ init . init <$> text "span"

ballottaggi = altriCalciatori "BALLOTTAGGI" $ stripFirstSpaces <$> texts "span"
    where stripFirstSpace :: String -> String
          stripFirstSpace str = if head str == ' '
                                   then tail str
                                   else str
          stripFirstSpaces :: [String] -> [String]
          stripFirstSpaces = map stripFirstSpace


diffidati = altriCalciatori "DIFFIDATI" $ text "span"
