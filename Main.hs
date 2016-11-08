{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad (guard)
import System.IO (readFile)

data Role = Titolare | Riserva
          deriving (Show)

data PlayerInfos = PlayerInfos
                 { name :: String
                 , position :: String
                 , role :: Role
                 , perc :: String
                 }
                 deriving (Show)


main :: IO ()
main = do
    -- str <- readFile "Prova.html"
    -- let res = scrapeStringLike str myScraper
    res <- scrapeURL "http://www.fantagazzetta.com/probabili-formazioni-serie-a" myScraper
    case res of
         Nothing -> putStrLn "Errore"
         Just s  -> mapM_ (putStrLn . show) s


-- myScraper :: Scraper String [String]
myScraper = chroot ("div" @: ["id" @= "artContainer"] // ("div" @: ["id" @= "sqtab"])) innerScraper
    where innerScraper = chroots ("div" @: [hasClass "pgroup"]) ballottaggi -- (Left <$> squalificati <|> Right <$> diffidati)

player = do
    name <- text (tagSelector "a")
    pos <- text ("span" @: [hasClass "role"])
    (role, perc) <- ((,) Titolare <$> text ("span" @: [hasClass "perc"])) <|> ((,) Riserva <$> text ("div" @: [hasClass "is"]))
    return $ PlayerInfos name pos role perc


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
