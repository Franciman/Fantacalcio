{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad (guard)

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
    res <- scrapeURL "http://www.fantagazzetta.com/probabili-formazioni-serie-a" myScraper
    case res of
         Nothing -> putStrLn "Errore"
         Just s  -> mapM_ (putStrLn . show) s


-- myScraper :: Scraper String [String]
myScraper = chroot ("div" @: ["id" @= "sqtab"]) innerScraper
    where innerScraper = chroots ("div" @: [hasClass "pgroup"]) (Left <$> squalificati <|> Right <$> diffidati)

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

indisponibili = altriCalciatori "INDISPONIBILI" $ text "span"

inDubbio = altriCalciatori "IN DUBBIO" $ text "span"

ballottaggi = altriCalciatori "BALLOTTAGGI" $ texts "span"

diffidati = altriCalciatori "DIFFIDATI" $ text "span"
