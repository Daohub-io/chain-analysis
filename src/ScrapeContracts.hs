{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel.Core

import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Network.HTTP.Req
import Data.Default
import Control.Monad.IO.Class
import Network.HTTP.Req

import Data.Monoid ((<>))

import Text.HTML.TagSoup

main = do
    let (Right address) = Address.fromText "0x273930d21e01ee25e4c219b63259d214872220a2"
    let url = "etherscan.io"
    str <- runReq def $ do
        r <- req
            GET
            (https url /: "address" /: ("0x" <> (Address.toText address) <> "#code"))
            NoReqBody
            bsResponse -- specify how to interpret response
            mempty       -- query params, headers, explicit port number, etc.
        pure (responseBody r)
    print $ parseTags str
    mapM_ print $ (\(Just x)->x) $ getIt str

getIt = scrape (chroots ("html" // "table" // "tr") (texts ("td"))) . parseTags

-- allComments :: Address -> IO (Maybe [String])
-- allComments address = scrapeURL  comments
--    where
--        comments :: Scraper String [String]
--        comments = chroots ("table") comment

--     --    comment :: Scraper String Comment
--     --    comment = textComment <|> imageComment

--        textComment :: Scraper String String
--        textComment = do
--            author      <- text $ "tbody"
--         --    commentText <- text $ "div"  @: [hasClass "text"]
--            return author

--     --    imageComment :: Scraper String Comment
--     --    imageComment = do
--     --        author   <- text       $ "span" @: [hasClass "author"]
--     --        imageURL <- attr "src" $ "img"  @: [hasClass "image"]
--     --        return $ ImageComment author imageURL