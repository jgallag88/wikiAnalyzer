-- philoGame.hs  John Gallagher 9/15/13
--
-- Haskell program to the follow the first link in an English Wikipedia page
-- and find how many end up at the Wikipedia page for Philosophy

--TODO: Add quickcheck for parsec stuff
--TODO: Change from monadic to applicative style where possible in parsec stuff

import System.IO
import qualified Text.XML.Expat.Tree as XPat
import qualified Text.XML.Expat.Format as XPat
import qualified Text.XML.Expat.Proc as XPat
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import PageParser
import Data.Maybe

data Page = FullPage { title :: T.Text,
                       link :: T.Text}
            | Stub { title :: T.Text }
            | Redirect { title :: T.Text,
                         redirectLink :: T.Text} deriving Show

isFullPage (FullPage _ _) = True
isFullPage _ = False

isStub (Stub _) = True
isStub _ = False

wikiFile = "/media/OS/Users/John/Desktop/enwiki-20130904-pages-articles.xml"
sampleXML = openFile "sample.xml" ReadMode >>= hGetContents
wikiXMLString = openFile wikiFile ReadMode >>= hGetContents


main = do
    xmlFile <- L.readFile wikiFile
    let (xmlTree, mErr) = XPat.parse XPat.defaultParseOptions xmlFile :: (XPat.UNode T.Text, Maybe XPat.XMLParseError) 
    let pageList = fmap createPage $ getPages xmlTree
    putStrLn $ flip examinePageAST (T.pack "Autism") $ getPages xmlTree
    return $ take 10 $ filter isStub pageList

createPage :: XPat.NodeG [] T.Text T.Text -> Page
createPage p = case extractTitle p of 
    Nothing -> Stub { title = T.pack "" }
    Just t -> case extractRedir p of 
            Just r -> Redirect { title = t, redirectLink = r }
            Nothing -> case extractLink p of 
                    Just (Right l) -> FullPage { title = t, link = l}
                    Just (Left err) -> Stub { title = t `T.append` (errToText err)}
                    Nothing -> Stub { title = t }
 
--Extract Redirect Link (if it exists) from XML Page element
extractRedir :: XPat.NodeG [] T.Text a -> Maybe a
extractRedir p = fmap extract elem
    where extract = \(XPat.Element _ [(_,link)] _) -> link
          elem = XPat.findChild (T.pack "redirect") p

extractLink p = fmap (philoLink . extractText) elems
    where elems = XPat.findElement (T.pack "text") p

--Extract Title from XML Page element
extractTitle :: XPat.NodeG [] T.Text a -> Maybe a
extractTitle p = fmap extract elem
    where extract = \(XPat.Element _ _ [XPat.Text title]) -> title
          elem = XPat.findChild (T.pack "title") p

extractText (XPat.Element _ _ textList) = T.concat $ XPat.onlyText textList

getPages tree = XPat.findChildren (T.pack "page") tree

-- Debug wiki AST of a particular page
examinePageAST tree pageName = show ast 
    where ast = lazyMany node "" $ page . head $ filter rightPage tree
          page p = extractText $ fromJust $ XPat.findElement (T.pack "text") p   
          rightPage p = case extractTitle p of
                             Just t -> t == pageName
                             Nothing -> False
          



