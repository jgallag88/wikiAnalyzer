--Parse wikipedia page for links in specified wiki markup language
module PageParser (philoLink, WikiParseErr (BadFormat, NoLinks)) where
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import Control.Applicative ((*>))

data WikiNode = Template [WikiNode]
                | Parenthetical [WikiNode]
                | Italics [WikiNode]
                | Link WikiNode
                | Content T.Text deriving Show

data WikiParseErr = BadFormat T.Text
                    | NoLinks deriving Show

--General link format:
--[[the Link]]
--[[the Link|display Text]]
--[[the Link#subLink]]
--info on wiki link formating: http://meta.wikimedia.org/wiki/Help:Link 

--Template - specially formmated page within a page (e.g. side bar, contents table)
-- {{These can be {{nested}}  }}

--Italics
--''Italicized Text''

--Parenthetical - not special markup, just normal paretheses 
--(parenthisized text)

-- Get the first top level link from the wiki AST

philoLink :: T.Text -> Either WikiParseErr T.Text
philoLink page = 
    case ast of
        Left err -> Left . BadFormat $ (T.pack . show) err
        Right [] -> Left NoLinks
        Right ls -> Right . linkText . head $ ls
    where ast = fmap (filter notLink) $ parse wikiAST "Bad Format" page
          notLink (Link _) = True
          notLink _ = False
          linkText (Link (Content link)) = link

--Build Abstract Syntax Tree of wikipedia page, using markup
wikiAST = many node

node = template 
        <|> italics
        <|> parenthetical
        <|> content
        <|> link

--Parsers for constructs we are interested in
template = fmap Template $ between (try rTmpltTok) (try lTmpltTok) wikiAST

italics = fmap Italics $ (try italTok) *> manyTill node (try italTok)

parenthetical = fmap Parenthetical $ between (try rParenTok) (try lParenTok) wikiAST

link = fmap Link $ between (try rLinkTok) (lLinkTok) content 

content = fmap (Content . T.pack) $ many1 (notFollowedBy (choice reserved) *> anyChar)
 
--Wiki markup tokens
rTmpltTok = string "{{"
lTmpltTok = string "}}"
italTok = string "''"
rParenTok = string "("
lParenTok = string ")"
rLinkTok = string "[["
lLinkTok = string "]]"

reserved = [rTmpltTok, lTmpltTok, italTok, rParenTok, lParenTok, rLinkTok, lLinkTok]

getLinkTest :: [Either WikiParseErr T.Text]
getLinkTest = fmap (philoLink . T.pack) testList 
    where testList = ["['{  [[realLink]]"
                     , "''asdf''daa''asdf[[NOT_REAL_LINK]]as''[[realLink]]asdf"
                     , "aa''aa[[NOT_REAL_LINK]]asfd''[[realLink]]"
                     , "  {{aaaa[[NOT_REAL_LINK]]aaaa}}aaa[[realLink]]"
                     , "nested Double Curly Brac{{  {{ }} [[NOT_REAL_LINK]] }} [[realLink]]"
                     , "  '  ' [[realLink]]"
                     , "()  ([[NOT_REAL_LINK]]   ) [[realLink]] "
                     , "No link in this line"
                     ]
