--Parse wikipedia page for links in specified wiki markup language

{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Parsec
import qualified Data.Text as T
import Control.Applicative ((*>))

data WikiNode = Template [WikiNode]
                | Parenthetical [WikiNode]
                | Italics [WikiNode]
                | Link WikiNode
                | Content String deriving Show

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

------ Link Extractor ----- 
-- Get the first top level link from the wiki AST
philoLink :: T.Text -> Either ParseError WikiNode
philoLink page = fmap firstLink $ parse wikiAST "Bad wiki format" page
    where firstLink = head . filter notLink
          notLink (Link _) = True
          notLink _ = False

--Build Abstract Syntax Tree of wikipedia page, using markup
wikiAST = many node

node = template 
        <|> italics'
        <|> parenthetical'
        <|> content
        <|> link'

template = fmap Template $ between (try rTmpltTok) (try lTmpltTok) wikiAST
italics' = fmap Italics $ (try italTok) *> manyTill node (try italTok)
parenthetical' = fmap Parenthetical $ between (try rParenTok) (try lParenTok) wikiAST
link' = fmap Link $ between (try rLinkTok) (lLinkTok) content 
content = fmap Content $ many1 (notFollowedBy (choice reserved) *> anyChar)
 
--Wiki markup tokens
rTmpltTok = string "{{"
lTmpltTok = string "}}"
italTok = string "''"
rParenTok = string "("
lParenTok = string ")"
rLinkTok = string "[["
lLinkTok = string "]]"

reserved = [rTmpltTok, lTmpltTok, italTok, rParenTok, lParenTok, rLinkTok, lLinkTok]

getLink :: T.Text -> Either ParseError T.Text
getLink = parse firstLink "No Links"

firstLink = do  
    beforeLink 
    link <- many $ noneOf "]#|"
    endLink
    return $ T.pack link
  
beforeLink = manyTill (many notLink) (try $ string "[[") 

notLink = italics
      <|> doubleCurlyBrac
      <|> parenthetical 
      <|> (many1 normalText)

normalText = noneOf "'[{("
           <|> notFollowedByItself '['
           <|> notFollowedByItself '\''
           <|> notFollowedByItself '{'

notFollowedByItself c = try ( do x <- char c
                                 notFollowedBy $ char c
                                 return x)
 
italics = between (try $ string "''") (try $ string "''") (many $ noneOf "'")
doubleCurlyBrac = (try $ string "{{") >> manyTill (notLink) (try $ string "}}") >> return []
--doubleCurlyBrac = manyTill middleChars (try $ string "}}")
--    where middleChars = many 
parenthetical = between (try $ char '(') (try $ char ')') (many $ noneOf ")")

endLink = optional (char '#' >> (many $ noneOf "]|")) >> 
            optional (char '|' >> optional (many $ noneOf "]")) >>
            string "]]"

getLinkTest :: [Either ParseError WikiNode]
getLinkTest = fmap (philoLink . T.pack) testList 
    where testList = ["['{  [[realLink]]"
                     , "''asdf''daa''asdf[[NOT_REAL_LINK]]as''[[realLink]]asdf"
                     , "aa''aa[[NOT_REAL_LINK]]asfd''[[realLink]]"
                     , "  {{aaaa[[NOT_REAL_LINK]]aaaa}}aaa[[realLink]]"
                     , "nested Double Curly Brac{{  {{ }} [[NOT_REAL_LINK]] }} [[realLink]]"
                     , "  '  ' [[realLink]]"
                     , "()  ([[NOT_REAL_LINK]]   ) [[realLink]] "
                     ]

