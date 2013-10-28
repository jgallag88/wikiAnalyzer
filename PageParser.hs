--Parse wikipedia page for links in specified wiki markup language

{-# LANGUAGE NoMonomorphismRestriction #-}

module PageParser (philoLink, WikiParseErr (BadFormat, NoLinks), errToText) where

import Text.Parsec
--TODO: compare performance of Text.Lazy
import Text.Parsec.Text
import qualified Data.Text as T
import Control.Applicative ((*>))

data WikiNode = Template [WikiNode]
                | Parenthetical [WikiNode]
                | Italics [WikiNode]
                | Link [WikiNode]
                | Content T.Text deriving Show

data WikiParseErr = BadFormat T.Text
                    | NoLinks deriving Show

errToText (BadFormat errMsg) = errMsg
errToText NoLinks = T.pack "No Links"

lazyMany p filename contents = lm state0
    where
        state0 = case parse getParserState filename contents of -- get an initial state
            Right s -> s
            Left e -> error $ show e
        lm state = case parse p' "" $ T.pack "" of
            Right ([], st) -> []
            Right (node, st) -> (Right node):(lm st)
            Left err -> [Left err]
            where p' = setParserState state >> (end <|> p'')
                  end = do
                            eof 
                            state' <- getParserState 
                            return ([], state')
                  p'' = do
                            x <- p
                            state' <- getParserState
                            return (x, state')
{--
lazyMany :: Parser a -> SourceName -> T.Text -> [Either ParseError a]
lazyMany p filename contents = lm state0
    where
        state0 = case parse getParserState filename contents of -- get an initial state
            Right s -> s
            Left e -> error $ show e
        lm state = case parse p' "" $ T.pack "" of
                Right [] st -> [Right []]
                Right node st -> node:(lm state')
                Left err -> [Left err]
        p' = setParserState state >> choice [eof >> return [],
                do
                    x <- p
                    state' <- getParserState
                    return (x:lm state')]
--}
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

--TODO: Make parser fully lazy by eliminating Either (for robustness, and maybe speed)
-- Get the first top level link from the wiki AST
philoLink :: T.Text -> Either WikiParseErr T.Text
philoLink page = 
    case links of
        [] -> Left NoLinks
        (Left err):_ -> Left . BadFormat $ (T.pack . show) err
        (Right l):_ -> Right . linkText $ l
    where links = filter notLink $ lazyMany wikiAST "Bad Format" page
          notLink (Right [(Link _)]) = True
          notLink (Left _) = True --TODO: call this function something else b/c it allows Lefts
          notLink _ = False
          linkText [(Link [Content link])] = link

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

link = fmap Link $ between (try rLinkTok) (lLinkTok) wikiAST 

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
   
testList = ["['{  [[realLink]]"
             , "''asdf''daa''asdf[[NOT_REAL_LINK]]as''[[realLink]]asdf"
             , "aa''aa[[NOT_REAL_LINK]]asfd''[[realLink]]"
             , "  {{aaaa[[NOT_REAL_LINK]]aaaa}}aaa[[realLink]]"
             , "nested Double Curly Brac{{  {{ }} [[NOT_REAL_LINK]] }} [[realLink]]"
             , "  '  ' [[realLink]]"
             , "()  ([[NOT_REAL_LINK]]   ) [[realLink]] "
             , "No link in this line"
            ]
