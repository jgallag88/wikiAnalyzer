--Parse wikipedia page for links in specified wiki markup language

{-# LANGUAGE NoMonomorphismRestriction #-}

module PageParser (philoLink, lazyMany, WikiNode, node, WikiParseErr (BadFormat, NoLinks), errToText) where

import Text.Parsec
--TODO: compare performance of Text.Lazy
import Text.Parsec.Text
import qualified Data.Text as T
import Control.Applicative ((*>))

data WikiNode = Template [WikiNode]
                | Parenthetical [WikiNode]
                | Italics [WikiNode]
                | Ref [WikiNode]
                | Link [WikiNode]
                | Comment T.Text
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
            Right (Nothing, st) -> []
            Right (Just node, st) -> (Right node):(lm st)
            Left err -> [Left err]
            where p' = setParserState state >> (end <|> p'')
                  end = do
                            x <- eof 
                            state' <- getParserState 
                            return (Nothing, state')
                  p'' = do
                            x <- p
                            state' <- getParserState
                            return (Just x, state')
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
        (Right (Link [Content l])):_ -> Right l
    where links = filter notLink $ lazyMany node "Bad Format" page
          notLink (Right (Link [Content link])) = not $ beginsWithFile link
          notLink (Left _) = True --TODO: call this function something else b/c it allows Lefts
          notLink _ = False

beginsWithFile :: T.Text -> Bool
beginsWithFile x = case parse (string "File:") "" x of
                        Right _ -> True
                        Left _ -> False

--Build Abstract Syntax Tree of wikipedia page, using markup
wikiAST = many node

node = template 
        <|> italics
        <|> parenthetical
        <|> emptyRef
        <|> ref
        <|> comment
        <|> content
        <|> link

--Parsers for constructs we are interested in
template = fmap Template $ between (try rTmpltTok) (try lTmpltTok) wikiAST

italics = fmap Italics $ (try italTok) *> manyTill node (try italTok)

parenthetical = fmap Parenthetical $ between (try rParenTok) (try lParenTok) wikiAST

emptyRef = fmap Ref $ try $ string "<ref" *> many (noneOf "/>") *> string "/>" *> return []

ref = fmap Ref $ between (try openTag) (try rRefTok) wikiAST
    where openTag = lRefTok >> (many $ noneOf ">") >> char '>'

link = fmap Link $ between (try lLinkTok) (rLinkTok) (many (link <|> linkText))
    where linkText = untilKey $ map try [lLinkTok, rLinkTok]

comment = fmap (Comment . T.pack) $ between (try lCommentTok) (try rCommentTok) commentText
    where commentText = many (notFollowedBy rCommentTok *> anyChar)

--Any content until we reach one of the reserved keywords
content = untilKey reserved
    where reserved = map try [rTmpltTok, lTmpltTok, italTok, rParenTok, lParenTok, rLinkTok,
                              lLinkTok, rRefTok, lRefTok, lCommentTok]

untilKey keywords = fmap (Content . T.pack) $ many1 (notFollowedBy (choice keywords) *> anyChar)
    
--Wiki markup tokens
rTmpltTok = string "{{"
lTmpltTok = string "}}"
italTok = string "''"
rParenTok = string "("
lParenTok = string ")"
lRefTok = string "<ref"
rRefTok = string "</ref>"
lCommentTok = string "<!--"
rCommentTok = string "-->"
lLinkTok = string "[["
rLinkTok = string "]]"

getLinkTest :: [Either WikiParseErr T.Text]
getLinkTest = fmap (philoLink . T.pack) testList 
   
testList = ["['{  [[realLink]]"
             , "''asdf''daa''asdf[[NOT_REAL_LINK]]as''[[realLink]]asdf"
             , "aa''aa[[NOT_REAL_LINK]]asfd''[[realLink]]"
             , "  {{aaaa[[NOT_REAL_LINK]]aaaa}}aaa[[realLink]]"
             , "nested Double Curly Brac{{  {{ }} [[NOT_REAL_LINK]] }} [[realLink]]"
             , "  '  ' [[realLink]]"
             , "()  ([[NOT_REAL_LINK]]   ) [[realLink]] "
             , "<ref name=asdf> [[NOT_REAL_LINK]] </ref> [[realLink]]"
             , " <ref name=adsf/> [[realLink]]"
             , "  <!--This is just a comment) [[NOT_REAL_LINK]] --> [[realLink]]"
             , "No link in this line"
            ]
