--Parse wikipedia page for links in specified wiki markup language

import Text.Parsec
import qualified Data.Text as T

getLink :: T.Text -> Either ParseError T.Text  
getLink = parse pageParser "No Link"

pageParser = fmap T.pack $ many $ noneOf "{}"
