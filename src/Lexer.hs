module Lexer
  ( Tk(..)
  , Token(..)
  , Lexer
  , getTokenTk
  , getTokenSourcePos
  , lexTokens) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

import Control.Applicative ((<|>))

data Tk
  = TkBraceOpen
  | TkBraceClose
  | TkParenOpen
  | TkParenClose
  | TkColon
  | TkEq
  | TkComma
  | TkDot
  | TkFun
  | TkFunArr
  | TkTyArr
  | TkTop
  | TkIdent String
  | TkBool
  | TkTrue
  | TkFalse
  | TkIf
  | TkThen
  | TkElse
  | TkCase
  | TkOf
  | TkPipe
  | TkEnd
    deriving (Show, Eq)

data Token = Token Tk P.SourcePos deriving (Eq)

instance Show Token where
  show (Token tk _) = show tk

getTokenTk :: Token -> Tk
getTokenTk (Token tk _) = tk

getTokenSourcePos :: Token -> P.SourcePos
getTokenSourcePos (Token _ sp) = sp

type Lexer a = P.Parsec String () a

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ P.LanguageDef
  { P.commentStart = "{-"
  , P.commentEnd = "-}"
  , P.commentLine = "--"
  , P.nestedComments = False
  , P.identStart = P.letter <|> P.char '_'
  , P.identLetter = P.alphaNum <|> P.char '_'
  , P.opStart = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedNames =
      [ "fun", "Top", "Bool", "true", "false", "if", "then", "else", "end", "case", "of" ]
  , P.reservedOpNames =
      [ "=", ":", "=>", "->", "|" ]
  , P.caseSensitive = True }

lexeme :: Lexer a -> Lexer a
lexeme = P.lexeme lexer

colon :: Lexer String
colon = P.colon lexer

comma :: Lexer String
comma = P.comma lexer

reservedOp :: String -> Lexer ()
reservedOp = P.reservedOp lexer

reserved :: String -> Lexer ()
reserved = P.reserved lexer

identifier :: Lexer String
identifier = P.identifier lexer

lexTokenType :: Lexer Tk
lexTokenType =
  TkBraceOpen <$ lexeme (P.char '{') <|>
  TkBraceClose <$ lexeme (P.char '}') <|>
  TkParenOpen <$ lexeme (P.char '(') <|>
  TkParenClose <$ lexeme (P.char ')') <|>
  TkColon <$ colon <|>
  TkEq <$ reservedOp "=" <|>
  TkDot <$ lexeme (P.char '.') <|>
  TkComma <$ comma <|>
  TkFun <$ reserved "fun" <|>
  TkFunArr <$ reservedOp "=>" <|>
  TkTyArr <$ reservedOp "->" <|>
  TkTop <$ reserved "Top" <|>
  TkBool <$ reserved "Bool" <|>
  TkIdent <$> identifier <|>
  TkTrue <$ reserved "true" <|>
  TkFalse <$ reserved "false" <|>
  TkIf <$ reserved "if" <|>
  TkThen <$ reserved "then" <|>
  TkElse <$ reserved "else" <|>
  TkCase <$ reserved "case" <|>
  TkOf <$ reserved "of" <|>
  TkPipe <$ reservedOp "|" <|>
  TkEnd <$ reserved "end"

lexToken :: Lexer Token
lexToken = Token <$> lexTokenType <*> P.getPosition

lexTokens :: Lexer [Token]
lexTokens = P.many lexToken <* P.eof
