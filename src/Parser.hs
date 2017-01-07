module Parser
  (Parser, runParser, parseTm, parseTy) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Control.Monad.Reader as R
import qualified Data.List as L
import qualified Data.Map as M

import Text.Parsec ((<|>),(<?>))

import Lexer (Token(..), Tk(..), getTokenTk, getTokenSourcePos, lexTokens)
import AST (Ty(..), Tm(..), Context, findInContext, Label, Binding)



type Parser a = P.ParsecT [Token] () (R.Reader Context) a

tk :: Tk -> Parser ()
tk t = tk' $ \x -> if x==t then Just () else Nothing

tk' :: (Tk -> Maybe a) -> Parser a
tk' testTk = P.tokenPrim showToken updatePos testToken
  where
    showToken = show . getTokenTk
    updatePos _ t _ = getTokenSourcePos t
    testToken = (testTk . getTokenTk)

tkIdent :: Parser String
tkIdent = tk' testTk where
  testTk (TkIdent ident) = Just ident
  testTk _ = Nothing


braces :: Parser a -> Parser a
braces = P.between (tk TkBraceOpen) (tk TkBraceClose)

parens :: Parser a -> Parser a
parens = P.between (tk TkParenOpen) (tk TkParenClose)

parseTm :: Parser Tm
parseTm = parseTmApp

parseTmApp :: Parser Tm
parseTmApp = do
  tms <- P.many1 parseTmNoApp
  return $ foldl1 TmApp tms

parseTmNoApp :: Parser Tm
parseTmNoApp = parseTmProj

parseTmProj :: Parser Tm
parseTmProj = do
  tm <- parseTmNoProj
  labels <- P.many (tk TkDot *> tkIdent)
  return $ foldl TmProj tm labels

parseTmNoProj :: Parser Tm
parseTmNoProj =
  parseTmAbs <|>
  parseTmRec <|>
  parseTmVar <|>
  parseTmBool <|>
  parseTmIf <|>
  (parens parseTm)

parseTmAbs :: Parser Tm
parseTmAbs = do
  tk TkFun
  (ident,ty) <- parseBinding
  tk TkFunArr
  body <- R.local ((ident,ty):) parseTm
  return $ TmAbs ident ty body

parseTmRec :: Parser Tm
parseTmRec = braces parseAssignments
  where
    parseAssignments = TmRec . M.fromList <$> P.sepBy parseAssignment (tk TkComma)

parseTmVar :: Parser Tm
parseTmVar = do
  ident <- tkIdent
  binding <- R.asks (findInContext ident)
  maybe (P.unexpected $ "identifier "++ident++" not in context") (return . TmVar) binding

parseTmBool :: Parser Tm
parseTmBool =
  TmBool True <$ tk TkTrue <|>
  TmBool False <$ tk TkFalse

parseTmIf :: Parser Tm
parseTmIf = tk TkIf *> pure TmIf <*> parseTm <* tk TkThen <*> parseTm <* tk TkElse <*> parseTm <* tk TkEnd


parseTy :: Parser Ty
parseTy = parseTyArr

parseTyArr :: Parser Ty
parseTyArr = do
  tys <- P.sepBy1 parseTyNoArr (tk TkTyArr)
  return $ foldr1 TyArr tys

parseTyNoArr :: Parser Ty
parseTyNoArr = parseTyTop <|> parseTyBool <|> parseTyRec <|> (parens parseTy)

parseTyTop :: Parser Ty
parseTyTop = TyTop <$ tk TkTop

parseTyBool :: Parser Ty
parseTyBool = TyBool <$ tk TkBool

parseTyRec :: Parser Ty
parseTyRec = braces parseJudgements
  where
    parseJudgements = TyRec . M.fromList <$> P.sepBy parseBinding (tk TkComma)

parseBinding :: Parser Binding
parseBinding = (,) <$> tkIdent <* tk TkColon <*> P.try parseTy

parseAssignment :: Parser (Label, Tm)
parseAssignment = (,) <$> tkIdent <* tk TkEq <*> P.try parseTm

runParser :: Parser a -> String -> Context -> String -> Either P.ParseError a
runParser parser sourceName context input = do
  tokens <- (P.runParser lexTokens () sourceName input)
  R.runReader (P.runParserT parser () sourceName tokens) context
