module JavaRewrite.RuleParser (
    P, rules, rule, pattern
  , Text.Parsec.eof
  , Text.Parsec.parse
) where

import JavaRewrite.Rule
import Language.Java.Lexer
import Language.Java.Parser
import Text.Parsec
import Text.Parsec.Pos

type P = Parsec [L Token] ()

rules :: P [Rule]
rules = many (rule <* opt (tok SemiColon *> tok SemiColon))

rule :: P Rule
rule = 
  Rule
    <$> pattern
    <*> (tok LambdaArrow *> Language.Java.Parser.exp)

pattern :: P Pattern
pattern =
  Pattern
    <$> (try (forall *> many metavariable <* tok Period) <|> pure [])
    <*> Language.Java.Parser.exp

-- | Parse the keyword @forall@.
forall :: P ()
forall = javaToken (\tok ->
  case tok of IdentTok "forall" -> Just (); _ -> Nothing)

metavariable :: P Metavariable
metavariable =
  (Metavariable <$> ident <*> pure Nothing) <|>
  parens (Metavariable <$> ident <*> (colon *> (Just <$> expressionType)))

parens p = tok OpenParen *> p <* tok CloseParen

expressionType :: P ExpressionType
expressionType = javaToken $ \tok ->
  case tok of
    IdentTok str | [(ty, "")] <- reads str
      -> Just ty
    _ -> Nothing

-- the following stolen from Language.Java.Parser

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c
