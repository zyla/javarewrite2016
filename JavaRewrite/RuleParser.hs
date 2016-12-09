module JavaRewrite.RuleParser where

import JavaRewrite.Rule
import Language.Java.Lexer
import Language.Java.Parser
import Text.Parsec
import Text.Parsec.Pos

type P = Parsec [L Token] ()

rules :: P [Rule]
rules = rule `sepBy` (tok SemiColon *> tok SemiColon)

rule :: P Rule
rule = 
  Rule
    <$> (try (forall *> many ident <* tok Period) <|> pure [])
    <*> Language.Java.Parser.exp
    <*> (tok LambdaArrow *> Language.Java.Parser.exp)

forall :: P ()
forall = javaToken (\tok ->
  case tok of IdentTok "forall" -> Just (); _ -> Nothing)

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
