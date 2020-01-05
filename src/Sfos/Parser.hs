module Sfos.Parser
  ( ParseException
  , parseTerm
  , parseType
  , parseKind
  , parseTermAtom
  , parseTypeAtom
  , parseKindAtom
  )
where

import           Sfos.Syntax
import           Sfos.Support
import           Control.Applicative
import           Data.Maybe
import           Text.Trifecta
import           Text.Parser.Expression
import           Text.Parser.Token.Highlight
import           Data.HashSet                  as H
import           Control.Exception       hiding ( try )

data ParseException = ExcNameNotInContext String TypingContext

instance Exception ParseException

instance Show ParseException where
  show (ExcNameNotInContext name ctx) =
    "Name '" ++ name ++ "' is not in context: " ++ show ctx

parseTerm :: Parser (TypingContext -> Term)
parseType :: Parser (TypingContext -> Type)
parseKind :: Parser (TypingContext -> Kind)
parseTerm = buildExpressionParser termOperatorTable parseTermAtom <?> "term"
parseType = buildExpressionParser typeOperatorTable parseTypeAtom <?> "type"
parseKind = buildExpressionParser kindOperatorTable parseKindAtom <?> "kind"

parseTermAtom :: Parser (TypingContext -> Term)
parseTermAtom =
  parens parseTerm
    <|> (   (\x ctx -> TmVar
              (fromMaybe (throw $ ExcNameNotInContext x ctx) (tmNameToIndex ctx x))
              (length ctx)
            )
        <$> ident termIdentStyle
        )
    <?> "atom term"

termOperatorTable :: OperatorTable Parser (TypingContext -> Term)
termOperatorTable =
  [ [Postfix (chainedPostfix tmTyApp)]
  , [Infix tmApp AssocLeft]
  , [Prefix (chainedPrefix (tmAbs <|> tmTyAbs <|> tmLet <|> tmTyLet))]
  ]
 where
  tmApp   = return (\l r ctx -> TmApp (l ctx) (r ctx))
  tmTyApp = (\ty t ctx -> TmTyApp (t ctx) (ty ctx)) <$> brackets parseType
  tmAbs   = do
    x  <- try $ lambda termIdentStyle *> ident termIdentStyle
    ty <- between (reservedOp ":") (reservedOp ".") parseType
    return
      (\t ctx ->
        let ctx' = TyAsmHasType x (ty ctx) : ctx in TmAbs x (ty ctx) (t ctx')
      )
  tmTyAbs = do
    x     <- try $ lambda termIdentStyle *> ident typeIdentStyle
    bound <- between (reservedOp "<:") (reservedOp ".") parseType
    return
      (\t ctx ->
        let ctx' = TyAsmSubtype x (bound ctx) : ctx
        in  TmTyAbs x (bound ctx) (t ctx')
      )
  tmLet = do
    x  <- try $ reserve termIdentStyle "let" *> ident termIdentStyle
    t1 <- between (reservedOp "=") (reserve termIdentStyle "in") parseTerm
    return
      (\t ctx ->
        let ctx' = TyAsmHasType x TyTop : ctx in TmLet x (t1 ctx) (t ctx')
      )
  tmTyLet = do
    x   <- try $ reserve termIdentStyle "let" *> ident typeIdentStyle
    ty1 <- between (reservedOp "=") (reserve termIdentStyle "in") parseType
    return
      (\t ctx ->
        let ctx' = TyAsmSubtype x TyTop : ctx in TmTyLet x (ty1 ctx) (t ctx')
      )

parseTypeAtom :: Parser (TypingContext -> Type)
parseTypeAtom =
  parens parseType
    <|> (const TyTop <$ reserve typeIdentStyle "Top")
    <|> (   (\x ctx -> TyVar
              (fromMaybe (throw $ ExcNameNotInContext x ctx) (tyNameToIndex ctx x))
              (length ctx)
            )
        <$> ident typeIdentStyle
        )
    <?> "atom type"

typeOperatorTable :: OperatorTable Parser (TypingContext -> Type)
typeOperatorTable =
  [ [Infix tyApp AssocLeft]
  , [Infix tyArrow AssocRight]
  , [Prefix (chainedPrefix (tyAll <|> tyAbs))]
  ]
 where
  tyApp   = return (\l r ctx -> TyApp (l ctx) (r ctx))
  tyArrow = (\l r ctx -> TyArrow (l ctx) (r ctx)) <$ reservedOp "->"
  tyAll   = do
    x     <- forall *> ident typeIdentStyle
    bound <- between (reservedOp "<:") (reservedOp ".") parseType
    return
      (\ty ctx ->
        let ctx' = TyAsmSubtype x (bound ctx) : ctx
        in  TyAll x (bound ctx) (ty ctx')
      )
  tyAbs = do
    x    <- lambda typeIdentStyle *> ident typeIdentStyle
    kind <- between (reservedOp "::") (reservedOp ".") parseKind
    return
      (\ty ctx ->
        let top  = topKind (kind ctx)
            ctx' = TyAsmSubtype x top : ctx
        in  TyAbs x (kind ctx) (ty ctx')
      )

parseKindAtom :: Parser (TypingContext -> Kind)
parseKindAtom =
  parens parseKind <|> (const KProper <$ symbolic '*') <?> "atom kind"

kindOperatorTable :: OperatorTable Parser (TypingContext -> Kind)
kindOperatorTable = [[Infix kArrow AssocRight]]
  where kArrow = (\l r ctx -> KArrow (l ctx) (r ctx)) <$ reservedOp "=>"

keywords :: [String]
keywords = ["lambda", "forall", "let", "in"]
reservedTermIdents :: H.HashSet String
reservedTermIdents = H.fromList ([] ++ keywords)
reservedTypeIdents :: H.HashSet String
reservedTypeIdents = H.fromList (["Top"] ++ keywords)
reservedOps :: H.HashSet String
reservedOps = H.fromList ([":", "<:", "::", ".", "->", "=>", "="] ++ keywords)

termIdentStyle :: IdentifierStyle Parser
termIdentStyle = IdentifierStyle { _styleName              = "term identifer"
                                 , _styleStart             = lower
                                 , _styleLetter            = alphaNum
                                 , _styleReserved          = reservedTermIdents
                                 , _styleHighlight         = Identifier
                                 , _styleReservedHighlight = ReservedIdentifier
                                 }

typeIdentStyle :: IdentifierStyle Parser
typeIdentStyle = IdentifierStyle { _styleName              = "type identifer"
                                 , _styleStart             = upper
                                 , _styleLetter            = alphaNum
                                 , _styleReserved          = reservedTypeIdents
                                 , _styleHighlight         = Identifier
                                 , _styleReservedHighlight = ReservedIdentifier
                                 }

opsStyle :: IdentifierStyle Parser
opsStyle = IdentifierStyle { _styleName              = "operator"
                           , _styleStart             = _styleLetter opsStyle
                           , _styleLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                           , _styleReserved          = reservedOps
                           , _styleHighlight         = Operator
                           , _styleReservedHighlight = ReservedOperator
                           }

reservedOp :: String -> Parser ()
reservedOp = reserve opsStyle

lambda :: IdentifierStyle Parser -> Parser ()
lambda style =
  reservedOp "\\" <|> reservedOp "\x03BB" <|> reserve style "lambda"

forall :: Parser ()
forall = reservedOp "\x2200" <|> reserve typeIdentStyle "forall"

chainedPrefix :: Parser (a -> a) -> Parser (a -> a)
chainedPrefix p = chainl1 p (return (.))

chainedPostfix :: Parser (a -> a) -> Parser (a -> a)
chainedPostfix p = chainr1 p (return $ flip (.))
