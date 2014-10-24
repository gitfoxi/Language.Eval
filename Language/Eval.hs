
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Eval (expr, eval) where

import Data.Text
import Text.Parsec.Text
import Text.Parsec.Prim
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Char (satisfy, alphaNum, char, oneOf, letter)
import Text.Parsec.Language (emptyDef, javaStyle)
import Data.Functor.Identity
import Data.Bits


-- I really wanted to avoid this bit but in order to parse Text
-- it's necessary to define the full type of the language for the
-- lexer
ezLanguage :: GenLanguageDef Text [Integer] Identity
ezLanguage = LanguageDef {
      caseSensitive   = True
    , commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "#"
    , nestedComments  = False
    , identStart      = letter
    -- TODO: Only single-letter idents
    , identLetter     = letter -- satisfy $ const False 
    , opStart         = oneOf "<>+-&|"
    , opLetter        = oneOf "<>"
    , reservedNames   = names
    , reservedOpNames = ops
    }
  where
    names = []
    ops   = ["<<",">>", "&", "|", "+", "-", "*"]

lexer = makeTokenParser ezLanguage

expr :: Stream Text Identity Char => ParsecT Text [Integer] Identity Integer
expr    = buildExpressionParser table term
        <?> "expression"

substituteSymbol = do
        s <- lexeme lexer letter <?> "variable"
        let idx = fromEnum s - fromEnum 'a'
        u <- getState
        -- TODO: friendlier error when idx is out of the array
        -- now it throws an exception. Rather make it a parse
        -- error
        return $ u !! idx

term    =  parens lexer expr 
        <|> integer lexer
        <|> substituteSymbol
        <?> "simple expression"

shiftr :: Bits a => a -> Integer -> a
shiftr x b = shiftR x (fromIntegral b)

shiftl :: Bits a => a -> Integer -> a
shiftl x b = shiftL x (fromIntegral b)

table   = [ [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)]
          , [binary "<<" shiftl AssocLeft, binary ">>" shiftr AssocLeft ]
          , [binary "*" (*) AssocLeft, binary "/" div AssocLeft, binary "&" (.&.) AssocLeft ]
          , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
          , [binary "|" (.|.) AssocLeft ]
          ]

binary  name fun  = Infix (do{ reservedOp lexer name; return fun })
prefix  name fun  = Prefix (do{ reservedOp lexer name; return fun })
postfix name fun  = Postfix (do{ reservedOp lexer name; return fun })

-- Will substitute 1 for 'a', 2 for 'b'
test = runParser expr [1, 2] "" "(0xff & 0xf) << (a + b)"

eval :: Text -> [Integer] -> Integer
eval s vars = case runParser expr vars "" s of
        Left e -> error $ show e
        Right i -> i
