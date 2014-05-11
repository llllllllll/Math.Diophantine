{
module Math.Diophantine.Grammar
    ( parseRawEquation -- :: String -> EqParser Equals
    , Equals(..)       -- :: instances: Show
    , ParseError(..)   -- :: instances: Show
    ) where

import Data.Char (isDigit,isSpace)
}

%name parseRawEquation
%monad { EqParser }
%tokentype { Token }
%error { parseError }

%token
    'x' { TokenX }
    'X' { TokenX }
    'y' { TokenY }
    'Y' { TokenY }
    '=' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '^' { TokenExp }
    '(' { TokenO }
    ')' { TokenC }
    int { TokenInt $$ }

%%

Equals   : Expr '=' Expr        { Equals $1 $3 }

Expr     : Expr '+' Term        { Plus $1 $3 }
         | Expr '-' Term        { Minus $1 $3 }
         | Term                 { ETerm $1 }

Term     : int                  { Constant $1 }
         | Variable             { Variable 1 $1 1 }
         | Variable '^' int     {% if $3 `notElem` [1,2]
                                     then Invalid PowerOutOfBounds
                                     else return $ Variable 1 $1 $3 }
         | int Variable         { Variable $1 $2 1 }
         | int Variable '^' int {% if $4 `notElem` [1,2]
                                     then Invalid PowerOutOfBounds
                                     else return $ Variable $1 $2 $4 }

Variable : 'x'                  { XTerm }
         | 'X'                  { XTerm }
         | 'y'                  { YTerm }
         | 'Y'                  { YTerm }
         | '(' 'x' 'y' ')'      { XYTerm }
         | '(' 'X' 'y' ')'      { XYTerm }
         | '(' 'x' 'Y' ')'      { XYTerm }
         | '(' 'X' 'Y' ')'      { XYTerm }
         | '(' 'y' 'x' ')'      { XYTerm }
         | '(' 'Y' 'x' ')'      { XYTerm }
         | '(' 'y' 'X' ')'      { XYTerm }
         | '(' 'Y' 'X' ')'      { XYTerm }

{
-- | Function to invoke in case of an error.
parseError :: [Token] -> EqParser a
parseError _ = Invalid BadGrammar

-- | The equation parsing monad.
data EqParser a = Valid a
                | Invalid ParseError
                  deriving Show

-- | The types of parse erros that can occur.
data ParseError = PowerOutOfBounds -- ^ We are only solving quadratics.
                | BadGrammar       -- ^ Not a valid equation type.
                  deriving Show

-- | Pretty print instance for 'ParseError's.
instance Show ParseError where
    show PowerOutOfBounds = "Power out of bounds"
    show BadGrammar       = "Bad equation grammar"

-- | Monad instance for the parser.
instance Monad EqParser where
    return t            = Valid t
    (>>=) (Valid v) f   = f v
    (>>=) (Invalid i) _ = Invalid i

-- | The main equality statement.
data Equals = Equals Expr Expr deriving Show

-- | An expression that lives on one side of an equality statement.
data Expr = Plus Expr Term   -- ^ Addition.
          | Minus Expr Term  -- ^ Subtraction.
          | ETerm Term       -- ^ A single expression as a term.
            deriving Show

data Term = Constant Int              -- ^ Constant terms.
          | Variable Int VarTerm Int  -- ^ Terms with variables.
            deriving Show

data VarTerm = XTerm  -- ^ Terms with x.
             | YTerm  -- ^ Terms with y.
             | XYTerm -- ^ terms with both x and y.
               deriving Show

-- | The token types.
data Token = TokenX       -- ^ 'x' | 'X'
           | TokenY       -- ^ 'y' | 'X'
           | TokenEq      -- ^ '='
           | TokenPlus    -- ^ '+'
           | TokenMinus   -- ^ '-'
           | TokenExp     -- ^ '^'
           | TokenInt Int -- ^ Integers
           | TokenO       -- ^ '('
           | TokenC       -- ^ ')'
             deriving Show

-- | A basic lexing function.
lexer :: String -> [Token]
lexer [] = []
lexer str@(c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum str
lexer ('x':cs) = TokenX     : lexer cs
lexer ('X':cs) = TokenX     : lexer cs
lexer ('y':cs) = TokenY     : lexer cs
lexer ('Y':cs) = TokenY     : lexer cs
lexer ('=':cs) = TokenEq    : lexer cs
lexer ('+':cs) = TokenPlus  : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('^':cs) = TokenExp   : lexer cs
lexer ('(':cs) = TokenO     : lexer cs
lexer (')':cs) = TokenC     : lexer cs

-- | Lexes the 'TokenInt's.
lexNum :: String -> [Token]
lexNum cs = let (num,rest) = span isDigit cs
            in TokenInt (read num) : lexer rest
}
