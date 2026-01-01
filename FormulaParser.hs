module FormulaParser where

import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isLower, isUpper)

-- | Term data type representing variables or constants
data Term
  = Var Char                -- Variables (e.g., x, y, z)
  | Const Char              -- Constants (e.g., a, b, c)
  deriving (Show, Eq)

-- | Predicate formula data type
data PredFormula
  = Predicate Char [Term]           -- Predicate symbol with a list of terms (e.g., F(x), R(x,y))
  | Boolean Bool                    -- Boolean constants as 0-place predicates (e.g., True, False)
  | Not PredFormula                 -- Negation (¬)
  | And PredFormula PredFormula     -- Conjunction (∧)
  | Or PredFormula PredFormula      -- Disjunction (∨)
  | Implies PredFormula PredFormula -- Implication (→)
  | ForAll Char PredFormula         -- Universal quantifier (∀x)
  | Exists Char PredFormula         -- Existential quantifier (∃x)
  deriving (Show, Eq)

-- Define the logical symbols
orT, andT, impliesT, notT, forallT, existsT :: String
orT = "∨"
andT = "∧"
impliesT = "→"
notT = "¬"
forallT = "∀"
existsT = "∃"

-- Token function to skip spaces
token :: ReadP a -> ReadP a
token p = skipSpaces *> p <* skipSpaces

-- Parse negation and quantifiers (both bind tightly)
parseUnary :: ReadP PredFormula
parseUnary = parseNegation <++ parseQuantifier <++ parsePredicate <++ parens parseBinary

-- Parse negation (unary operator)
parseNegation :: ReadP PredFormula
parseNegation = Not <$> (token (string notT) *> parseUnary)

-- Parse quantifiers (universal ∀ and existential ∃), ensuring quantifiers bind tightly
parseQuantifier :: ReadP PredFormula
parseQuantifier = parseForAll <++ parseExists

-- Parse universal quantifier (∀x), binds tightly unless parentheses
parseForAll :: ReadP PredFormula
parseForAll = do
  token (string forallT)  -- Parse ∀
  var <- satisfy isLower  -- Parse variable x
  formula <- parseUnary   -- Quantifiers bind tightly to the next subformula
  return $ ForAll var formula

-- Parse existential quantifier (∃x), binds tightly unless parentheses
parseExists :: ReadP PredFormula
parseExists = do
  token (string existsT)  -- Parse ∃
  var <- satisfy isLower  -- Parse variable x
  formula <- parseUnary   -- Quantifiers bind tightly to the next subformula
  return $ Exists var formula

-- Parse binary connectives (left-associative for ∧ and ∨, right-associative for →)
parseBinary :: ReadP PredFormula
parseBinary = chainr1 parseAndOr (parseOp impliesT Implies)

-- Parse conjunction (∧) and disjunction (∨)
parseAndOr :: ReadP PredFormula
parseAndOr = chainl1 parseUnary ((parseOp andT And) <++ (parseOp orT Or))

-- Helper to parse binary operators
parseOp :: String -> (PredFormula -> PredFormula -> PredFormula) -> ReadP (PredFormula -> PredFormula -> PredFormula)
parseOp symbol op = token (string symbol) *> pure op

-- Parse atomic predicates (e.g., P(x), R(x,y))
parsePredicate :: ReadP PredFormula
parsePredicate = do
  predSym <- satisfy isUpper           -- Parse predicate symbol (e.g., P)
  terms <- many parseTerm              -- Parse the terms (variables/constants)
  return $ Predicate predSym terms

-- Parse a term (either a variable or constant)
parseTerm :: ReadP Term
parseTerm = parseVar <++ parseConst

-- Parse a variable (lowercase letter)
parseVar :: ReadP Term
parseVar = Var <$> satisfy isLower

-- Parse a constant (lowercase letter)
parseConst :: ReadP Term
parseConst = Const <$> satisfy isLower

-- Parse parentheses for grouping
parens :: ReadP PredFormula -> ReadP PredFormula
parens p = token (char '(') *> p <* token (char ')')

-- Parse full formula, starting with the binary connectives
parsePredFormula :: ReadP PredFormula
parsePredFormula = parseBinary <++ parens parsePredFormula

-- Run the parser on a given string
runParser :: String -> Either String PredFormula
runParser input =
  case readP_to_S (parsePredFormula <* eof) input of
    [(result, "")] -> Right result
    _              -> Left ("Failed to parse formula: " ++ input)






