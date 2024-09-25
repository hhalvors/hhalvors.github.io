{-# LANGUAGE OverloadedStrings #-}

module LemmonFilter (applyLemmonFilter) where

import Text.Pandoc.JSON
import Text.Pandoc.Definition (Block, Pandoc)  -- Import Block and Pandoc here
import Text.Pandoc.Walk (walk)  -- Import walk here
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import Data.List (intercalate)

-- | Apply the Lemmon filter to a Pandoc document
applyLemmonFilter :: Pandoc -> Pandoc
applyLemmonFilter = walk lemmonFilter  -- Walk through the document applying lemmonFilter

-- | Term data type representing variables or constants
data Term
  = Var Char                -- Variables (e.g., x, y, z)
  | Const Char              -- Constants (e.g., a, b, c)
  deriving (Show, Eq)

-- | Predicate formula data type
data PredFormula
  = Predicate Char [Term]           -- Predicate symbol with a list of terms (e.g., F(x), R(x,y))
  | Not PredFormula                 -- Negation (¬)
  | And PredFormula PredFormula     -- Conjunction (∧)
  | Or PredFormula PredFormula      -- Disjunction (∨)
  | Implies PredFormula PredFormula -- Implication (→)
  | ForAll Char PredFormula         -- Universal quantifier (∀x)
  | Exists Char PredFormula         -- Existential quantifier (∃x)
  deriving (Show, Eq)

-- | Data type representing a single proof line.
data ProofLine = ProofLine
  { references   :: [Int]        -- List of step references
  , lineNumber   :: Int          -- Line number
  , formula      :: PredFormula  -- Logical formula (now generalized to PredFormula)
  , justification :: String      -- Justification (A, ∧I, RA, DN, etc.)
  } deriving (Show, Eq)

-- | A proof is a list of proof lines.
type Proof = [ProofLine]

-- | Lemmon filter to process Lemmon-style proofs in the Pandoc AST
lemmonFilter :: Block -> Block
lemmonFilter (CodeBlock (id, classes, attrs) content)
  | "lemmon" `elem` classes =
      let proof = parseLemmonProof (T.unpack content)  -- Parse content into proof
      in RawBlock "html" (T.pack (renderHTML proof))   -- Render HTML directly
lemmonFilter blk = blk  -- Return the block unchanged if not "lemmon"

-- parsing human written input

parseLemmonProof :: String -> Proof
parseLemmonProof content =
  let linesOfProof = lines content
      parsedProof = map parseProofLine linesOfProof
  in trace ("Parsed proof: " ++ show parsedProof) parsedProof

-- Parse a proof line
parseProofLine :: String -> ProofLine
parseProofLine line =
  let lineTrimmed = dropWhile (== ' ') line  -- Remove leading spaces
      -- Split at first '(' to separate references from line number
      (beforeParen, afterParen) = break (== '(') lineTrimmed
      -- Check if there are references before the parentheses, otherwise set to empty list
      references = if null (words beforeParen)
                     then []
                     else case reads ("[" ++ beforeParen ++ "]") :: [( [Int], String)] of
                            [(refs, _)] -> refs
                            _           -> error ("Failed to parse references: " ++ beforeParen)
      -- Find the line number inside parentheses
      (lineNumStr, rest) = break (== ')') (drop 1 afterParen)
      lineNum = case reads lineNumStr :: [(Int, String)] of
                  [(n, _)] -> n
                  _        -> error ("Failed to parse line number: " ++ show lineNumStr)
      -- Skip any spaces after the closing parenthesis
      rest' = dropWhile (== ' ') (drop 1 rest)
      -- Split at the first space to isolate the formula from the justification
      (formula, justification) = break (== ' ') rest'
      parsedFormula = parsePredExpr formula
  in ProofLine references lineNum parsedFormula (dropWhile (== ' ') justification)
  



--- Parsing logic

-- | Parse a complete predicate logic expression.
parsePredExpr :: String -> PredFormula
parsePredExpr ('¬':xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Negation applies to the entire next expression
    in Not subExpr `combinePredExpr` rest
parsePredExpr ('∀':x:xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Universal quantifier applies to the entire next expression
    in ForAll x subExpr `combinePredExpr` rest
parsePredExpr ('∃':x:xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Existential quantifier applies to the entire next expression
    in Exists x subExpr `combinePredExpr` rest
parsePredExpr ('(':xs) = 
    let (subExpr, rest) = parseParenPredExpr ('(':xs) in combinePredExpr subExpr rest  -- Parenthesized expression
parsePredExpr str = parseSimplePredExpr str -- Handle simple expressions

-- | Parse a unary expression, which can be negation, universal, or existential quantification.
parseUnaryExpr :: String -> (PredFormula, String)
parseUnaryExpr ('¬':xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Negation can apply recursively
    in (Not subExpr, rest)
parseUnaryExpr ('∀':x:xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Universal quantifier applies recursively
    in (ForAll x subExpr, rest)
parseUnaryExpr ('∃':x:xs) = 
    let (subExpr, rest) = parseUnaryExpr xs  -- Existential quantifier applies recursively
    in (Exists x subExpr, rest)
parseUnaryExpr ('(':xs) = parseParenPredExpr ('(':xs)  -- If parentheses are present, parse within them
parseUnaryExpr str = parseSimplePredExprWithRest str  -- Otherwise, it's a simple expression

-- | Parse a simple predicate expression and return the rest of the string.
parseSimplePredExprWithRest :: String -> (PredFormula, String)
parseSimplePredExprWithRest str =
    let (left, rest) = span (`notElem` ['∧', '∨', '→']) str -- Split on the first connective
    in case rest of
        ('∧':more) -> (And (parseSimplePredExpr left) (parsePredExpr more), more)
        ('∨':more) -> (Or (parseSimplePredExpr left) (parsePredExpr more), more)
        ('→':more) -> (Implies (parseSimplePredExpr left) (parsePredExpr more), more)
        _          -> (parseSimplePredExpr left, rest)  -- No connective, return the parsed formula and rest

-- | Parse a simple predicate expression.
parseSimplePredExpr :: String -> PredFormula
parseSimplePredExpr (predSym:rest)
    | predSym `elem` ['F'..'Z'] =
        let (terms, rest') = parseDirectTerms predSym rest  -- Parse terms directly following the predicate symbol
        in Predicate predSym terms `combinePredExpr` rest'
parseSimplePredExpr str =
    let (left, rest) = span (`notElem` ['∧', '∨', '→']) str -- Split on the first connective
    in case rest of
        ('∧':more) -> And (parseSimplePredExpr left) (parsePredExpr more)
        ('∨':more) -> Or (parseSimplePredExpr left) (parsePredExpr more)
        ('→':more) -> Implies (parseSimplePredExpr left) (parsePredExpr more)
        _          -> Predicate (head left) []  -- If there's no connective, treat as a predicate constant

-- | Parse terms directly following the predicate symbol without parentheses.
parseDirectTerms :: Char -> String -> ([Term], String)
parseDirectTerms predSym (x:xs)
    | x `elem` ['a'..'z'] = let (terms, rest) = parseDirectTerms predSym xs
                            in (Const x : terms, rest)
    | x `elem` ['x'..'z'] = let (terms, rest) = parseDirectTerms predSym xs
                            in (Var x : terms, rest)
    | otherwise = ([], x:xs)  -- Stop when no more terms are found
parseDirectTerms _ rest = ([], rest)  -- Return empty if no terms found

-- | Combine left-hand side expression with the rest of the string.
combinePredExpr :: PredFormula -> String -> PredFormula
combinePredExpr left ('∧':rest) = And left (parsePredExpr rest)
combinePredExpr left ('∨':rest) = Or left (parsePredExpr rest)
combinePredExpr left ('→':rest) = Implies left (parsePredExpr rest)
combinePredExpr left _ = left

-- | Parse an expression starting with parentheses, returning the subexpression and the remaining string.
parseParenPredExpr :: String -> (PredFormula, String)
parseParenPredExpr ('(':xs) = 
    let (inside, rest) = parseNested xs 0 ""  -- Parse until closing the right number of parentheses
        subExpr = parsePredExpr inside
    in (subExpr, rest)
parseParenPredExpr str = error ("Expected '(', but got: " ++ str)

-- | Helper function to parse nested parentheses.
parseNested :: String -> Int -> String -> (String, String)
parseNested [] _ acc = (acc, [])  -- End of input
parseNested (')':xs) 0 acc = (acc, xs)  -- Closing the main parenthesis
parseNested (')':xs) n acc = parseNested xs (n-1) (acc ++ ")")  -- Closing a nested parenthesis
parseNested ('(':xs) n acc = parseNested xs (n+1) (acc ++ "(")  -- Opening a nested parenthesis
parseNested (x:xs) n acc = parseNested xs n (acc ++ [x])  -- Continue parsing





--- html Rendering

-- | Render the formula with parentheses for binary connectives.
renderWithParens :: PredFormula -> String
renderWithParens (Predicate p terms) = [p] ++ concatMap renderTerm terms
renderWithParens (Not f) = "¬" ++ renderWithParens f  -- Negation
renderWithParens (And f1 f2) = "(" ++ renderWithParens f1 ++ " ∧ " ++ renderWithParens f2 ++ ")"
renderWithParens (Or f1 f2)  = "(" ++ renderWithParens f1 ++ " ∨ " ++ renderWithParens f2 ++ ")"
renderWithParens (Implies f1 f2) = "(" ++ renderWithParens f1 ++ " → " ++ renderWithParens f2 ++ ")"
renderWithParens (ForAll v f) = "∀" ++ [v] ++ renderWithParens f
renderWithParens (Exists v f) = "∃" ++ [v] ++ renderWithParens f

-- | Render formula with outer parentheses removed for binary connectives (And, Or, Implies).
renderFormulaHTML :: PredFormula -> String
renderFormulaHTML (And f1 f2) = stripOuterParens $ renderWithParens (And f1 f2)
renderFormulaHTML (Or f1 f2)  = stripOuterParens $ renderWithParens (Or f1 f2)
renderFormulaHTML (Implies f1 f2) = stripOuterParens $ renderWithParens (Implies f1 f2)
renderFormulaHTML f = renderWithParens f  -- For all other cases

-- | Strip outer parentheses if present (only for And, Or, and Implies formulas).
stripOuterParens :: String -> String
stripOuterParens ('(':xs) | last xs == ')' = init xs  -- Remove first '(' and last ')'
stripOuterParens str = str  -- Return as is if no outer parentheses

-- | Render terms
renderTerm :: Term -> String
renderTerm (Var v) = [v]
renderTerm (Const c) = [c]

-- | Render a single proof line in HTML.
renderLineHTML :: ProofLine -> String
renderLineHTML (ProofLine refs ln formula justification) =
  "<tr><td>" ++ showRefs refs ++ "</td><td>(" ++ show ln ++ ") " ++ renderFormulaHTML formula ++
  "</td><td>" ++ justification ++ "</td></tr>\n"

-- | Render references in HTML with commas between them and no spaces.
showRefs :: [Int] -> String
showRefs refs = intercalate "," (map show refs)  

-- | Render the entire proof in HTML with line breaks.
renderHTML :: Proof -> String
renderHTML proof = "<table class=\"proof-table\">\n" ++ concatMap renderLineHTML proof ++ "</table>\n"



--- Latex rendering 

-- | Render Lemmon proof to LaTeX.
renderLatex :: Proof -> String
renderLatex proof =
    "\\begin{tabular}{ll l}\n" ++
    concatMap renderLineLatex proof ++
    "\\end{tabular}"

-- | Render a single proof line in LaTeX.
renderLineLatex :: ProofLine -> String
renderLineLatex (ProofLine refs ln formula justification) =
    showRefsLatex refs ++ " & (" ++ show ln ++ ") " ++ renderFormulaLatex formula ++ " & " ++ justification ++ " \\\\\n"

-- | Render references in LaTeX.
showRefsLatex :: [Int] -> String
showRefsLatex refs = concatMap show refs

-- | Render a formula in LaTeX, handling different operators.
renderFormulaLatex :: PredFormula -> String
renderFormulaLatex (Predicate p terms) = [p] ++ concatMap renderTermLatex terms
renderFormulaLatex (Not f) = "\\neg " ++ renderFormulaLatex f
renderFormulaLatex (And f1 f2) = "(" ++ renderFormulaLatex f1 ++ " \\land " ++ renderFormulaLatex f2 ++ ")"
renderFormulaLatex (Or f1 f2) = "(" ++ renderFormulaLatex f1 ++ " \\lor " ++ renderFormulaLatex f2 ++ ")"
renderFormulaLatex (Implies f1 f2) = "(" ++ renderFormulaLatex f1 ++ " \\rightarrow " ++ renderFormulaLatex f2 ++ ")"
renderFormulaLatex (ForAll v f) = "\\forall " ++ [v] ++ ". " ++ renderFormulaLatex f
renderFormulaLatex (Exists v f) = "\\exists " ++ [v] ++ ". " ++ renderFormulaLatex f

-- | Render terms for LaTeX
renderTermLatex :: Term -> String
renderTermLatex (Var v) = [v]
renderTermLatex (Const c) = [c]

