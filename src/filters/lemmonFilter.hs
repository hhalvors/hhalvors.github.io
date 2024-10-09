{-# LANGUAGE OverloadedStrings #-}

module LemmonFilter (
    applyLemmonFilter,  -- Existing export
    renderFormulaHTML
    ) where

import Text.Pandoc.JSON
import Text.Pandoc.Definition (Block, Pandoc)  -- Import Block and Pandoc here
import Text.Pandoc.Walk (walk)  -- Import walk here
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import Data.List (intercalate)
import FormulaParser (runParser, PredFormula(..), Term(..))
import Data.Char (isSpace)

-- | Apply the Lemmon filter to a Pandoc document
applyLemmonFilter :: Pandoc -> Pandoc
applyLemmonFilter = walk lemmonFilter  -- Walk through the document applying lemmonFilter

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



-- Helper function to trim leading and trailing spaces or tabs
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Parse a single proof line with enhanced tracing and whitespace trimming
parseProofLine :: String -> ProofLine
parseProofLine line =
  let lineTrimmed = dropWhile (== ' ') line
      (beforeParen, afterParen) = break (== '(') lineTrimmed
      references = if null (words beforeParen)
                     then []
                     else case reads ("[" ++ beforeParen ++ "]") :: [( [Int], String)] of
                            [(refs, _)] -> refs
                            _           -> error ("Failed to parse references: " ++ beforeParen)
      (lineNumStr, rest) = break (== ')') (drop 1 afterParen)
      lineNum = case reads lineNumStr :: [(Int, String)] of
                  [(n, _)] -> n
                  _        -> error ("Failed to parse line number: " ++ show lineNumStr)
      rest' = dropWhile (== ' ') (drop 1 rest)
      (formulaStr, justification) = break (== ' ') rest'
      formulaStrTrimmed = trim formulaStr  -- Trim whitespace here
      parsedFormula = case runParser (trace ("Formula to parse: \"" ++ formulaStrTrimmed ++ "\" | Length: " ++ show (length formulaStrTrimmed)) formulaStrTrimmed) of
                        Right formula -> formula
                        Left err -> trace ("Error parsing formula: " ++ err ++ " | Formula: \"" ++ formulaStrTrimmed ++ "\" | Length: " ++ show (length formulaStrTrimmed)) (Predicate 'E' [])
  in ProofLine references lineNum parsedFormula (dropWhile (== ' ') justification)
  
  

--- html Rendering

-- | Render the formula with parentheses for binary connectives.
renderWithParens :: PredFormula -> String
renderWithParens (Predicate p terms) = [p] ++ concatMap renderTerm terms
renderWithParens (Boolean True) = "True"  -- Render Boolean True
renderWithParens (Boolean False) = "False"  -- Render Boolean False
renderWithParens (Not f) = "¬" ++ renderWithParens f  -- Negation
renderWithParens (And f1 f2) = "(" ++ renderWithParens f1 ++ " ∧ " ++ renderWithParens f2 ++ ")"
renderWithParens (Or f1 f2)  = "(" ++ renderWithParens f1 ++ " ∨ " ++ renderWithParens f2 ++ ")"
renderWithParens (Implies f1 f2) = "(" ++ renderWithParens f1 ++ " → " ++ renderWithParens f2 ++ ")"
renderWithParens (ForAll v f) = "∀" ++ [v] ++ renderWithParens f
renderWithParens (Exists v f) = "∃" ++ [v] ++ renderWithParens f

-- | Render formula with outer parentheses removed for binary connectives (And, Or, Implies).
renderFormulaHTML :: PredFormula -> String
renderFormulaHTML (Boolean True) = "True"  -- Handle Boolean True
renderFormulaHTML (Boolean False) = "False"  -- Handle Boolean False
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

-- | Render a single proof line in HTML with MathJax.
renderLineHTML :: ProofLine -> String
renderLineHTML (ProofLine refs ln formula justification) =
  "<tr><td>" ++ showRefs refs ++ "</td><td>(" ++ show ln ++ ")&nbsp;" ++
  renderFormulaWithMathJax formula ++
  "</td><td>" ++ justification ++ "</td></tr>\n"

-- | Render a formula in MathJax inline math mode.
renderFormulaWithMathJax :: PredFormula -> String
renderFormulaWithMathJax formula =
  "\\( " ++ renderFormulaHTML formula ++ " \\)"
  
-- | Render references in HTML with commas between them and no spaces.
showRefs :: [Int] -> String
showRefs refs = intercalate "," (map show refs)  

-- | Render the entire proof in HTML with line breaks.
renderHTML :: Proof -> String
renderHTML proof = "<table class=\"proof-table\">\n" ++ concatMap renderLineHTML proof ++ "</table>\n"




