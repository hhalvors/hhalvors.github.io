{-# LANGUAGE OverloadedStrings #-}

module TruthTableFilter (applyTruthTableFilter) where

import Data.List (nub, sort)
import Text.Pandoc.JSON
import FormulaParser (PredFormula(..), runParser)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T
import Text.Pandoc.Definition (Block, Pandoc)
import Text.Pandoc.Walk (walk)
import Data.Maybe (fromMaybe)

-- | Data type representing the subformula tree of a logical formula
data SubformulaTree
    = Leaf PredFormula                -- Leaf node representing an atomic formula (a variable or constant)
    | Node PredFormula [SubformulaTree]  -- Node representing a composite formula with subtrees for subformulas
    deriving (Show, Eq)

-- | Evaluate a propositional formula with a given assignment of truth values.
evaluateFormula :: PredFormula -> [(Char, Bool)] -> Bool
evaluateFormula (Not f) assignmentMap = not (evaluateFormula f assignmentMap)
evaluateFormula (And f1 f2) assignmentMap = evaluateFormula f1 assignmentMap && evaluateFormula f2 assignmentMap
evaluateFormula (Or f1 f2) assignmentMap = evaluateFormula f1 assignmentMap || evaluateFormula f2 assignmentMap
evaluateFormula (Implies f1 f2) assignmentMap = not (evaluateFormula f1 assignmentMap) || evaluateFormula f2 assignmentMap
evaluateFormula (Predicate p _) assignmentMap = lookupTruthValue p assignmentMap
evaluateFormula (Boolean b) _ = b

-- Define a mapping of subformulas to positions in the string
data PositionMap = PositionMap {
    flattenedString :: String,         -- The flattened string representation of the formula
    subformulaMap   :: [(PredFormula, Int)]  -- List of subformulas and their corresponding positions
}


-- Helper to show Boolean values
showBool :: Bool -> String
showBool True  = "1"
showBool False = "0"

-- Generate the HTML for truth values
truthValueCell :: [(Char, Bool)] -> Char -> H.Html
truthValueCell assignmentMap var =
    H.td $ H.toHtml (showBool $ lookupTruthValue var assignmentMap)

-- Lookup the truth value of a variable in the assignment map
lookupTruthValue :: Char -> [(Char, Bool)] -> Bool
lookupTruthValue var assignmentMap = case lookup var assignmentMap of
    Just val -> val
    Nothing  -> error $ "Variable " ++ [var] ++ " not found in assignment."



-- Helper function to generate all possible truth assignments for a list of variables
allPossibleAssignments :: [Char] -> [[(Char, Bool)]]
allPossibleAssignments [] = [[]]
allPossibleAssignments (v:vs) =
    [(v, True) : rest | rest <- allPossibleAssignments vs] ++
    [(v, False) : rest | rest <- allPossibleAssignments vs]

-- Apply the truth table filter to a Pandoc document
applyTruthTableFilter :: Pandoc -> Pandoc
applyTruthTableFilter = walk processBlock

-- Process a Pandoc block to replace code blocks with truth tables if necessary
processBlock :: Block -> Block
processBlock (CodeBlock (id, classes, attrs) code)
    | "truth-table" `elem` classes =
        let formula = fromMaybe (error "Failed to parse formula") $ runParser code
            htmlTable = renderTruthTable formula
        in RawBlock (Format "html") (T.pack htmlTable)
processBlock block = block

-- | Collect all variables used in a propositional formula
getVars :: PredFormula -> [Char]
getVars (Not f) = getVars f
getVars (And f1 f2) = getVars f1 ++ getVars f2
getVars (Or f1 f2) = getVars f1 ++ getVars f2
getVars (Implies f1 f2) = getVars f1 ++ getVars f2
getVars (Predicate p _) = [p]  -- We assume the predicate symbol is a single Char
getVars (Boolean _) = []  -- No variables in a Boolean value

-- Remove duplicates from the list of variables
getUniqueVars :: PredFormula -> [Char]
getUniqueVars = nub . getVars

-- | Flatten the subformula tree to a string, and map positions in the string to subformulas.
flattenSubformulaTree :: SubformulaTree -> (String, [(PredFormula, Int)])
flattenSubformulaTree (Leaf formula) =
    let str = showFormula formula
    in (str, [(formula, 0)])

flattenSubformulaTree (Node formula subtrees) =
    case formula of
        -- Binary connectives: add parentheses around the entire expression.
        And _ _ -> flattenBinary formula subtrees "\\land"
        Or _ _  -> flattenBinary formula subtrees "\\lor"
        Implies _ _ -> flattenBinary formula subtrees "\\to"
        -- Unary connective: just add the operator (e.g., \neg) and recurse.
        Not f   -> let (subStr, mapping) = flattenSubformulaTree (head subtrees)
                       operatorStr = "\\neg"
                   in (operatorStr ++ subStr, (formula, length operatorStr - 1) : mapping)
        _ -> error "Unsupported formula"

-- | Helper function to flatten binary connectives like And, Or, Implies.
flattenBinary :: PredFormula -> [SubformulaTree] -> String -> (String, [(PredFormula, Int)])
flattenBinary formula [leftSubtree, rightSubtree] operatorStr =
    let (leftStr, leftMapping) = flattenSubformulaTree leftSubtree
        (rightStr, rightMapping) = flattenSubformulaTree rightSubtree
        -- Add parentheses around the entire expression.
        combinedStr = "(" ++ leftStr ++ operatorStr ++ rightStr ++ ")"
        -- Adjust positions of the subformulas in the final string.
        offsetLeftMapping = offsetMapping 1 leftMapping  -- for the opening parenthesis
        operatorPos = length leftStr + 1  -- position of the operator inside the parentheses
        operatorMapping = [(formula, operatorPos)]
        offsetRightMapping = offsetMapping (operatorPos + length operatorStr) rightMapping
    in (combinedStr, offsetLeftMapping ++ operatorMapping ++ offsetRightMapping)

-- | Adjust the positions of subformulas in the mapping based on the offset.
offsetMapping :: Int -> [(PredFormula, Int)] -> [(PredFormula, Int)]
offsetMapping offset = map (\(f, pos) -> (f, pos + offset))

-- | Display the formula as a string (for the leaves of the subformula tree).
showFormula :: PredFormula -> String
showFormula (Predicate p _) = [p]  -- Simply return the predicate symbol.
showFormula (Boolean True)  = "1"   -- Represent Boolean True as "1".
showFormula (Boolean False) = "0"   -- Represent Boolean False as "0".
showFormula _ = error "Unsupported leaf formula"

-- | Test function to display the flattened version of a formula.
testFlattenFormula :: PredFormula -> IO ()
testFlattenFormula formula =
    let subformulaTree = buildSubformulaTree formula
        (flattenedStr, mapping) = flattenSubformulaTree subformulaTree
    in do
        putStrLn $ "Flattened Formula: " ++ flattenedStr
        putStrLn "Subformula Mapping:"
        mapM_ print mapping

-- | Test function to display the formula string with valuation applied.
testApplyValuation :: PredFormula -> [(Char, Bool)] -> IO ()
testApplyValuation formula valuation =
    let subformulaTree = buildSubformulaTree formula
        (flattenedStr, mapping) = flattenSubformulaTree subformulaTree
        appliedValuationStr = applyValuation formula valuation
    in do
        putStrLn $ "Original Flattened Formula: " ++ flattenedStr
        putStrLn $ "Formula with Valuation Applied: " ++ appliedValuationStr






