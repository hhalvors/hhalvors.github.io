-- | talks-tool.hs — command-line companion to data/talks-master.yaml.
--
--   talks build-cv [OUTDIR]   write cv-talks.tex and cv-outreach.tex
--   talks check               validate the catalog and deck links
--
-- See docs/talks-pipeline-plan.md.

module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.Directory   (doesFileExist)
import System.FilePath    ((</>))
import Data.Yaml          (decodeFileEither)
import Data.List          (group, isPrefixOf, sort)
import Data.Maybe         (isJust)
import Data.Char          (isSpace)
import Control.Monad      (forM, forM_, when)
import TalksMaster

masterPath :: FilePath
masterPath = "data/talks-master.yaml"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("build-cv":rest) -> buildCv (case rest of (d:_) -> d; _ -> ".")
    ["check"]         -> check
    _ -> putStrLn "usage: talks (build-cv [OUTDIR] | check)"

loadMaster :: IO MasterData
loadMaster = do
  r <- decodeFileEither masterPath
  case r of
    Left e   -> putStrLn ("YAML parse error in " ++ masterPath ++ ":\n" ++ show e)
                  >> exitFailure >> error "unreachable"
    Right md -> return md

------------------------------------------------------------------------
-- build-cv
------------------------------------------------------------------------

buildCv :: FilePath -> IO ()
buildCv outdir = do
  md <- loadMaster
  let f1 = outdir </> "cv-talks.tex"
      f2 = outdir </> "cv-outreach.tex"
  writeFile f1 (renderInvited md)
  writeFile f2 (renderOutreach md)
  putStrLn ("Wrote " ++ f1 ++ " and " ++ f2)

------------------------------------------------------------------------
-- check
------------------------------------------------------------------------

data Level = Err | Warn deriving Eq

tag :: Level -> String
tag Err  = "ERROR"
tag Warn = "warn "

check :: IO ()
check = do
  md <- loadMaster
  let items = map snd (allTalks md)

  -- 1. duplicate ids
  let dups = [ head xs | xs <- group (sort (map tId items)), length xs > 1 ]
      dupMsgs = [ (Err, "duplicate id: " ++ d) | d <- dups ]

  -- 2/3/4. per-item structural + filesystem checks
  perItem <- forM items checkItem

  let msgs = dupMsgs ++ concat perItem
      errs = length [ () | (Err, _) <- msgs ]
      warns = length [ () | (Warn, _) <- msgs ]

  forM_ msgs $ \(lvl, m) -> putStrLn (tag lvl ++ "  " ++ m)

  putStrLn ""
  putStrLn $ "Summary: " ++ show (length items) ++ " talks, "
          ++ show (length [() | t <- items, tCv t]) ++ " in CV, "
          ++ show (length [() | t <- items, not (null (tLinks t))]) ++ " with slides, "
          ++ show errs ++ " errors, " ++ show warns ++ " warnings."
  when (errs > 0) exitFailure

checkItem :: MTalk -> IO [(Level, String)]
checkItem t = do
  let here msg = tId t ++ ": " ++ msg
      -- web rows must have links
      webNoLinks = [ (Warn, here "web: true but no links") | tWeb t, null (tLinks t) ]
      -- a CV row should be identifiable
      noTitle    = [ (Warn, here "no title and no cv_raw")
                   | not (isJust (tTitle t)), not (isJust (tCvRaw t)) ]
      -- date sanity
      badDate    = [ (Warn, here ("unparseable date: " ++ d))
                   | Just d <- [tDate t], not (okDate d) ]

  -- link targets exist on disk (run from repo root)
  fileMsgs <- fmap concat $ forM (tLinks t) $ \l ->
    if "talks/" `isPrefixOf` lUrl l
      then do ex <- doesFileExist (lUrl l)
              return [ (Err, here ("missing link target: " ++ lUrl l)) | not ex ]
      else return []

  -- deck-header cross-check (only when a .tex deck carries a %%talk header)
  hdrMsgs <- fmap concat $ forM (tLinks t) $ \l ->
    if "talks/" `isPrefixOf` lUrl l && ".tex" `isSuffixOf` lUrl l
      then do ex <- doesFileExist (lUrl l)
              if ex then headerCheck t (lUrl l) else return []
      else return []

  return (webNoLinks ++ noTitle ++ badDate ++ fileMsgs ++ hdrMsgs)

-- | If the .tex carries a "%%talk … id: … %%end" header, verify its id matches.
headerCheck :: MTalk -> FilePath -> IO [(Level, String)]
headerCheck t path = do
  src <- readFile path
  let ls = take 25 (lines src)
  if any ("%%talk" `isPrefixOf`) (map (dropWhile isSpace) ls)
    then case headerField "id" ls of
           Just hid | hid /= tId t ->
             return [(Warn, tId t ++ ": deck header id (" ++ hid ++ ") /= row id")]
           _ -> return []
    else return []  -- not yet stamped; silent

headerField :: String -> [String] -> Maybe String
headerField key ls =
  case [ strip (drop (length pat) cleaned)
       | l <- ls
       , let cleaned = strip (dropPercent l)
       , pat `isPrefixOf` cleaned ] of
    (v:_) -> Just v
    _     -> Nothing
  where
    pat = key ++ ":"
    dropPercent = dropWhile (\c -> c == '%' || c == ' ')

------------------------------------------------------------------------
-- small utilities
------------------------------------------------------------------------

strip :: String -> String
strip = f . f where f = reverse . dropWhile isSpace

isSuffixOf :: String -> String -> Bool
isSuffixOf s xs = reverse s `isPrefixOf` reverse xs

-- | Accept "YYYY" or "YYYY-MM".
okDate :: String -> Bool
okDate d = case break (== '-') d of
  (y, "")      -> length y == 4 && all (`elem` "0123456789") y
  (y, '-':mm)  -> length y == 4 && all (`elem` "0123456789") y
                  && length mm == 2 && all (`elem` "0123456789") mm
  _            -> False
