{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CoursePages.Course
  ( loadCourseYaml
  , courseBaseCtx
  , courseNavItemCtx
  , courseSiteCtx
  , compileLecturesFromCourseYaml
  , compilePsetsFromCourseYaml
  , compilePreceptsFromCourseYaml
  ) where

import           Control.Applicative   ((<|>), empty)
import           Control.Monad         (forM)
import           Data.Aeson            (FromJSON(..), (.:), (.:?), (.!=), withObject)
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import qualified Data.Yaml             as Y
import           GHC.Generics          (Generic)
import           Hakyll
import qualified System.Directory      as Dir
import           System.FilePath       (takeDirectory, (</>))

--------------------------------------------------------------------------------
-- Helpers

stripPrincetonDomain :: String -> String
stripPrincetonDomain email =
  case break (== '@') email of
    (user, "@princeton.edu") -> user <> "@"
    _                        -> email

nonEmpty :: String -> Bool
nonEmpty = not . null

joinBullets :: [String] -> String
joinBullets xs =
  let ys = filter nonEmpty xs
  in intercalate " • " ys

joinComma :: [String] -> String
joinComma = intercalate ", "

--------------------------------------------------------------------------------
-- YAML top-level

data CourseYaml = CourseYaml
  { cyCourse   :: CourseMeta
  , cySchedule :: Maybe ScheduleMeta
  , cyStaff    :: Maybe Staff
  , cyUnits    :: [Unit]
  } deriving (Show, Eq, Generic)

instance FromJSON CourseYaml where
  parseJSON = withObject "CourseYaml" $ \o ->
    CourseYaml <$> o .:  "course"
               <*> o .:? "schedule"
               <*> o .:? "staff"
               <*> o .:? "units" .!= []

--------------------------------------------------------------------------------
-- Course metadata

data NavItem = NavItem
  { navId    :: String
  , navLabel :: String
  , navPath  :: String
  } deriving (Show, Eq, Generic)

instance FromJSON NavItem where
  parseJSON = withObject "NavItem" $ \o ->
    NavItem <$> o .: "id"
            <*> o .: "label"
            <*> o .: "path"

data CourseMeta = CourseMeta
  { courseCode        :: String
  , courseTitle       :: String
  , courseTerm        :: String
  , courseUniversity  :: String
  , courseDescription :: Maybe String
  , courseNavigation  :: [NavItem]
  } deriving (Show, Eq, Generic)

instance FromJSON CourseMeta where
  parseJSON = withObject "course" $ \o ->
    CourseMeta <$> o .:  "code"
               <*> o .:  "title"
               <*> o .:  "term"
               <*> o .:  "university"
               <*> o .:? "description"
               <*> o .:? "navigation" .!= []

--------------------------------------------------------------------------------
-- Schedule metadata (optional)

data LecturesMeta = LecturesMeta
  { lecDays     :: [String]
  , lecTime     :: Maybe String
  , lecLocation :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON LecturesMeta where
  parseJSON = withObject "lectures" $ \o ->
    LecturesMeta <$> o .:? "days" .!= []
                <*> o .:? "time"
                <*> o .:? "location"

data PsetsMeta = PsetsMeta
  { psetsDueDay  :: Maybe String
  , psetsDueTime :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON PsetsMeta where
  parseJSON = withObject "psets" $ \o -> do
    dueObj <- o .:? "due"
    case dueObj of
      Nothing -> pure (PsetsMeta Nothing Nothing)
      Just v  ->
        withObject "due"
          (\d -> PsetsMeta <$> d .:? "day" <*> d .:? "time")
          v

data ScheduleMeta = ScheduleMeta
  { scheduleLectures :: Maybe LecturesMeta
  , schedulePsets    :: Maybe PsetsMeta
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleMeta where
  parseJSON = withObject "schedule" $ \o ->
    ScheduleMeta <$> o .:? "lectures"
                 <*> o .:? "psets"

--------------------------------------------------------------------------------
-- Staff (optional)

data StaffMember = StaffMember
  { staffName  :: String
  , staffRole  :: Maybe String
  , staffEmail :: Maybe String
  } deriving (Show, Eq, Generic)

instance FromJSON StaffMember where
  parseJSON = withObject "StaffMember" $ \o ->
    StaffMember <$> o .:  "name"
                <*> o .:? "role"
                <*> o .:? "email"

data Staff = Staff
  { staffInstructors :: [StaffMember]
  , staffPreceptors  :: [StaffMember]
  } deriving (Show, Eq, Generic)

instance FromJSON Staff where
  parseJSON = withObject "staff" $ \o ->
    Staff <$> o .:? "instructors" .!= []
          <*> o .:? "preceptors"  .!= []

--------------------------------------------------------------------------------
-- Units + activities

-- Shared “files” shape used by lecture/quiz/exam in your YAML
data EventFiles = EventFiles
  { efSlides    :: Maybe FilePath
  , efHandout   :: Maybe FilePath
  , efPractice  :: Maybe FilePath
  , efNotes     :: Maybe FilePath     -- optional (if you add it later)
  } deriving (Show, Eq, Generic)

instance FromJSON EventFiles where
  parseJSON = withObject "EventFiles" $ \o ->
    EventFiles <$> o .:? "slides"
               <*> o .:? "handout"
               <*> o .:? "practice"
               <*> o .:? "notes"

data Lecture = Lecture
  { lecDate    :: String
  , lecTitle   :: String
  , lecReading :: Maybe String
  , lecInfo    :: Maybe String
  , lecSkills  :: [String]
  , lecFiles   :: Maybe EventFiles
  } deriving (Show, Eq, Generic)

instance FromJSON Lecture where
  parseJSON = withObject "Lecture" $ \o ->
    Lecture <$> o .:  "date"
            <*> o .:  "title"
            <*> o .:? "reading"
            <*> o .:? "info"
            <*> o .:? "skills" .!= []
            <*> o .:? "files"

data SimpleMeeting = SimpleMeeting
  { smDate   :: String
  , smTitle  :: Maybe String
  , smKind   :: Maybe String
  , smInfo   :: Maybe String
  , smSkills :: [String]
  , smFiles  :: Maybe EventFiles
  } deriving (Show, Eq, Generic)

instance FromJSON SimpleMeeting where
  parseJSON = withObject "SimpleMeeting" $ \o ->
    SimpleMeeting <$> o .:  "date"
                  <*> o .:? "title"
                  <*> (o .:? "kind" <|> o .:? "type")
                  <*> o .:? "info"
                  <*> o .:? "skills" .!= []
                  <*> o .:? "files"

data Quiz = Quiz
  { quizDate   :: String
  , quizTitle  :: Maybe String
  , quizInfo   :: Maybe String
  , quizSkills :: [String]
  , quizFiles  :: Maybe EventFiles
  } deriving (Show, Eq, Generic)

instance FromJSON Quiz where
  parseJSON = withObject "Quiz" $ \o ->
    Quiz <$> o .:  "date"
         <*> o .:? "title"
         <*> o .:? "info"
         <*> o .:? "skills" .!= []
         <*> o .:? "files"

data Exam = Exam
  { examId    :: String
  , examDate  :: String
  , examTitle :: Maybe String
  , examInfo  :: Maybe String
  , examFiles :: Maybe EventFiles
  } deriving (Show, Eq, Generic)

instance FromJSON Exam where
  parseJSON = withObject "Exam" $ \o ->
    Exam <$> o .:  "id"
         <*> o .:  "date"
         <*> o .:? "title"
         <*> o .:? "info"
         <*> o .:? "files"

data PsetFiles = PsetFiles
  { pfHandout   :: Maybe FilePath
  , pfSolutions :: Maybe FilePath     -- optional (add when you want)
  } deriving (Show, Eq, Generic)

instance FromJSON PsetFiles where
  parseJSON = withObject "PsetFiles" $ \o ->
    PsetFiles <$> o .:? "handout"
              <*> o .:? "solutions"

data Pset = Pset
  { psetId      :: Int
  , psetTopic   :: Maybe String       -- explicit topic
  , psetSkills  :: [String]           -- skills list
  , psetInfo    :: Maybe String
  , psetDueDate :: Maybe String
  , psetDueTime :: Maybe String
  , psetFiles   :: Maybe PsetFiles
  } deriving (Show, Eq, Generic)

instance FromJSON Pset where
  parseJSON = withObject "Pset" $ \o ->
    Pset <$> o .:  "id"
         <*> o .:? "topic"
         <*> o .:? "skills" .!= []
         <*> o .:? "info"
         <*> o .:? "due_date"
         <*> o .:? "due_time"
         <*> o .:? "files"

data Unit = Unit
  { unitId              :: String
  , unitTitle           :: String
  , unitInfo            :: Maybe String
  , unitReadingOverview :: Maybe String
  , unitLectures        :: [Lecture]
  , unitPrecepts        :: [SimpleMeeting]
  , unitPsets           :: [Pset]
  , unitQuizzes         :: [Quiz]
  , unitExams           :: [Exam]
  } deriving (Show, Eq, Generic)

instance FromJSON Unit where
  parseJSON = withObject "Unit" $ \o ->
    Unit <$> o .:  "id"
         <*> o .:  "title"
         <*> ((o .:? "unit_info") <|> (o .:? "info"))
         <*> o .:? "reading_overview"
         <*> o .:? "lectures"  .!= []
         <*> o .:? "precepts"  .!= []
         <*> o .:? "psets"     .!= []
         <*> o .:? "quizzes"   .!= []
         <*> o .:? "exams"     .!= []

--------------------------------------------------------------------------------
-- Loader

loadCourseYaml :: FilePath -> Compiler CourseYaml
loadCourseYaml fp = do
  e <- unsafeCompiler $ Y.decodeFileEither fp
  case e of
    Left err -> fail $ "Course YAML parse error in " <> fp <> ":\n" <> Y.prettyPrintParseException err
    Right v  -> pure v

--------------------------------------------------------------------------------
-- Existence checks (for “no link until PDF exists”)

fileExistsRelToYaml :: FilePath -> FilePath -> Compiler Bool
fileExistsRelToYaml yamlPath rel =
  unsafeCompiler $ Dir.doesFileExist (takeDirectory yamlPath </> rel)

--------------------------------------------------------------------------------
-- Rows emitted to templates

data LectureRow = LectureRow
  { lrLectureDate  :: String
  , lrTitle        :: String
  , lrReading      :: String
  , lrHasReading  :: Bool
  , lrInfo         :: String
  , lrSkills       :: String

  , lrHasSlides    :: Bool
  , lrSlidesHref   :: String

  , lrHasHandout   :: Bool
  , lrHandoutHref  :: String
  } deriving (Show, Eq)

data PsetRow = PsetRow
  { prNumber         :: String
  , prDue            :: String
  , prTopic          :: String
  , prInfo           :: String
  , prSkills         :: String
  , prHasHandout     :: Bool
  , prHandoutHref    :: String
  , prHasSolutions   :: Bool
  , prSolutionsHref  :: String
  } deriving (Show, Eq)

data PreceptRow = PreceptRow
  { prcDate        :: String
  , prcTitle       :: String
  , prcKind        :: String
  , prcInfo        :: String
  , prcSkills      :: String
  , prcHasHandout  :: Bool
  , prcHandoutHref :: String
  , prcHasPractice :: Bool
  , prcPracticeHref :: String
  } deriving (Show, Eq)

data PreceptUnitBlock = PreceptUnitBlock
  { pubcUnitId              :: String
  , pubcUnitTitle           :: String
  , pubcUnitInfo            :: String
  , pubcUnitReadingOverview :: String
  , pubcPrecepts            :: [PreceptRow]
  } deriving (Show, Eq)


-- Unit-chunk blocks (Path 1): template loops over $for(units)$, then $for(lectures)$ / $for(psets)$
data LectureUnitBlock = LectureUnitBlock
  { lubUnitId              :: String
  , lubUnitTitle           :: String
  , lubUnitInfo            :: String
  , lubUnitReadingOverview :: String
  , lubLectures            :: [LectureRow]
  } deriving (Show, Eq)

data PsetUnitBlock = PsetUnitBlock
  { pubUnitId              :: String
  , pubUnitTitle           :: String
  , pubUnitInfo            :: String
  , pubUnitReadingOverview :: String
  , pubPsets               :: [PsetRow]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Build rows from a unit (keeps chunking intact)

lectureRowsFromUnit :: FilePath -> Unit -> Compiler [LectureRow]
lectureRowsFromUnit yamlPath u = do
  let uInfo = fromMaybe "" (unitInfo u)
  forM (unitLectures u) $ \lec -> do
    let dt     = lecDate lec
        ttl    = lecTitle lec
        rd     = fromMaybe "" (lecReading lec)
        hasRd  = nonEmpty rd
        info   = joinBullets [fromMaybe "" (lecInfo lec), uInfo]
        skills = joinComma (lecSkills lec)

        slidesPath  = lecFiles lec >>= efSlides
        handoutPath = lecFiles lec >>= efHandout

    (hasSlides, slidesHref) <-
      case slidesPath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    (hasHandout, handoutHref) <-
      case handoutPath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    pure $ LectureRow
      dt ttl rd hasRd info skills 
      hasSlides slidesHref
      hasHandout handoutHref

psetRowsFromUnit :: FilePath -> Unit -> Compiler [PsetRow]
psetRowsFromUnit yamlPath u = do
  let uInfo = fromMaybe "" (unitInfo u)
  forM (unitPsets u) $ \p -> do
    let num  = show (psetId p)

        dueD = fromMaybe "" (psetDueDate p)
        dueT = fromMaybe "" (psetDueTime p)
        due  = case (dueD, dueT) of
                 ("","") -> ""
                 (d,"")  -> d
                 ("",t)  -> t
                 (d,t)   -> d <> " " <> t

        topic  = fromMaybe "" (psetTopic p)
        info   = joinBullets [fromMaybe "" (psetInfo p), uInfo]
        skills = joinComma (psetSkills p)

        handoutPath   = psetFiles p >>= pfHandout
        solutionsPath = psetFiles p >>= pfSolutions

    (hasH, hrefH) <-
      case handoutPath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    (hasS, hrefS) <-
      case solutionsPath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    pure $ PsetRow num due topic info skills hasH hrefH hasS hrefS

--------------------------------------------------------------------------------
-- Contexts

navItemCtxWith :: String -> Context NavItem
navItemCtxWith activeId =
     field "id"    (pure . navId    . itemBody)
  <> field "label" (pure . navLabel . itemBody)
  <> field "path"  (pure . navPath  . itemBody)
  <> field "activeClass" (\it ->
        pure $ if navId (itemBody it) == activeId then "active" else "")

courseMetaCtx :: String -> CourseMeta -> Context String
courseMetaCtx activeId cm =
     constField "courseCode"        (courseCode cm)
  <> constField "courseTitle"       (courseTitle cm)
  <> constField "courseTerm"        (courseTerm cm)
  <> constField "courseUniversity"  (courseUniversity cm)
  <> constField "courseDescription" (fromMaybe "" (courseDescription cm))
  <> listField "nav" (navItemCtxWith activeId) (mapM makeItem (courseNavigation cm))

lectureRowCtx :: Context LectureRow
lectureRowCtx =
     field "date"  (pure . lrLectureDate . itemBody)
  <> field "title" (pure . lrTitle       . itemBody)

  <> field "reading" (\it ->
       let r = lrReading (itemBody it)
       in if nonEmpty r then pure r else empty)

  <> field "info" (\it ->
       let i = lrInfo (itemBody it)
       in if nonEmpty i then pure i else empty)

  <> field "skills" (\it ->
       let s = lrSkills (itemBody it)
       in if nonEmpty s then pure s else empty)

  <> field "slidesHref" (\it ->
       let h = lrSlidesHref (itemBody it)
       in if nonEmpty h then pure h else empty)

  <> field "handoutHref" (\it ->
       let h = lrHandoutHref (itemBody it)
       in if nonEmpty h then pure h else empty)  


psetRowCtx :: Context PsetRow
psetRowCtx =
     field "number"        (pure . prNumber . itemBody)
  <> field "due"           (pure . prDue . itemBody)
  <> field "topic"         (pure . prTopic . itemBody)
  <> field "info"          (pure . prInfo . itemBody)
  <> field "skills"        (pure . prSkills . itemBody)
  <> field "hasSkills"     (pure . (\r -> if nonEmpty (prSkills r) then "true" else "") . itemBody)
  <> field "hasHandout"    (pure . (\r -> if prHasHandout r then "true" else "") . itemBody)
  <> field "handoutHref"   (pure . prHandoutHref . itemBody)
  <> field "hasSolutions"  (pure . (\r -> if prHasSolutions r then "true" else "") . itemBody)
  <> field "solutionsHref" (pure . prSolutionsHref . itemBody)

lectureUnitBlockCtx :: Context LectureUnitBlock
lectureUnitBlockCtx =
     field "unitId"           (pure . lubUnitId . itemBody)
  <> field "unitTitle"        (pure . lubUnitTitle . itemBody)
  <> field "unitInfo"         (pure . lubUnitInfo . itemBody)
  <> field "readingOverview"  (pure . lubUnitReadingOverview . itemBody)
  <> listFieldWith "lectures" lectureRowCtx
       (\it -> mapM makeItem (lubLectures (itemBody it)))

psetUnitBlockCtx :: Context PsetUnitBlock
psetUnitBlockCtx =
     field "unitId"           (pure . pubUnitId . itemBody)
  <> field "unitTitle"        (pure . pubUnitTitle . itemBody)
  <> field "unitInfo"         (pure . pubUnitInfo . itemBody)
  <> field "readingOverview"  (pure . pubUnitReadingOverview . itemBody)
  <> listFieldWith "psets" psetRowCtx
       (\it -> mapM makeItem (pubPsets (itemBody it)))

-- For the course-website header nav
courseNavItemCtx :: String -> Context NavItem
courseNavItemCtx activeId =
     field "id"    (pure . navId    . itemBody)
  <> field "label" (pure . navLabel . itemBody)
  <> field "path"  (pure . navPath  . itemBody)
  <> field "isActive"    (\it -> pure $ if navId (itemBody it) == activeId then "true" else "")
  <> field "activeClass" (\it -> pure $ if navId (itemBody it) == activeId then "active" else "")

-- Context for course-site pages (header fields + nav)
courseSiteCtx :: CourseYaml -> String -> Context String
courseSiteCtx cy activeId =
     constField "courseCode"        (courseCode (cyCourse cy))
  <> constField "courseTitle"       (courseTitle (cyCourse cy))
  <> constField "courseTerm"        (courseTerm (cyCourse cy))
  <> constField "courseUniversity"  (courseUniversity (cyCourse cy))
  <> constField "courseDescription" (fromMaybe "" (courseDescription (cyCourse cy)))
  <> listField "nav" (courseNavItemCtx activeId)
       (mapM makeItem (courseNavigation (cyCourse cy)))
  <> defaultContext       

--------------------------------------------------------------------------------
-- Staff contexts (for templates)

staffMemberCtx :: Context StaffMember
staffMemberCtx =
     field "name"  (pure . staffName  . itemBody)
  <> field "role"  (pure . fromMaybe "" . staffRole . itemBody)
  <> field "email" (pure . maybe "" stripPrincetonDomain . staffEmail . itemBody)

  -- Convenience booleans for $if(...)$ in templates
  <> field "hasRole"  (pure . (\m -> if maybe False nonEmpty (staffRole m) then "true" else "") . itemBody)
  <> field "hasEmail" (pure . (\m -> if maybe False nonEmpty (staffEmail m) then "true" else "") . itemBody)

--------------------------------------------------------------------------------
-- Schedule helper fields (lecture times)

lectureScheduleFields :: Maybe ScheduleMeta -> Context a
lectureScheduleFields mSched =
  let mLecs = mSched >>= scheduleLectures  -- Maybe LecturesMeta
      days  = maybe "" (joinComma . lecDays) mLecs
      time  = maybe "" (fromMaybe "" . lecTime) mLecs
      loc   = maybe "" (fromMaybe "" . lecLocation) mLecs
      has   = nonEmpty days || nonEmpty time || nonEmpty loc
  in
       constField "hasLectureSchedule" (if has then "true" else "")
    <> constField "lectureDays" days
    <> constField "lectureTime" time
    <> constField "lectureLocation" loc

--------------------------------------------------------------------------------
-- Course base context (moved from site.hs)

courseBaseCtx :: FilePath -> String -> Compiler (Context String)
courseBaseCtx rootPath activeId = do
  cy <- loadCourseYaml (rootPath <> "/course.yaml")
  let cm    = cyCourse cy
      st    = fromMaybe (Staff [] []) (cyStaff cy)
      mSched = cySchedule cy

  navItems <- mapM makeItem (courseNavigation cm)

  let navCtx =
           field "id"    (pure . navId    . itemBody)
        <> field "label" (pure . navLabel . itemBody)
        <> field "path"  (pure . navPath  . itemBody)
        <> field "isActive" (\it -> pure $ if navId (itemBody it) == activeId then "true" else "")
        <> field "activeClass" (\it -> pure $ if navId (itemBody it) == activeId then "active" else "")

  instructorItems <- mapM makeItem (staffInstructors st)
  preceptorItems  <- mapM makeItem (staffPreceptors  st)

  pure $
       constField "root" ("/" <> rootPath)
    <> constField "courseCode"  (courseCode cm)
    <> constField "courseTitle" (courseTitle cm)
    <> constField "courseTerm"  (courseTerm cm)
    <> constField "courseUniversity" (courseUniversity cm)
    <> constField "courseDescription" (fromMaybe "" (courseDescription cm))

    <> lectureScheduleFields mSched

    <> listField "instructors" staffMemberCtx (pure instructorItems)
    <> listField "preceptors"  staffMemberCtx (pure preceptorItems)
    <> listField "nav" navCtx (pure navItems)

    <> defaultContext       

--------------------------------------------------------------------------------
-- Public compilers (Path 1: emit unit-chunked data)

compileLecturesFromCourseYaml
  :: FilePath
  -> Identifier
  -> Compiler (Item String)
compileLecturesFromCourseYaml yamlPath templateId = do
  cy <- loadCourseYaml yamlPath

  blocks <- forM (cyUnits cy) $ \u -> do
    rows <- lectureRowsFromUnit yamlPath u
    pure $ LectureUnitBlock
      (unitId u)
      (unitTitle u)
      (fromMaybe "" (unitInfo u))
      (fromMaybe "" (unitReadingOverview u))
      rows

  blockItems <- mapM makeItem blocks

  let ctx =
        constField "title" "Lectures"
        <> courseMetaCtx "lectures" (cyCourse cy)
        <> listField "units" lectureUnitBlockCtx (pure blockItems)
        <> defaultContext

  makeItem ("" :: String) >>= loadAndApplyTemplate templateId ctx

compilePsetsFromCourseYaml
  :: FilePath
  -> Identifier
  -> Compiler (Item String)
compilePsetsFromCourseYaml yamlPath templateId = do
  cy <- loadCourseYaml yamlPath

  blocks <- forM (cyUnits cy) $ \u -> do
    rows <- psetRowsFromUnit yamlPath u
    pure $ PsetUnitBlock
      (unitId u)
      (unitTitle u)
      (fromMaybe "" (unitInfo u))
      (fromMaybe "" (unitReadingOverview u))
      rows

  blockItems <- mapM makeItem blocks

  let ctx =
        constField "title" "Psets"
        <> courseMetaCtx "psets" (cyCourse cy)
        <> listField "units" psetUnitBlockCtx (pure blockItems)
        <> defaultContext

  makeItem ("" :: String) >>= loadAndApplyTemplate templateId ctx

preceptRowsFromUnit :: FilePath -> Unit -> Compiler [PreceptRow]
preceptRowsFromUnit yamlPath u = do
  let uInfo = fromMaybe "" (unitInfo u)

  forM (unitPrecepts u) $ \m -> do
    let dt     = smDate m
        ttl    = fromMaybe "Precept" (smTitle m)
        kind   = fromMaybe "" (smKind m)
        info   = joinBullets [fromMaybe "" (smInfo m), uInfo]
        skills = joinComma (smSkills m)

        handoutPath  = smFiles m >>= efHandout
        practicePath = smFiles m >>= efPractice

    (hasH, hrefH) <-
      case handoutPath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    (hasP, hrefP) <-
      case practicePath of
        Nothing -> pure (False, "")
        Just fp -> do ok <- fileExistsRelToYaml yamlPath fp
                      pure (ok, if ok then fp else "")

    pure $ PreceptRow dt ttl kind info skills hasH hrefH hasP hrefP

preceptRowCtx :: Context PreceptRow
preceptRowCtx =
     field "date"         (pure . prcDate . itemBody)
  <> field "title"        (pure . prcTitle . itemBody)
  <> field "kind"         (pure . prcKind . itemBody)
  <> field "info"         (pure . prcInfo . itemBody)
  <> field "skills"       (pure . prcSkills . itemBody)

  <> field "hasKind"      (pure . (\r -> if nonEmpty (prcKind r) then "true" else "") . itemBody)
  <> field "hasSkills"    (pure . (\r -> if nonEmpty (prcSkills r) then "true" else "") . itemBody)

  <> field "hasHandout"   (pure . (\r -> if prcHasHandout r then "true" else "") . itemBody)
  <> field "handoutHref"  (pure . prcHandoutHref . itemBody)

  <> field "hasPractice"  (pure . (\r -> if prcHasPractice r then "true" else "") . itemBody)
  <> field "practiceHref" (pure . prcPracticeHref . itemBody)

preceptUnitBlockCtx :: Context PreceptUnitBlock
preceptUnitBlockCtx =
     field "unitId"          (pure . pubcUnitId . itemBody)
  <> field "unitTitle"       (pure . pubcUnitTitle . itemBody)
  <> field "unitInfo"        (pure . pubcUnitInfo . itemBody)
  <> field "readingOverview" (pure . pubcUnitReadingOverview . itemBody)
  <> listFieldWith "precepts" preceptRowCtx
       (\it -> mapM makeItem (pubcPrecepts (itemBody it)))

compilePreceptsFromCourseYaml
  :: FilePath
  -> Identifier
  -> Compiler (Item String)
compilePreceptsFromCourseYaml yamlPath templateId = do
  cy <- loadCourseYaml yamlPath

  blocks <- forM (cyUnits cy) $ \u -> do
    rows <- preceptRowsFromUnit yamlPath u
    pure $ PreceptUnitBlock
      (unitId u)
      (unitTitle u)
      (fromMaybe "" (unitInfo u))
      (fromMaybe "" (unitReadingOverview u))
      rows

  blockItems <- mapM makeItem blocks

  let ctx =
        constField "title" "Precepts"
        <> courseMetaCtx "precepts" (cyCourse cy)
        <> listField "units" preceptUnitBlockCtx (pure blockItems)
        <> defaultContext

  makeItem ("" :: String) >>= loadAndApplyTemplate templateId ctx

  
