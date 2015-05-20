{-|
Module      : CSH.Eval.Model
Description : CSH Eval data structures.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Model.Member defines all data structures required for representing and
manipulating the system state.
-}

module CSH.Eval.Model where

import Data.Maybe

import Data.List

import Data.Time.Clock

import Data.UUID

import Data.Word

import qualified Data.ByteString as B

import qualified Data.Text       as T

-- | The 'Shadow' typeclass is for types whose elements are uniquely mappable
--   to a natural number. This allows us build a cache of large data structures
--   whose constituent parts may not be known yet. If data might arbitrarily
--   change underneath the transactional processes inspecting it, we can't
--   rely on lazy IO alone, and need a way to construct pure, immutable
--   references to large, unreliable data structures.
--
--   The 'Word64' type is used as a rough analogue to the natrual numbers.
class Shadow a where
    shadow :: Word64 -> a

-- | The 'Shadowed' type represents a part of a data structure that might not be
--   known yet. It is isomorphic to the 'Maybe' type.
type Shadowed a = Maybe a

-- | Purely show a shadowed structure.
mshow :: Show a => Shadowed a -> String
mshow (Just x) = show x
mshow Nothing  = "<unspecified>"

data Member = Member {
    memberID            :: Word64
   ,memberUUID          :: Shadowed UUID
   ,memberUsername      :: Shadowed T.Text
   ,memberCommonname    :: Shadowed T.Text
   ,memberToken         :: Shadowed B.ByteString
   ,memberHousingPoints :: Shadowed Int
   ,memberOnfloorStatus :: Shadowed Bool
   ,memberEboards       :: Shadowed [Eboard]
   ,memberRooms         :: Shadowed [Room]
   ,memberMemberships   :: Shadowed [Membership]
   ,memberEvaluations   :: Shadowed [Evaluation]
   ,memberPackets       :: Shadowed [Packet]
   ,memberQueues        :: Shadowed [Queue]
   ,memberApplications  :: Shadowed [Application]
   ,memberDues          :: Shadowed [Dues]
   }

instance Eq Member where
    a == b = (memberID a) == (memberID b)

instance Show Member where
    show m = intercalate "\n"
        [("Member ID: " ++ (show $ memberID m))
       ,("Member UUID: " ++ (mshow $ memberUUID m))
       ,("Member Username: " ++ (mshow $ memberUsername m))
       ,("Member Common Name: " ++ (mshow $ memberCommonname m))
       ,("Member Logged In: " ++ (if (isJust $ memberToken m) then "True" else "False"))
       ,("Member Housing Points: " ++ (mshow $ memberHousingPoints m))
       ,("Member Onfloor Status: " ++ (mshow $ memberOnfloorStatus m))
       ,("Member Eboard Service: " ++ (mshow $ memberEboards m))
       ,("Member Residence: " ++ (mshow $ memberRooms m))
       ,("Member Status History: " ++ (mshow $ memberMemberships m))
       ,("Member Evaluation History: " ++ (mshow $ memberEvaluations m))
       ,("Member Packet History: " ++ (mshow $ memberPackets m))
       ,("Member Queue History: " ++ (mshow $ memberQueues m))
       ,("Member Application History: " ++ (mshow $ memberApplications m))
       ,("Member Dues History: " ++ (mshow $ memberDues m))
       ]

instance Shadow Member where
    shadow i = Member i
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing 

data Committee = Evals     |
                 RnD       |
                 Social    |
                 History   |
                 OpComm    |
                 Imps      |
                 Financial |
                 Chairman deriving (Eq, Show)

data Eboard = Eboard {
    eboardCommittee :: Committee
   ,eboardStartDate :: UTCTime
   ,eboardEndDate   :: UTCTime
   } deriving (Eq, Show)

data Room = Room {
    roomNumber    :: T.Text
   ,roomStartDate :: UTCTime
   ,roomEndDate   :: UTCTime
   } deriving (Eq, Show)

data MemberStatus = Active       |
                    AlumniGood   |
                    AlumniBad    |
                    Honorary     |
                    Advisory     |
                    Intro        |
                    Non deriving (Eq, Show)


data Membership = Membership {
    membershipStatus    :: MemberStatus
   ,membershipStartDate :: UTCTime
   ,membershipEndDate   :: UTCTime
   } deriving (Eq, Show)

data Queue = Queue {
    queueStartDate :: UTCTime
   ,queueEndDate   :: UTCTime
   } deriving (Eq, Show)

data DuesStatus = Paid   |
                  Exempt deriving (Eq, Show)

data Dues = Dues {
    duesTerm :: Term
   ,duesStatus :: DuesStatus
   } deriving (Show)

data Term = Term {
    termID        :: Word64
   ,termStartDate :: Shadowed UTCTime
   ,termEndDate   :: Shadowed UTCTime
   }

instance Eq Term where
    a == b = (termID a) == (termID b)

instance Show Term where
    show t = "Term {termID = " ++ (show $ termID t) ++
             ", termStartDate = " ++ (mshow $ termStartDate t) ++
             ", termEndDate = " ++ (mshow $ termEndDate t) ++ "}"

instance Shadow Term where
    shadow i = Term i Nothing Nothing

data EvaluationStatus = Pending |
                        Passed  |
                        Failed deriving (Eq, Show)

data EvaluationType = IntroEval |
                      MembershipEval deriving (Eq, Show)

data Evaluation = Evaluation {
    evaluationID              :: Word64
   ,evaluationComments        :: Shadowed T.Text
   ,evaluationDeadline        :: Shadowed UTCTime
   ,evaluationAvailability    :: Shadowed Bool
   ,evaluationStatus          :: Shadowed EvaluationStatus
   ,evaluationType            :: Shadowed EvaluationType
   ,evaluationConditional     :: Shadowed (Maybe Conditional)
   ,evaluationFreshProjParts  :: Shadowed (Maybe FreshmanProjectParticipant)
   }

instance Eq Evaluation where
    a == b = (evaluationID a) == (evaluationID b)

instance Show Evaluation where
    show e = intercalate "\n"
        [("Evaluation ID: " ++ (show $ evaluationID e))
       ,("Evaluation Comments: " ++ (mshow $ evaluationComments e))
       ,("Evaluation Deadline: " ++ (mshow $ evaluationDeadline e))
       ,("Evaluation Availability: " ++ (mshow $ evaluationAvailability e))
       ,("Evaluation Status: " ++ (mshow $ evaluationStatus e))
       ,("Evaluation Type: " ++ (mshow $ evaluationType e))
       ,("Evaluation Conditional: " ++ (mshow $ evaluationType e))
       ,("Evaluation Freshman Project Participant: " ++ (mshow $ evaluationFreshProjParts e))
       ]

instance Shadow Evaluation where
    shadow i = Evaluation i
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing

data Conditional = Conditional {
    conditionalDeadline    :: UTCTime
   ,conditionalDescription :: T.Text
   ,conditionalComments    :: T.Text
   } deriving (Eq, Show)

data FreshmanProjectParticipant = FreshmanProjectParticipant {
    freshProjPartProject  :: FreshmanProject
   ,freshProjPartEboard   :: Bool
   ,freshProjPartStatus   :: EvaluationStatus
   ,freshProjPartComments :: T.Text
   } deriving (Show)

data FreshmanProject = FreshmanProject {
    freshProjID          :: Word64
   ,freshProjDescription :: Shadowed T.Text
   ,freshProjDate        :: Shadowed UTCTime
   }

instance Eq FreshmanProject where
    a == b = (freshProjID a) == (freshProjID b)

instance Show FreshmanProject where
    show f = intercalate "\n"
        [("Freshman Project ID: " ++ (show $ freshProjID f))
       ,("Freshman Project Description: " ++ (mshow $ freshProjDescription f))
       ,("Freshman Project Date: " ++ (mshow $ freshProjDate f))
       ]

instance Shadow FreshmanProject where
    shadow i = FreshmanProject i Nothing Nothing

data Packet = Packet {
    packetID         :: Word64
   ,packetDueDate    :: Shadowed UTCTime
   ,packetPercentReq :: Shadowed Int
   ,packetSignatures :: Shadowed [Signature]
   }

instance Eq Packet where
    a == b = (packetID a) == (packetID b)

instance Show Packet where
    show p = intercalate "\n"
        [("Packet ID: " ++ (show $ packetID p))
       ,("Packet Due Date: " ++ (mshow $ packetDueDate p))
       ,("Packet Percent Required: " ++ (mshow $ packetPercentReq p))
       ,("Packet Signatures: " ++ (mshow $ packetSignatures p))
       ]

instance Shadow Packet where
    shadow i = Packet i Nothing Nothing Nothing

data Signature = Signature {
    signatureMember   :: Member
   ,signatureRequired :: Bool
   ,signatureSigned   :: Maybe UTCTime
   } deriving (Eq, Show)

data Application = Application {
    applicationID           :: Word64
   ,applicationCreated      :: Shadowed UTCTime
   ,applicationStatus       :: Shadowed EvaluationStatus
   ,applicationReviewers    :: Shadowed [Reviewer]
   ,applicationInterviewers :: Shadowed [Interviewer]
   ,applicationAnswers      :: Shadowed [Answer]
   }

instance Eq Application where
    a == b = (applicationID a) == (applicationID b)

instance Show Application where
    show a = intercalate "\n"
        [("Application ID: " ++ (show $ applicationID a))
       ,("Application Creation Date: " ++ (mshow $ applicationCreated a))
       ,("Application Status: " ++ (mshow $ applicationStatus a))
       ,("Application Reviewers: "  ++ (mshow $ applicationReviewers a))
       ,("Application Interviewers: " ++ (mshow $ applicationInterviewers a))
       ,("Application Prompt Answers: " ++ (mshow $ applicationAnswers a))
       ]

instance Shadow Application where
    shadow i = Application i Nothing Nothing Nothing Nothing Nothing

data Reviewer = Reviewer {
    reviewerID      :: Word64
   ,reviewerMember  :: Shadowed Member
   ,reviewerStart   :: Shadowed UTCTime
   ,reviewerSubmit  :: Shadowed UTCTime
   ,reviewerMetrics :: Shadowed [ReviewerMetric]
   }

instance Eq Reviewer where
    a == b = (reviewerID a) == (reviewerID b)

instance Show Reviewer where
    show r = intercalate "\n"
        [("Reviewer ID: " ++ (show $ reviewerID r))
       ,("Reviewer Member: " ++ (mshow $ reviewerMember r))
       ,("Reviewer Start Time: " ++ (mshow $ reviewerStart r))
       ,("Reviwer Submission Time: " ++ (mshow $ reviewerSubmit r))
       ,("Reviwer Metrics: " ++ (mshow $ reviewerMetrics r))
      ]

instance Shadow Reviewer where
    shadow i = Reviewer i Nothing Nothing Nothing Nothing

data Interviewer = Interviewer {
    interviewerID       :: Word64
   ,interviewerMember   :: Shadowed Member
   ,interviewerStart    :: Shadowed UTCTime
   ,interviewerMetrics  :: Shadowed [InterviewerMetric]
   }

instance Eq Interviewer where
    a == b = (interviewerID a) == (interviewerID b)

instance Show Interviewer where
    show r = intercalate "\n"
        [("Interviewer ID: " ++ (show $ interviewerID r))
       ,("Interviewer Member: " ++ (mshow $ interviewerMember r))
       ,("Interviewer Start Time: " ++ (mshow $ interviewerStart r))
       ,("Interviewer Metrics: " ++ (mshow $ interviewerMetrics r))
      ]

instance Shadow Interviewer where
    shadow i = Interviewer i Nothing Nothing Nothing

data ReviewerMetric = ReviewerMetric {
    reviewerMetricMetric :: Metric
   ,reviewerMetricScore  :: Int
   } deriving (Eq, Show)

data InterviewerMetric = InterviewerMetric {
    interviewerMetricMetric :: Metric
   ,interviewerMetricScore  :: Int
   } deriving (Eq, Show)

data Metric = Metric {
    metricID     :: Word64
   ,metricName   :: Shadowed T.Text
   ,metricActive :: Shadowed Bool
   }

instance Eq Metric where
    a == b = (metricID a) == (metricID b)

instance Show Metric where
    show m = "Metric {metricID = " ++ (show $ metricID m)
             ++ ", metricName = " ++ (mshow $ metricName m)
             ++ ", metricActive = " ++ (mshow $ metricActive m)
             ++ "}"

instance Shadow Metric where
    shadow i = Metric i Nothing Nothing

data Answer = Answer {
    answerQuestion :: Question
   ,answerResponse :: T.Text
   } deriving (Eq, Show)

data Question = Question {
    questionID     :: Word64
   ,questionActive :: Shadowed Bool
   ,questionQuery  :: Shadowed T.Text
   }

instance Eq Question where
    a == b = (questionID a) == (questionID b)

instance Show Question where
    show q = "Question {questionID = " ++ (show $ questionID q)
             ++ ", questionActive = " ++ (mshow $ questionActive q)
             ++ ", questionQuery = " ++ (mshow $ questionQuery q)
             ++ "}"

instance Shadow Question where
    shadow i = Question i Nothing Nothing

data HousingEval = HousingEval {
    housingEvalID     :: Word64
   ,housingEvalDate   :: Shadowed UTCTime
   ,housingEvaluators :: Shadowed [HousingEvaluator]
   }

instance Eq HousingEval where
    a == b = (housingEvalID a) == (housingEvalID b)

instance Show HousingEval where
    show h = intercalate "\n"
        [("Housing Eval ID: " ++ (show $ housingEvalID h))
       ,("Housing Eval Date: " ++ (mshow $ housingEvalDate h))
       ,("Housing Evaluators: " ++ (mshow $ housingEvaluators h))
       ]

instance Shadow HousingEval where
    shadow i = HousingEval i Nothing Nothing

data HousingEvaluator = HousingEvaluator {
    housingEvaluatorMember :: Member
   ,housingEvaluatorScore  :: Int
   ,housingEvaluatorVoted  :: Bool
   } deriving (Eq, Show)

data EventType = HouseMeeting     |
                 SocialEvent      |
                 CommitteeMeeting |
                 Seminar          |
                 Orientation deriving (Eq, Show)

data Event = Event {
    eventID          :: Word64
   ,eventTitle       :: Shadowed T.Text
   ,eventHeld        :: Shadowed UTCTime
   ,eventCategory    :: Shadowed EventType
   ,eventCommittee   :: Shadowed Committee
   ,eventDescription :: Shadowed T.Text
   ,eventAttendees   :: Shadowed [EventAttendee]
   }

instance Eq Event where
    a == b = (eventID a) == (eventID b)

instance Show Event where
    show e = intercalate "\n"
        [("Event ID: " ++ (show $ eventID e))
       ,("Event Title: " ++ (mshow $ eventTitle e))
       ,("Event Held: " ++ (mshow $ eventHeld e))
       ,("Event Category: " ++ (mshow $ eventCategory e))
       ,("Event Committee: " ++ (mshow $ eventCommittee e))
       ,("Event Description: " ++ (mshow $ eventDescription e))
       ]

instance Shadow Event where
    shadow i = Event i
        Nothing Nothing Nothing
        Nothing Nothing Nothing

data EventAttendee = EventAttendee {
    eventAttendeeMember :: Member
   ,eventAttendeeHost   :: Bool
   } deriving (Eq, Show)

data ProjectType = Major deriving (Eq, Show)

data Project = Project {
    projectID           :: Word64
   ,projectTitle        :: Shadowed T.Text
   ,projectDescription  :: Shadowed T.Text
   ,projectSubmitted    :: Shadowed UTCTime
   ,projectPassed       :: Shadowed (Maybe UTCTime)
   ,projectCommittee    :: Shadowed Committee
   ,projectType         :: Shadowed ProjectType
   ,projectComments     :: Shadowed T.Text
   ,projectStatus       :: Shadowed EvaluationStatus
   ,projectParticipants :: Shadowed [ProjectParticipant]
   }

instance Eq Project where
    a == b = (projectID a) == (projectID b)

instance Show Project where
    show p = intercalate "\n"
        [("Project ID: " ++ (show $ projectID p))
       ,("Project Title: " ++ (mshow $ projectTitle p))
       ,("Project Description: " ++ (mshow $ projectDescription p))
       ,("Project Submission Time: " ++ (mshow $ projectSubmitted p))
       ,("Project Pass Time: "       ++ (mshow $ projectPassed p))
       ,("Project Committee: " ++ (mshow $ projectCommittee p))
       ,("Project Type: " ++ (mshow $ projectType p))
       ,("Project Comments: " ++ (mshow $ projectComments p))
       ,("Project Status: " ++ (mshow $ projectStatus p))
       ,("Project Participants: " ++ (mshow $ projectParticipants p))
       ]

instance Shadow Project where
    shadow i = Project i
        Nothing Nothing Nothing
        Nothing Nothing Nothing
        Nothing Nothing Nothing

data ProjectParticipant = ProjectParticipant {
    projPartMember :: Member
   ,projPartDescription :: T.Text
   } deriving (Eq, Show)
