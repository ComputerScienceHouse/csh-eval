{-|
Module      : CSH.Eval.Model
Description : CSH Eval state representation data structures.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Model defines all data structures required for representing and
manipulating the system state. Please note that the CSH.Eval.DB.Schema module
has much stricter documentation regarding the objects, including constitutional
cross-references.
-}

module CSH.Eval.Model (
    -- * /Enum/-like Types
    Committee(..)
  , EvaluationType(..)
  , EvaluationStatus(..)
  , MemberStatus(..)
  , DuesStatus(..)
  , EventType(..)
  , ProjectType(..)

    -- * Top-level Objects
  , Member(..)
  , Event(..)
  , Project(..)
  , Evaluation(..)
  , Conditional(..)
  , FreshmanProject(..)
  , Packet(..)
  , Application(..)
  , Metric(..)
  , Review(..)
  , Interview(..)
  , Question(..)
  , Term(..)

    -- * Contexts
  , Eboard(..)
  , Room(..)
  , Queue(..)
  , Membership(..)
  , EventAttendee(..)
  , ProjectParticipant(..)
  , FreshmanProjectParticipant(..)
  , Signature(..)
  , ReviewMetric(..)
  , InterviewMetric(..)
  , Answer(..)
  , Dues(..)

  -- * Cache
  , IDCache
  , Cache(..)
  , defTxMode
  , CacheM
  , Cacheable
  , CacheError(..)
  , runCacheable
  , execCacheable
  ) where

import Control.Concurrent.MVar

import Control.Monad.Trans.Either

import Data.Maybe

import Data.List

import Data.Time.Clock
import Data.Time.Calendar

import Data.UUID

import Data.Word

import qualified Data.ByteString as B

import qualified Data.Text       as T

import qualified Data.Map        as M

import Hasql
import Hasql.Postgres

import System.Log.Logger

-- | Committees.
data Committee = Evals     -- ^ Evaluations
               | RnD       -- ^ Research and Development
               | Social    -- ^ Social
               | History   -- ^ House History
               | OpComm    -- ^ Operations/Communications
               | Imps      -- ^ House Improvements
               | Financial -- ^ Financial
               | Chairman  -- ^ Chairman
               deriving (Eq, Show)

-- | Evaluation process type. TODO: Housing evals.
data EvaluationType = IntroEval      -- ^ Introductory member evaluations, i.e.
                                     --   fall evals.
                    | MembershipEval -- ^ Membership evaluations, i.e. spring
                                     --   evals.
                    deriving (Eq, Show)

-- | A context's status with respect to the corresponding evaluations process.
data EvaluationStatus = Pending -- ^ Evaluation pending
                      | Passed  -- ^ Evaluation passed/approved
                      | Failed  -- ^ Evaluation failed/dismissed
                      deriving (Eq, Show)

-- | A member's status with respect to the evaluations process.
data MemberStatus = Active     -- ^ Active (dues paying) member
                  | AlumniGood -- ^ Alumnus in good standing
                  | AlumniBad  -- ^ Alumnus in bad standing
                  | Honorary   -- ^ Andrew Potter, Jen Smith
                  | Advisory   -- ^ Advisory member
                  | Intro      -- ^ Introductory member
                  | Non        -- ^ Non-member
                  deriving (Eq, Show)

-- | A member's dues status.
data DuesStatus = Paid   -- ^ Dues paid.
                | Exempt -- ^ Dues exempt.
                deriving (Eq, Show)

-- | The type of a house event.
data EventType = HouseMeeting     -- ^ Weekly house meeting
               | SocialEvent      -- ^ Social event
               | CommitteeMeeting -- ^ Weekly committee meeting
               | Seminar          -- ^ Technical seminar
               | Orientation      -- ^ Orientation event
               deriving (Eq, Show)

-- | The type of a member-submitted project.
data ProjectType = Major -- ^ Technical major project
                 deriving (Eq, Show)

-- | Construct an equality function for a record type based on the value of a
--   single constructor.
mkEq :: Eq b => (a -> b) -> a -> a -> Bool
mkEq f = (\ a b -> (f a) == (f b))

-- | Representation of a member in any state, including alumni and introductory
--   members (even those that have failed; they might try again).
data Member = Member {
    -- | Each member is assigned a unique evals ID at creation time.
    memberID            :: Word64
    -- | Each member is assigned a unique UUID at creation time.
  , memberUUID          :: UUID
    -- | Each member chooses a unique username at creation time. Caveat emptor,
    --   this username can be very difficult to change later on.
  , memberUsername      :: T.Text
    -- | Each member enters a common name, e.g. "Travis Whitaker," at creation
    --   time. This name is /not/ guaranteed to be unique.
  , memberCommonname    :: T.Text
    -- | This token must be presented by a user's client to authenticate their
    --   activity; no token will be present if the user is not logged in.
  , memberToken         :: Maybe B.ByteString
    -- | Housing points determine member precedence when choosing rooms.
  , memberHousingPoints :: Int
    -- | Whether or not a member has onfloor status, i.e. the ability to
    --   participate in room selection.
  , memberOnfloorStatus :: Bool
    -- | A member's Eboard terms.
  , memberEboards       :: Cacheable [Eboard]
    -- | A member's room residences.
  , memberRooms         :: Cacheable [Room]
    -- | A member's membership terms.
  , memberMemberships   :: Cacheable [Membership]
    -- | A member's evaluations.
  , memberEvaluations   :: Cacheable [Evaluation]
    -- | A member's packets.
  , memberPackets       :: Cacheable [Packet]
    -- | A member's queue instantiation.
  , memberQueues        :: Cacheable [Queue]
    -- | A member's applications.
  , memberApplications  :: Cacheable [Application]
    -- | A member's dues.
  , memberDues          :: Cacheable [Dues]
  }

-- | Member records are equatable based on their IDs, which are guaranteed to
--   be unique.
instance Eq Member where
    (==) = mkEq memberID

-- | Only pure values in the member record are shown.
instance Show Member where
    show m = intercalate "\n"
        [ ("Member ID: " ++ (show $ memberID m))
        , ("Member UUID: " ++ (show $ memberUUID m))
        , ("Member Username: " ++ (show $ memberUsername m))
        , ("Member Common Name: " ++ (show $ memberCommonname m))
        , ("Member Logged In: " ++ (if (isJust $ memberToken m) then "True" else "False"))
        , ("Member Housing Points: " ++ (show $ memberHousingPoints m))
        , ("Member Onfloor Status: " ++ (show $ memberOnfloorStatus m))
        ]

-- | A house event.
data Event = Event {
    -- | Each event is assigned a unique ID at creation time.
    eventID          :: Word64
    -- | Each event is assigned a title at creation time. These titles are /not/
    --   guaranteed to be unique.
  , eventTitle       :: T.Text
    -- | When an event was held.
  , eventHeld        :: UTCTime
    -- | The event type.
  , eventCategory    :: EventType
    -- | The event committee.
  , eventCommittee   :: Committee
    -- | Event description.
  , eventDescription :: T.Text
    -- | Event attendees.
  , eventAttendees   :: Cacheable [EventAttendee]
  }

-- | Event records are equatable based on their IDs, which are guaranteed to be
--   unique.
instance Eq Event where
    (==) = mkEq eventID

-- | Only pure values in the event record are shown.
instance Show Event where
    show e = intercalate "\n"
        [ ("Event ID: " ++ (show $ eventID e))
        , ("Event Title: " ++ (show $ eventTitle e))
        , ("Event Held: " ++ (show $ eventHeld e))
        , ("Event Category: " ++ (show $ eventCategory e))
        , ("Event Committee: " ++ (show $ eventCommittee e))
        , ("Event Description: " ++ (show $ eventDescription e))
        ]

-- | A member submitted project.
data Project = Project {
    -- | A project is assigned a unique ID at creation time.
    projectID           :: Word64
    -- | A project is assigned a title at creation time. These titles are /not/
    --   guaranteed to be unique.
  , projectTitle        :: T.Text
    -- | A project's description.
  , projectDescription  :: T.Text
    -- | Project submission time.
  , projectSubmitted    :: UTCTime
    -- | When (and if) a project is passed.
  , projectPassed       :: Maybe UTCTime
    -- | The committee associated with a project.
  , projectCommittee    :: Committee
    -- | Project type.
  , projectType         :: ProjectType
    -- | Project reviewer comments.
  , projectComments     :: T.Text
    -- | Project evaluation status.
  , projectStatus       :: EvaluationStatus
    -- | Project participants.
  , projectParticipants :: Cacheable [ProjectParticipant]
  }

-- | Project records are equatable based on their IDs, which are guaranteed to
--   be unique.
instance Eq Project where
    (==) = mkEq projectID

-- | Only pure values in the project record are shown.
instance Show Project where
    show p = intercalate "\n"
        [ ("Project ID: " ++ (show $ projectID p))
        , ("Project Title: " ++ (show $ projectTitle p))
        , ("Project Description: " ++ (show $ projectDescription p))
        , ("Project Submitted: " ++ (show $ projectSubmitted p))
        , ("Project Passed: " ++ (show $ projectPassed p))
        , ("Project Committee: " ++ (show $ projectCommittee p))
        , ("Project Type: " ++ (show $ projectType p))
        , ("Project Comments: " ++ (show $ projectComments p))
        , ("Project Status: " ++ (show $ projectStatus p))
        ]

-- | A house evaluation of a particular member.
data Evaluation = Evaluation {
    -- | Each evaluation is assigned a unique ID at creation time.
    evaluationID           :: Word64
    -- | Evaluation comments.
  , evaluationComments     :: T.Text
    -- | Due date for this evaluation.
  , evaluationDeadline     :: UTCTime
    -- | Whether or not the evaluation result is publicly viewable.
  , evaluationAvailable    :: Bool
    -- | Evaluation status.
  , evaluationStatus       :: EvaluationStatus
    -- | Evaluation type.
  , evaluationType         :: EvaluationType
    -- | The member being evaluated.
  , evaluationMember       :: Cacheable Member
    -- | Evaluation conditionals.
  , evaluationConditionals :: Cacheable [Conditional]
    -- | Evaluation freshman projects.
  , evaluationFreshProject :: Cacheable [FreshmanProjectParticipant]
  }

-- | Evaluation records are equatable based on their IDs, which are guaranteed
--   to be unique.
instance Eq Evaluation where
    (==) = mkEq evaluationID

-- | Only pure values in the evaluation record are shown.
instance Show Evaluation where
    show e = intercalate "\n"
        [ ("Evaluation ID: " ++ (show $ evaluationID e))
        , ("Evaluation Comments: " ++ (show $ evaluationComments e))
        , ("Evaluation Deadline: " ++ (show $ evaluationDeadline e))
        , ("Evaluation Available: " ++ (show $ evaluationAvailable e))
        , ("Evaluation Status: " ++ (show $ evaluationStatus e))
        , ("Evaluation Type: " ++ (show $ evaluationType e))
        ]

-- | An additional evaluation contingency.
data Conditional = Conditional {
    -- | Each conditional is assigned a unique ID at creation time.
    conditionalID          :: Word64
    -- | Due date for the conditional.
  , conditionalDeadline    :: UTCTime
    -- | Conditional description.
  , conditionalDescription :: T.Text
    -- | Conditional evaluator comments.
  , conditionalComments    :: T.Text
    -- | The contingent evaluation.
  , conditionalEvaluation  :: Cacheable Evaluation
  }

-- | Conditional records are equatable based on their IDs, which are guaranteed
--   to be unique.
instance Eq Conditional where
    (==) = mkEq conditionalID

-- | Only pure values in the conditional record are shown.
instance Show Conditional where
    show c = intercalate "\n"
        [ ("Conditional ID: " ++ (show $ conditionalID c))
        , ("Conditional Deadline: " ++ (show $ conditionalDeadline c))
        , ("Conditional Description: " ++ (show $ conditionalDescription c))
        , ("Conditional Comments: " ++ (show $ conditionalComments c))
        ]

-- | Yearly freshman project.
data FreshmanProject = FreshmanProject {
    -- | Each freshman project is assigned a unique ID at creation time.
    freshmanProjectID           :: Word64
    -- | Freshman project description.
  , freshmanProjectDescription  :: T.Text
    -- | The term during which a freshman project takes place.
  , freshmanProjectTerm         :: Cacheable Term
    -- | The event representing a freshman project.
  , freshmanProjectEvent        :: Cacheable Event
    -- | Freshman project participants.
  , freshmanProjectParticipants :: Cacheable [FreshmanProjectParticipant]
  }

-- | Freshman project records are equatable based on their IDs, which are
--   guaranteed to be unique.
instance Eq FreshmanProject where
    (==) = mkEq freshmanProjectID

-- | Only pure values in the freshman project record are shown.
instance Show FreshmanProject where
    show f = intercalate "\n"
        [ ("Freshman Project ID: " ++ (show $ freshmanProjectID f))
        , ("Freshman Project Description: " ++ (show $ freshmanProjectDescription f))
        ]

-- | An intro member's packet.
data Packet = Packet {
    -- | Each packet is assigned a unique ID at creation time.
    packetID         :: Word64
    -- | Packet due date.
  , packetDueDate    :: UTCTime
    -- | Percentage of signatures required.
  , packetPercentReq :: Int
    -- | The member to whom this packet belongs.
  , packetMember     :: Cacheable Member
    -- | Packet signatures.
  , packetSignatures :: Cacheable [Signature]
  }

-- | Packet records are equatable based on their IDs, which are guaranteed to be
--   unique.
instance Eq Packet where
    (==) = mkEq packetID

-- | Only pure values in the packet record are shown.
instance Show Packet where
    show p = intercalate "\n"
        [ ("Packet ID: " ++ (show $ packetID p))
        , ("Packet Due Date: " ++ (show $ packetDueDate p))
        , ("Packet Percent Required: " ++ (show $ packetPercentReq p))
        ]

-- | A member's presence in the housing queue.
data Queue = Queue {
    -- | Queue entrance time.
    queueEntered :: UTCTime
    -- | Queue exit time.
  , queueExited  :: Maybe UTCTime
    -- | The member in the queue.
  , queueMember  :: Cacheable Member
  }

-- | Only pure values in the queue record are shown.
instance Show Queue where
    show q = intercalate "\n"
        [ ("Queue Entered: " ++ (show $ queueEntered q))
        , ("Queue Exited: " ++ (show $ queueExited q))
        ]

-- | A membership application.
data Application = Application {
    -- | Each application is assigned a unique ID at creation time.
    applicationID        :: Word64
    -- | Application creation time.
  , applicationCreated   :: UTCTime
    -- | Application status.
  , applicationStatus    :: EvaluationStatus
    -- | The application's member.
  , applicationMember    :: Cacheable Member
    -- | Application reviews written by the selections committee.
  , applicationReviews   :: Cacheable [Review]
    -- | Applicant interviews conducted by the selections committee.
  , applicationInterview :: Cacheable [Interview]
    -- | Applicant answers to the application questions.
  , applicationAnswers   :: Cacheable [Answer]
  }

-- | Application records are equatable based on their IDs, which are guaranteed
--   to be unique.
instance Eq Application where
    (==) = mkEq applicationID

-- | Only pure values in the application record are shown.
instance Show Application where
    show a = intercalate "\n"
        [ ("Application ID: " ++ (show $ applicationID a))
        , ("Application Created: " ++ (show $ applicationCreated a))
        , ("Application Status: " ++ (show $ applicationStatus a))
        ]

-- | A metric for use in a review or interview.
data Metric = Metric {
    -- | Each metric is assigned a unique ID at creation time.
    metricID     :: Word64
    -- | Metric name.
  , metricName   :: T.Text
    -- | Whether or not the metric should be used in newly created
    --   reviews/interviews
  , metricActive :: Bool
  }

-- | Metric records are equatable based on their IDs, which are guaranteed to be
--   unique.
instance Eq Metric where
    (==) = mkEq metricID

-- | Only pure values in the metric record are shown.
instance Show Metric where
    show m = intercalate "\n"
        [ ("Metric ID: " ++ (show $ metricID m))
        , ("Metric Name: " ++ (show $ metricName m))
        , ("Metric Active: " ++ (show $ metricActive m))
        ]

-- | A review of an introductory member application.
data Review = Review {
    -- | Each review is assigned a unique ID at creation time.
    reviewID          :: Word64
    -- | Review start time.
  , reviewStart       :: UTCTime
    -- | Review submission time.
  , reviewSubmit      :: UTCTime
    -- | The member conducting the review.
  , reviewMember      :: Cacheable Member
    -- | The application under review.
  , reviewApplication :: Cacheable Application
    -- | The values provided by the reviewer for each metric.
  , reviewMetrics     :: Cacheable [ReviewMetric]
  }

-- | Review records are equatable based on their IDs, which are guaranteed to be
--   unique.
instance Eq Review where
    (==) = mkEq reviewID

-- | Only pure values in the review record are shown.
instance Show Review where
    show r = intercalate "\n"
        [ ("Review ID: " ++ (show $ reviewID r))
        , ("Review Start: " ++ (show $ reviewStart r))
        , ("Review Submit: " ++ (show $ reviewSubmit r))
        ]

-- | An interview with a prospective introductory member.
data Interview = Interview {
    -- | Each interview is assigned a unique ID at creation time.
    interviewID          :: Word64
    -- | Interview date.
  , interviewDate        :: UTCTime
    -- | The member conducting the interview.
  , interviewMember      :: Cacheable Member
    -- | The application belonging to the prospective member.
  , interviewApplication :: Cacheable Application
    -- | The values provided by the interviewer for each metric.
  , interviewMetrics     :: Cacheable [InterviewMetric]
  }

-- | Interview records are equatable based on their IDs, which are guaranteed to
--   be unique.
instance Eq Interview where
    (==) = mkEq interviewID

-- | Only pure values in the interview record are shown.
instance Show Interview where
    show i = intercalate "\n"
        [ ("Interview ID: " ++ (show $ interviewID i))
        , ("Interview Date: " ++ (show $ interviewDate i))
        ]

-- | An introductory member application question.
data Question = Question {
    -- | Each question is assigned a unique ID at creation time.
    questionID     :: Word64
    -- | The question text.
  , questionQuery  :: T.Text
    -- | Whether or not the question should be used for newly created
    --   applications.
  , questionActive :: Bool
  }

-- | Question records are equatable based on their IDs, which are guaranteed to
--   be unique.
instance Eq Question where
    (==) = mkEq questionID

-- | Only pure values in the question record are shown.
instance Show Question where
    show q = intercalate "\n"
        [ ("Question ID: " ++ (show $ questionID q))
        , ("Question Query: " ++ (show $ questionQuery q))
        , ("Question Active: " ++ (show $ questionActive q))
        ]

-- | An RIT term (quarter, semester, etc.)
data Term = Term {
    -- | Each term is assigned a unique ID at creation time.
    termID        :: Word64
    -- | Term start date.
  , termStartDate :: Day
    -- | Term end date.
  , termEndDate   :: Maybe Day
  }

-- | Terms are equatable based on their IDs, which are guaranteed to be unique.
instance Eq Term where
    (==) = mkEq termID

-- | Only pure values in the term record are shown.
instance Show Term where
    show t = intercalate "\n"
        [ ("Term ID: " ++ (show $ termID t))
        , ("Term Start Date: " ++ (show $ termStartDate t))
        , ("Term End Date: " ++ (show $ termEndDate t))
        ]

-- | A member's term of service on Eboard.
data Eboard = Eboard {
    -- | The committee the member served as the head of.
    eboardCommittee :: Committee
    -- | Eboard term start date.
  , eboardStartDate :: Day
    -- | Eboard term end date.
  , eboardEndDate   :: Maybe Day
    -- | The member servind on Eboard.
  , eboardMember    :: Cacheable Member
  }

-- | Only pure values in the Eboard record are shown.
instance Show Eboard where
    show e = intercalate "\n"
        [ ("Eboard Committee: " ++ (show $ eboardCommittee e))
        , ("Eboard Start Date: " ++ (show $ eboardStartDate e))
        , ("Eboard End Date: " ++ (show $ eboardEndDate e))
        ]

-- | A member's residence in a room on floor.
data Room = Room {
    -- | Room number.
    roomNumber    :: T.Text
    -- | The day the member moved into the room.
  , roomStartDate :: Day
    -- | The day the member moved out of the room.
  , roomEndDate   :: Maybe Day
    -- | The resident member.
  , roomMember    :: Cacheable Member
  }

-- | Only pure values in the room record are shown.
instance Show Room where
    show r = intercalate "\n"
        [ ("Room Number: " ++ (show $ roomNumber r))
        , ("Room Start Date: " ++ (show $ roomStartDate r))
        , ("Room End Date: " ++ (show $ roomEndDate r))
        ]

-- | A member's membeship status over time.
data Membership = Membership {
    -- | Membership status.
    membershipStatus    :: MemberStatus
    -- | Membership status start date.
  , membershipStartDate :: Day
    -- | Membership status end date.
  , membershipEndDate   :: Maybe Day
    -- | The member in question.
  , membershipMember    :: Cacheable Member
  }

-- | Only pure values in the membership record are shown.
instance Show Membership where
    show m = intercalate "\n"
        [ ("Membership Status: " ++ (show $ membershipStatus m))
        , ("Membership Start Date: " ++ (show $ membershipStartDate m))
        , ("Membership End Date: " ++ (show $ membershipEndDate m))
        ]

-- | A member's attendance at an event.
data EventAttendee = EventAttendee {
   -- | Whether or not the attendee hosted the event.
   eventAttendeeHost   :: Bool
   -- | The attendee.
 , eventAttendeeMember :: Cacheable Member
   -- | The event.
 , eventAttendeeEvent  :: Cacheable Event
 }

-- | Only pure values in the event attendee record are shown.
instance Show EventAttendee where
    show e = intercalate "\n"
        [ ("Event Attendee Host: " ++ (show $ eventAttendeeHost e))
        ]

-- | A member's participation in a project.
data ProjectParticipant = ProjectParticipant {
    -- | The description of the participant's contribution.
    projectParticipantDescription :: T.Text
    -- | The participant.
  , projectParticipantMember      :: Cacheable Member
    -- | The project.
  , projectParticipantProject     :: Cacheable Project
  }

-- | Only pure values in the project participant record are shown.
instance Show ProjectParticipant where
    show p = intercalate "\n"
        [ ("Project Participant Description: " ++ (show $ projectParticipantDescription p))
        ]

-- | A member's participation in a freshman project.
data FreshmanProjectParticipant = FreshmanProjectParticipant {
    -- | Whether or not the participant served on freshman project eboard.
    freshmanProjectParticipantEboard     :: Bool
    -- | Evaluation status of the participant's contribution.
  , freshmanProjectParticipantStatus     :: EvaluationStatus
    -- | Evaluator comments on the participant's contribution.
  , freshmanProjectParticipantComments   :: T.Text
    -- | The freshman project.
  , freshmanProjectParticipantFreshProj  :: Cacheable FreshmanProject
    -- | The participant's evaluation.
  , freshmanProjectParticipantEvaluation :: Cacheable Evaluation
  }

-- | Only pure values in the freshman project participant record are shown.
instance Show FreshmanProjectParticipant where
    show f = intercalate "\n"
        [ ("Freshman Project Participant Eboard: " ++ (show $ freshmanProjectParticipantEboard f))
        , ("Freshman Project Participant Status: " ++ (show $ freshmanProjectParticipantStatus f))
        , ("Freshman Project Participant Comments: " ++ (show $ freshmanProjectParticipantComments f))
        ]

-- | A signature on a packet.
data Signature = Signature {
    -- | Whether or not this signature is required.
    signatureRequired :: Bool
    -- | Signature time.
  , signatureSigned   :: Maybe UTCTime
    -- | The signee.
  , signatureMember   :: Cacheable Member
    -- | The packet.
  , signaturePacket   :: Cacheable Packet
  }

-- | Only pure values in the signature record are shown.
instance Show Signature where
    show s = intercalate "\n"
        [ ("Signature Required: " ++ (show $ signatureRequired s))
        , ("Signature Signed: " ++ (show $ signatureSigned s))
        ]

-- | A metric value provided by a reviewer.
data ReviewMetric = ReviewMetric {
    -- | The metric value.
    reviewMetricScore  :: Int
    -- | The metric.
  , reviewMetricMetric :: Cacheable Metric
    -- | The review.
  , reviewMetricReview :: Cacheable Review
  }

-- | Only pure values in the review metric record are shown.
instance Show ReviewMetric where
    show r = intercalate "\n"
        [ ("Review Metric Score: " ++ (show $ reviewMetricScore r))
        ]

-- | A metric value provided by an interviewer.
data InterviewMetric = InterviewMetric {
    -- | The metric value.
    interviewMetricScore     :: Int
    -- | The metric.
  , interviewMetricMetric    :: Cacheable Metric
    -- | The review.
  , interviewMetricInterview :: Cacheable Interview
  }

-- | Only pure values in the interview metric record are shown.
instance Show InterviewMetric where
    show i = intercalate "\n"
        [ ("Interview Metric Score: " ++ (show $ interviewMetricScore i))
        ]

-- | An applicant's answer to an application question.
data Answer = Answer {
    -- | The applicant's answer.
    answerResponse    :: T.Text
    -- | The question.
  , answerQuestion    :: Cacheable Question
    -- | The application.
  , answerApplication :: Cacheable Application
  }

-- | Only pure values in the answer record are shown.
instance Show Answer where
    show a = intercalate "\n"
        [ ("Answer Response: " ++ (show $ answerResponse a))
        ]

-- | A member's dues status over time.
data Dues = Dues {
    -- | Dues status.
    duesStatus :: DuesStatus
    -- | The dues-paying member.
  , duesMember :: Cacheable Member
    -- | The term.
  , duesTerm   :: Cacheable Term
  }

-- | Only pure values in the dues record are shown.
instance Show Dues where
    show d = intercalate "\n"
        [ ("Dues Status: " ++ (show $ duesStatus d))
        ]

-- | A cache segment from database IDs to objects.
type IDCache a = MVar (M.Map Word64 (MVar a))

-- | A cache state, consisting of all cache segments and a database connection
--   pool for fallbacks (this must be accessible within the 'Cacheable' monad).
data Cache = Cache {
    -- | The PostgreSQL connection pool.
    pool                                  :: Pool Postgres
    -- | The Logger
  , logger                                :: Logger
    -- | Map from IDs to 'Member's.
  , memberIDCache                         :: IDCache Member
    -- | Map from IDs to 'Event's.
  , eventIDCache                          :: IDCache Event
    -- | Map from IDs to 'Project's.
  , projectIDCache                        :: IDCache Project
    -- | Map from IDs to 'Evaluation's.
  , evaluationIDCache                     :: IDCache Evaluation
    -- | Map from IDs to 'Conditional's.
  , conditionalIDCache                    :: IDCache Conditional
    -- | Map from IDs to 'FreshmanProject's.
  , freshmanProjectIDCache                :: IDCache FreshmanProject
    -- | Map from IDs to 'Packet's.
  , packetIDCache                         :: IDCache Packet
    -- | Map from IDs to 'Queue's.
  , queueIDCache                          :: IDCache Queue
    -- | Map from IDs to 'Application's.
  , applicationIDCache                    :: IDCache Application
    -- | Map from IDs to 'Metric's.
  , metricIDCache                         :: IDCache Metric
    -- | Map from IDs to 'Review's.
  , reviewIDCache                         :: IDCache Review
    -- | Map from IDs to 'Interview's.
  , interviewIDCache                      :: IDCache Interview
    -- | Map from IDs to 'Question's.
  , questionIDCache                       :: IDCache Question
    -- | Map from IDs to 'Term's.
  , termIDCache                           :: IDCache Term
    -- | map from 'Member' IDs to 'Eboard's.
  , eboardMemberIDCache                   :: IDCache [Eboard]
    -- | Map from 'Member' IDs to 'Evaluation's.
  , evaluationMemberIDCache               :: IDCache [Evaluation]
    -- | Map from 'Evaluation' IDs to 'Conditional's.
  , conditionalEvaluationIDCache          :: IDCache [Conditional]
    -- | Map from 'Member' IDs to 'Room's.
  , roomMemberIDCache                     :: IDCache [Room]
    -- | Map from 'Member' IDs to 'Queue's.
  , queueMemberIDCache                    :: IDCache [Queue]
    -- | Map from 'Member' IDs to 'Membership's.
  , membershipMemberIDCache               :: IDCache [Membership]
    -- | Map from 'Member' IDs to 'EventAttendee's.
  , eventAttendeeMemberIDCache            :: IDCache [EventAttendee]
    -- | Map from 'Event' IDs to 'EventAttendee's.
  , eventAttendeeEventIDCache             :: IDCache [EventAttendee]
    -- | Map from 'Member' IDs to 'ProjectParticipant's.
  , projectParticipantMemberIDCache       :: IDCache [ProjectParticipant]
    -- | Map from 'Project' IDs to 'ProjectParticipant's.
  , projectParticipantProjectIDCache      :: IDCache [ProjectParticipant]
    -- | Map from 'FreshmanProject' IDs to 'FreshmanProjectParticipant's.
  , freshProjParticipantProjectIDCache    :: IDCache [FreshmanProjectParticipant]
    -- | Map from 'Evaluation' IDs to 'FreshmanProjectParticipant's.
  , freshProjParticipantEvaluationIDCache :: IDCache [FreshmanProjectParticipant]
    -- | Map from 'Member' IDs to 'Packet's.
  , packetMemberIDCache                   :: IDCache [Packet]
    -- | Map from 'Member' IDs to 'Signature's.
  , signatureMemberIDCache                :: IDCache [Signature]
    -- | Map from 'Packet' IDs to 'Signature's.
  , signaturePacketIDCache                :: IDCache [Signature]
    -- | Map from 'Member' IDs to 'Application's.
  , applicationMemberIDCache              :: IDCache [Application]
    -- | Map from 'Application' IDs to 'Review's.
  , reviewApplicationIDCache              :: IDCache [Review]
    -- | Map from 'Application' IDs to 'Interview's.
  , interviewApplicationIDCache           :: IDCache [Interview]
    -- | Map from 'Metric' IDs to 'ReviewMetric's.
  , reviewMetricMetricIDCache             :: IDCache [ReviewMetric]
    -- | Map from 'Review' IDs to 'ReviewMetric's.
  , reviewMetricReviewIDCache             :: IDCache [ReviewMetric]
    -- | Map from 'Metric' IDs to 'InterviewMetric's.
  , interviewMetricMetricIDCache          :: IDCache [InterviewMetric]
    -- | Map from 'Interview' IDs to 'InterviewMetric's.
  , interviewMetricInterviewIDCache       :: IDCache [InterviewMetric]
    -- | Map from 'Question' IDs to 'Answer's.
  , answerQuestionIDCache                 :: IDCache [Answer]
    -- | Map from 'Application' IDs to 'Answer's.
  , answerApplicationIDCache              :: IDCache [Answer]
    -- | Map from 'Member' IDs to 'Dues'.
  , duesMemberIDCache                     :: IDCache [Dues]
    -- | Map from 'Term' IDs to 'Dues'.
  , duesTermIDCache                       :: IDCache [Dues]
  }

-- | Default Hasql transaction mode.
defTxMode :: TxMode
defTxMode = Just (Serializable, (Just True))

-- | Interior transformer for operations directly on the cache state. Cache
--   users should never be able to bind out of this.
type CacheM a = EitherT CacheError IO a

-- | Exterior transformer for cache operations. Cache API caller-facing
--   functions /must/ return into this exterior transformer.
type Cacheable a = Cache -> CacheM a

-- | Cache error.
data CacheError = HasqlError (SessionError Postgres)
                | CacheError  String
                | Nonexistent String
                | Constraint  String
                deriving (Show)

-- | Enables embedding the interior cache transformer within another
--   transformer. This is only OK to use if you're embedding something
--   'Cacheable' in a different exterior transformer. I'm not sure how to
--   enforce that at the type level. You probably shouldn't use this.
runCacheable :: Cache -> Cacheable a -> CacheM a
runCacheable c m = m c

-- | Hoist 'Cacheable' into IO.
execCacheable :: Cache -> Cacheable a -> IO (Either CacheError a)
execCacheable c m = runEitherT (m c)
