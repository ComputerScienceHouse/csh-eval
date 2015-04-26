module CSH.Eval.DB.Statements where

import Database.HDBC

import Database.HDBC.PostgreSQL

data StatementGroup = StatementGroup {
    asfdS :: Statement
   }

type StatementInitializer :: Connection -> IO Statement

mkInit :: String -> StatementInitializer
mkInit = flip prepare

-- Object Initializers:

-- Member:

mkIntroMemberP :: StatementInitializer
mkIntroMemberP = mkInit "insert into \"member\" (\"uuid\", \"username\", \"commonname\", \"password_hash\", \"password_salt\") values (?, ?, ?, ?, ?);"

mkExtantMemberP :: StatementInitializer
mkExtantMemberP = mkInit "insert into \"member\" (\"uuid\", \"username\", \"commonname\", \"housing_points\", \"onfloor_status\") values (?, ?, ?, ?, ?);"

-- Event:

mkEventP :: StatementInitializer
mkEventP = mkInit "insert into \"event\" (\"title\", \"held\", \"category\", \"committee\", \"description\") values (?, ?, ?, ?, ?);"

-- Project:

mkProjectP :: StatementInitializer
mkProjectP = mkInit "insert into \"project\" (\"title\", \"description\", \"submitted\", \"committee\", \"project_type\") values (?, ?, ?, ?, ?);"

-- Evaluation:

mkEvaluationP :: StatementInitializer
mkEvaluationP = mkInit "insert into \"evaluation\" (\"member_id\", \"deadline\", \"eval_type\") values (?, ?, ?);"

-- Conditional:

mkConditionalP :: StatementInitializer
mkConditionalP = mkInit "insert into \"conditional\" (\"evaluation_id\", \"deadline\", \"description\") values (?, ?, ?);"

-- Freshman Project:

mkFreshmanProjectP :: StatementInitializer
mkFreshmanProjectP = mkInit "insert into \"freshman_project\" (\"description\", \"project_date\") values (?, ?);"

-- Packet:

mkPacketP :: StatementInitializer
mkPacketP = mkInit "insert into \"packet\" (\"member_id\", \"due_date\", \"percent_req\") values (?, ?, ?);"

-- Queue:

mkQueueP :: StatementInitializer
mkQueueP = mkInit "insert into \"queue\" (\"member_id\", \"entered\") values (?, ?);"

-- Application:

mkApplicationP :: StatementInitializer
mkApplicationP = mkInit "insert into \"application\" (\"member_id\", \"created\") values (?, ?);"

-- Metric:

mkMetricP :: StatementInitializer
mkMetricP = mkInit "insert into \"metric\" (\"name\") values (?);"

-- Reviewer:

mkReviewerP :: StatementInitializer
mkReviewerP = mkInit "inset into \"reviewer\" (\"member_id\", \"application_id\", \"review_start\", \"review_submit\") values (?, ?, ?, ?);"

-- Interviewer:

mkInterviewerP :: StatementInitializer
mkInterviewerP = mkInit "insert into \"interviewer\" (\"member_id\", \"application_id\", \"interview_date\") values (?, ?, ?);"

-- Question:

mkQuestionP :: StatementInitializer
mkQuestionP = mkInit "insert into \"question\" (\"query\") values (?);"

-- Housing Eval:

mkHousingEvalP :: StatementInitializer
mkHousingEvalP = mkInit "insert into \"housing_eval\" (\"eval_date\") values (?);"

-- Term:

mkTermP :: StatementInitializer
mkTermP = mkInit "insert into \"term\" (\"start_date\") values (?);"

-- Statement:

mkStatementP :: StatementInitializer
mkStatementP = mkInit "insert into \"statement\" (\"sg_record\", \"side_effects\") values (?, ?);"

-- Context Initializers:

-- Eboard:

grEboardP :: StatementInitializer
grEboardP = mkInit "insert into \"eboard\" (\"member_id\", \"committee\", \"start_date\") values (?, ?, ?);"

-- Room:

grRoomP :: StatementInitializer
grRoomP = mkInit "insert into \"room\" (\"member_id\", \"room_number\", \"start_date\") values (?, ?, ?);"

-- Membership:

grMembershipP :: StatementInitializer
grMembershipP = mkInit "insert into \"membership\" (\"member_id\", \"status\", \"start_date\") values (?, ?, ?);"

-- Event Attendee:

grEventAttendeeP :: StatementInitializer
grEventAttendeeP = mkInit "insert into \"event_attendee\" (\"member_id\", \"event_id\") values (?, ?);"

-- Project Participant

grProjectParticipantP :: StatementInitializer
mkProjectParticipantP = mkInit "insert into \"project_participant\" (\"member_id\", \"project_id\", \"description\") values (?, ?, ?)"

-- Freshman Project Participant

grFreshmanProjectParticipantP :: StatementInitializer
mkFreshmanProjectParticipantP = mkInit "insert into \"freshman_project_participant\" (\"freshman_project_id\", \"evaluation_id\") values (?, ?);"

-- Signature:

grSignatureP :: StatementInitializer
grSignatureP = mkInit "insert into \"signature\" (\"member_id\", \"packet_id\", \"required\") values (?, ?, ?);"

-- Reviewer Metric:

grReviewerMetricP :: StatementInitializer
grReviewerMetricP = mkInit "insert into \"reviewer_metric\" (\"metric_id\", \"reviewer_id\", \"score\") values (?, ?, ?);"

-- Interviewer Metric:

grInterviewerMetricP :: StatementInitializer
grInterviewerMetricP = mkInit "insert into \"interviewer_metric\" (\"metric_id\", \"interviewer_id\", \"score\") values (?, ?, ?);"

-- Answer:

grAnswerP :: StatementInitializer
grAnswerP = mkInit "insert into \"answer\" (\"application_id\", \"question_id\", \"response\") values (?, ?, ?);"

-- Housing Evaluator:

grHousingEvaluatorP :: StatementInitializer
grHousingEvaluatorP = mkInit "insert into \"housing_evaluator\" (\"housing_eval_id\", \"member_id\", \"score\") values (?, ?, ?);"

-- Dues:

grDuesP :: StatementInitializer
grDuesP = mkInit "insert into \"dues\" (\"term_id\", \"member_id\", \"status\") values (?, ?, ?);"

-- Statement Exec:

grStatementExecP :: StatementInitializer
grStatementExecP = mkInit "insert into \"statement_exec\" (\"statement_id\", \"member_id\", \"timestamp\") values (?, ?, ?);"

-- Object Mutators:

-- Member:

upMemberCommonNameP :: StatementInitializer
upMemberCommonNameP = mkInit "update \"member\" set \"commonname\" = ? where \"id\" = ?;"

upMemberPasswordP :: StatementInitializer
upMemberPasswordP = mkInit "update \"member\" set \"password_hash\" = ?, \"password_salt\" = ? where \"id\" = ?;"

upMemberHousingPointsP :: StatementInitializer
upMemberHousingPointsP = mkInit "update \"member\" set \"housing_points\" = ? where \"id\" = ?;"

upMemberOnfloorStatusP :: StatementInitializer
upMemberOnfloorStatusP = mkInit "update \"member\" set \"onfloor_status\" = ? where \"id\" = ?;"

-- Event:

upEventTitleP :: StatementInitializer
upEventTitleP = mkInit "update \"event\" set \"title\" = ? where \"id\" = ?;"

upEventHeldP :: StatementInitializer
upEventHelpP = mkInit "update \"event\" set \"held\" = ? where \"id\" = ?;"

upEventCategoryP :: StatementInitializer
upEventCategoryP = mkInit "update \"event\" set \"category\" = ? where \"id\" = ?;"

upEventCommitteeP :: StatementInitializer
upEventCommitteeP = mkInit "update \"event\" set \"committee\" = ? where \"id\" = ?;"

upEventDescriptionP :: StatementInitializer
upEventDescriptionP = mkInit "update \"event\" set \"description\" = ? where \"id\" = ?;"

-- Project:

upProjectTitleP :: StatementInitializer
upProjectTitleP = mkInit "update \"project\" set \"title\" = ? where \"id\" = ?;"

upProjectDescriptionP :: StatementInitializer
upProjectDescriptionP = mkInit "update \"project\" set \"description\" = ? where \"id\" = ?;"

upProjectSubmittedP :: StatementInitializer
upProjectSubmittedP = mkInit "update \"project\" set \"submitted\" = ? where \"id\" = ?;"

upProjectPassedP :: StatementInitializer
upProjectPassedP = mkInit "update \"project\" set \"passed\" = ? where \"id\" = ?;"

upProjectCommitteeP :: StatementInitializer
upProjectCommitteeP = mkInit "update \"project\" set \"committee\" = ? where \"id\" = ?;"

upProjectTypeP :: StatementInitializer
upProjectTypeP = mkInit "update \"project\" set \"project_type\" = ? where \"id\" = ?;"

upProjectCommentsP :: StatementInitializer
upProjectCommentsP = mkInit "update \"project\" set \"comments\" = ? where \"id\" = ?;"

upProjectStatusP :: StatementInitializer
upProjectStatusP = mkInit "update \"project\" set \"status\" = ? where \"id\" = ?;"

-- Evaluation:

upEvaluationCommentsP :: StatementInitializer
upEvaluationCommentsP = mkInit "update \"evaluation\" set \"comments\" = ? where \"id\" = ?;"

upEvaluationDeadlineP :: StatementInitializer
upEvaluationDeadlineP = mkInit "update \"evaluation\" set \"deadline\" = ? where \"id\" = ?;"

upEvaluationAvailableP :: StatementInitializer
upEvaluationAvailableP = mkInit "update \"evaluation\" set \"available\" = ? where \"id\" = ?;"

upEvaluationStatusP :: StatementInitializer
upEvaluationStatusP = mkInit "update \"evaluation\" set \"status\" = ? where \"id\" = ?;"

upEvaluationTypeP :: StatementInitializer
upEvaluationTypeP = mkInit "update \"evaluation\" set \"eval_tyep\" = ? where \"id\" = ?;"

-- Conditional:

upConditionalDeadlineP :: StatementInitializer
upConditionalDeadlineP = mkInit "update \"conditional\" set \"deadline\" = ? where \"id\" = ?;"

upConditionalDescriptionP :: StatementInitializer
upConditionalDescriptionP = mkInit "update \"conditional\" set \"description\" = ? where \"id\" = ?;"

upConditionalCommentsP :: StatementInitializer
upConditionalCommentsP = mkInit "update \"conditional\" set \"comments\" = ? where \"id\" = ?;"

-- Freshman Project:

upFreshmanProjectDescriptionP :: StatementInitializer
upFreshmanProjectDescriptionP = mkInit "update \"freshman_project\" set \"description\" = ? where \"id\" = ?;"

upFreshmanProjectDateP :: StatementInitializer
upFreshmanProjectDateP = mkInit "update \"freshman_project\" set \"project_date\" = ? where \"id\" = ?;"

-- Packet:

upPacketDueDateP :: StatementInitializer
upPacketDueDateP = mkInit "update \"packet\" set \"due_date\" = ? where \"id\" = ?;"

upPacketPercentReq :: StatementInitializer
upPacketPercentReq = mkInit "update \"packet\" set \"percent_req\" = ? where \"id\" = ?;"

-- Queue:

upQueueEnteredP :: StatementInitializer
upQueueEnteredP = mkInit "update \"queue\" set \"entered\" = ? where \"id\" = ?;"

upQueueExitedP :: StatementInitializer
upQueueExitedP = mkInit "update \"queue\" set \"exited\" = ? where \"id\" = ?;"

-- Application:

upApplicationCreatedP :: StatementInitializer
upApplicationCreatedP = mkInit "update \"application\" set \"created\" = ? where \"id\" = ?;"

upApplicationStatusP :: StatementInitializer
upApplicationStatusP = mkInit "update \"application\" set \"status\" = ? where \"id\" = ?;"

-- Metric:

upMetricNameP :: StatementInitializer
upMetricNameP = mkInit "update \"metric\" set \"name\" = ? where \"id\" = ?;"

upMetricActiveP :: StatementInitializer
upMetricActiveP = mkInit "update \"metric\" set \"active\" = ? where \"id\" = ?;"

-- Reviewer:

upReviewerReviewStartP :: StatementInitializer
upReviewerReviewStartP = mkInit "update \"reviewer\" set \"review_start\" = ? where \"id\" = ?;"

upReviewerReviewSubmitP :: StatementInitializer
upReviewerReviewSubmitP = mkInit "update \"reviewer\" set \"review_submit\" = ? where \"id\" = ?;"

-- Interviewer:

upInterviewerDateP :: StatementInitializer
upInterviewerDateP = mkInit "update \"interviewer\" set \"interview_date\" = ? where \"id\" = ?;"

-- Question:

upQuestionActiveP :: StatementInitializer
upQuestionActiveP = mkInit "update \"question\" set \"active\" = ? where \"id\" = ?;"

upQuestionQueryP :: StatementInitializer
upQuestionQueryP = mkInit "update \"question\" set \"query\" = ? where \"id\" = ?;"

-- Housing Eval:

upHousingEvalDateP :: StatementInitializer
upHousingEvalDateP = mkInit "update \"housing_eval\" set \"eval_date\" = ? where \"id\" = ?;"

-- Term:

upTermStartDateP :: StatementInitializer
upTermStartDateP = mkInit "update \"term\" set \"start_date\" = ? where \"id\" = ?;"

upTermEndDateP :: StatementInitializer
upTermEndDateP = mkInit "update \"term\" set \"end_date\" = ? where \"id\" = ?;"

-- Statement:

upStatementSgRecordP :: StatementInitializer
upStatementSgRecordP = mkInit "update \"statement\" set \"sg_record\" = ? where \"id\" = ?;"

upStatementSideEffectsP :: StatementInitializer
upStatementSideEffectsP = mkInit "update \"statement\" set \"side_effects\" = ? where \"id\" = ?;"

-- Context Mutators:

-- Event Attendee:

upEventAttendeeHostP :: StatementInitializer
upEventAttendeeHostP = mkInit "update \"event_attendee\" set \"host\" = ? where \"member_id\" = ? and \"event_id\" = ?;"

-- Project Participant:

upProjectParticipantDescriptionP :: StatementInitializer
upProjectParticipantDescriptionP = mkInit "update \"project_participant\" set \"description\" = ? where \"member_id\" = ? and \"project_id\" = ?;"

-- Freshman Project Participant:

upFreshmanProjectParticipantEboardP :: StatementInitializer
upFreshmanProjectParticipantEboardP = mkInit "update \"freshman_project_participant\" set \"eboard\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

upFreshmanProjectParticipantResultP :: StatementInitializer
upFreshmanProjectParticipantResultP = mkInit "update \"freshman_project_participant\" set \"result\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

upFreshmanProjectParticipantCommentsP :: StatementInitializer
upFreshmanProjectParticipantCommentsP = mkInit "update \"freshman_project_participant\" set \"comments\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

-- Signature:

upSignatureRequiredP :: StatementInitializer
upSignatureRequiredP = mkInit "update \"signature\" set \"required\" = ? where \"member_id\" = ? and \"packet_id\" = ?;"

upSignatureSignedP :: StatementInitializer
upSignatureSignedP = mkInit "update \"signature\" set \"signed\" = ? where \"member_id\" = ? and \"packet_id\" = ?;"

-- Reviewer Metric:

upReviewerMetricScoreP :: StatementInitializer
upReviewerMetricScoreP = mkInit "update \"reviewer_metric\" set \"score\" = ? where \"metric_id\" = ? and \"reviewer_id\" = ?;"

-- Interviewer Metric:

upInterviewerMetricScoreP :: StatementInitializer
upInterviewerMetricScoreP = mkInit "update \"interviewer_metric\" set \"score\" = ? where \"metric_id\" = ? and \"interviewer_id\" = ?;"

-- Answer:

upAnswerResponseP :: StatementInitializer
upAnswerResponseP = mkInit "update \"answer\" set \"response\" = ? where \"application_id\" = ? and \"question_id\" = ?;"

-- Housing Evaluator:

upHousingEvaluatorScoreP :: StatementInitializer
upHousingEvaluatorScoreP = mkInit "update \"housing_evaluator\" set \"score\" = ? where \"housing_eval_id\" = ? and \"member_id\" = ?;"

upHousingEvaluatorVotedP :: StatementInitializer
upHousingEvaluatorVotedP = mkInit "update \"housing_evaluator\" set \"voted\" = ? where \"housing_eval_id\" = ? and \"member_id\" = ?;"

-- Dues:

upDuesStatusP :: StatementInitializer
upDuesStatusP = mkInit "update \"dues\" set \"status\" = ? where \"term_id\" = ? and \"member_id\" = ?;"
