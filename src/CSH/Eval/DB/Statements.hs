{-|
Module      : CSH.Eval.DB.Statements
Description : Prepared statement definitions and associated data structures.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.DB.Statements contains the definition of all prepared statements and
associated data structures, namely the 'StatementGroup'.
-}

{-# OPTIONS_HADDOCK ignore-exports #-}

module CSH.Eval.DB.Statements (

    -- * StatementGroup
    StatementGroup(..)
   ,createStatementGroup
   ,cleanupStatementGroup

) where


import Database.HDBC

import Database.HDBC.PostgreSQL

-- | A 'StatementGroup' holds a reference to a pre-compiled SQL statement that
--   the PostgreSQL server associates with a specific connection. For this
--   reason, a 'StatementGroup' also appears in each
--   'CSH.Eval.DB.ContextPool'.
data StatementGroup = StatementGroup {
    getMemberIDS                                     :: Statement
   ,getMemberUUIDS                                   :: Statement
   ,getMemberUsernameS                               :: Statement
   ,getMembersCommonnameS                            :: Statement
   ,getMembersOnfloorS                               :: Statement
   ,getEventIDS                                      :: Statement
   ,getEventsTitleS                                  :: Statement
   ,getProjectIDS                                    :: Statement
   ,getProjectsTitleS                                :: Statement
   ,getProjectsStatusS                               :: Statement
   ,getEvaluationIDS                                 :: Statement
   ,getEvaluationsMemberIDS                          :: Statement
   ,getEvaluationsStatusS                            :: Statement
   ,getConditionalIDS                                :: Statement
   ,getConditionalEvaluationIDS                      :: Statement
   ,getFreshmanProjectIDS                            :: Statement
   ,getPacketIDS                                     :: Statement
   ,getPacketsMemberIDS                              :: Statement
   ,getQueueIDS                                      :: Statement
   ,getQueuesMemberIDS                               :: Statement
   ,getApplicationIDS                                :: Statement
   ,getApplicationsMemberIDS                         :: Statement
   ,getApplicationsStatusS                           :: Statement
   ,getApplicationsMemberIDStatusS                   :: Statement
   ,getMetricIDS                                     :: Statement
   ,getMetricsActiveS                                :: Statement
   ,getReviewerIDS                                   :: Statement
   ,getReviewersMemberIDS                            :: Statement
   ,getReviewersApplicationIDS                       :: Statement
   ,getReviewerMemberIDApplicationIDS                :: Statement
   ,getInterviewerIDS                                :: Statement
   ,getInterviewersMemberIDS                         :: Statement
   ,getInterviewersApplicationIDS                    :: Statement
   ,getInterviewerMemberIDApplicationIDS             :: Statement
   ,getQuestionIDS                                   :: Statement
   ,getQuestionsActiveS                              :: Statement
   ,getHousingEvalIDS                                :: Statement
   ,getTermIDS                                       :: Statement
   ,getStatementIDS                                  :: Statement
   ,getStatementSgRecordS                            :: Statement
   ,getStatementsSideEffectsS                        :: Statement
   ,getEboardsMemberIDS                              :: Statement
   ,getCurrentEboardsS                               :: Statement
   ,getRoomsMemberIDS                                :: Statement
   ,getRoomsRoomNumberS                              :: Statement
   ,getCurrentRoomsS                                 :: Statement
   ,getMembershipsMemberIDS                          :: Statement
   ,getCurrentMembershipsS                           :: Statement
   ,getEventAttendeesEventIDS                        :: Statement
   ,getEventAttendeeMemberIDS                        :: Statement
   ,getProjectParticipantsMemberIDS                  :: Statement
   ,getProjectParticipantsProjectIDS                 :: Statement
   ,getFreshmanProjectParticipantsFreshmanProjectIDS :: Statement
   ,getFreshmanProjectParticipantsEvaluationIDS      :: Statement
   ,getFreshmanProjectParticipantsEboardS            :: Statement
   ,getSignaturesMemberIDS                           :: Statement
   ,getSignaturesPacketIDS                           :: Statement
   ,getSignaturesRequiredS                           :: Statement
   ,getSignaturesSignedS                             :: Statement
   ,getReviewerMetricsMetricIDS                      :: Statement
   ,getReviewerMetricsReviewerIDS                    :: Statement
   ,getInterviewerMetricsMetricIDS                   :: Statement
   ,getInterviewerMetricsInterviewerIDS              :: Statement
   ,getAnswersApplicationIDS                         :: Statement
   ,getAnswersQuestionIDS                            :: Statement
   ,getHousingEvaluatorsHousingEvalIDS               :: Statement
   ,getHousingEvaluatorsMemberIDS                    :: Statement
   ,getDuesTermIDS                                   :: Statement
   ,getDuesMemberIDS                                 :: Statement
   ,getStatementExecStatementIDS                     :: Statement
   ,getStatementExecMemberIDS                        :: Statement
   ,mkIntroMemberS                                   :: Statement
   ,mkExtantMemberS                                  :: Statement
   ,mkEventS                                         :: Statement
   ,mkProjectS                                       :: Statement
   ,mkEvaluationS                                    :: Statement
   ,mkConditionalS                                   :: Statement
   ,mkFreshmanProjectS                               :: Statement
   ,mkPacketS                                        :: Statement
   ,mkQueueS                                         :: Statement
   ,mkApplicationS                                   :: Statement
   ,mkMetricS                                        :: Statement
   ,mkReviewerS                                      :: Statement
   ,mkInterviewerS                                   :: Statement
   ,mkQuestionS                                      :: Statement
   ,mkHousingEvalS                                   :: Statement
   ,mkTermS                                          :: Statement
   ,mkStatementS                                     :: Statement
   ,grEboardS                                        :: Statement
   ,grRoomS                                          :: Statement
   ,grMembershipS                                    :: Statement
   ,grEventAttendeeS                                 :: Statement
   ,grProjectParticipantS                            :: Statement
   ,grFreshmanProjectParticipantS                    :: Statement
   ,grSignatureS                                     :: Statement
   ,grReviewerMetricS                                :: Statement
   ,grInterviewerMetricS                             :: Statement
   ,grAnswerS                                        :: Statement
   ,grHousingEvaluatorS                              :: Statement
   ,grDuesS                                          :: Statement
   ,grStatementExecS                                 :: Statement
   ,upMemberCommonNameS                              :: Statement
   ,upMemberPasswordS                                :: Statement
   ,upMemberHousingPointsS                           :: Statement
   ,upMemberOnfloorStatusS                           :: Statement
   ,upEventTitleS                                    :: Statement
   ,upEventHeldS                                     :: Statement
   ,upEventCategoryS                                 :: Statement
   ,upEventCommitteeS                                :: Statement
   ,upEventDescriptionS                              :: Statement
   ,upProjectTitleS                                  :: Statement
   ,upProjectDescriptionS                            :: Statement
   ,upProjectSubmittedS                              :: Statement
   ,upProjectPassedS                                 :: Statement
   ,upProjectCommitteeS                              :: Statement
   ,upProjectTypeS                                   :: Statement
   ,upProjectCommentsS                               :: Statement
   ,upProjectStatusS                                 :: Statement
   ,upEvaluationCommentsS                            :: Statement
   ,upEvaluationDeadlineS                            :: Statement
   ,upEvaluationAvailableS                           :: Statement
   ,upEvaluationStatusS                              :: Statement
   ,upEvaluationTypeS                                :: Statement
   ,upConditionalDeadlineS                           :: Statement
   ,upConditionalDescriptionS                        :: Statement
   ,upConditionalCommentsS                           :: Statement
   ,upFreshmanProjectDescriptionS                    :: Statement
   ,upFreshmanProjectDateS                           :: Statement
   ,upPacketDueDateS                                 :: Statement
   ,upPacketPercentReqS                              :: Statement
   ,upQueueEnteredS                                  :: Statement
   ,upQueueExitedS                                   :: Statement
   ,upApplicationCreatedS                            :: Statement
   ,upApplicationStatusS                             :: Statement
   ,upMetricNameS                                    :: Statement
   ,upMetricActiveS                                  :: Statement
   ,upReviewerReviewStartS                           :: Statement
   ,upReviewerReviewSubmitS                          :: Statement
   ,upInterviewerDateS                               :: Statement
   ,upQuestionActiveS                                :: Statement
   ,upQuestionQueryS                                 :: Statement
   ,upHousingEvalDateS                               :: Statement
   ,upTermStartDateS                                 :: Statement
   ,upTermEndDateS                                   :: Statement
   ,upStatementSgRecordS                             :: Statement
   ,upStatementSideEffectsS                          :: Statement
   ,upEventAttendeeHostS                             :: Statement
   ,upProjectParticipantDescriptionS                 :: Statement
   ,upFreshmanProjectParticipantEboardS              :: Statement
   ,upFreshmanProjectParticipantResultS              :: Statement
   ,upFreshmanProjectParticipantCommentsS            :: Statement
   ,upSignatureRequiredS                             :: Statement
   ,upSignatureSignedS                               :: Statement
   ,upReviewerMetricScoreS                           :: Statement
   ,upInterviewerMetricScoreS                        :: Statement
   ,upAnswerResponseS                                :: Statement
   ,upHousingEvaluatorScoreS                         :: Statement
   ,upHousingEvaluatorVotedS                         :: Statement
   ,upDuesStatusS                                    :: Statement
    }

-- | Creates a 'StatementGroup' whose 'Database.HDBC.Statement's are associated
--   with the provided 'Database.HDBC.PostgreSQL.Connection'.
createStatementGroup :: Connection -> IO StatementGroup
createStatementGroup c = do
    getMemberIDS'                                     <- getMemberIDP c
    getMemberUUIDS'                                   <- getMemberUUIDP c
    getMemberUsernameS'                               <- getMemberUsernameP c
    getMembersCommonnameS'                            <- getMembersCommonnameP c
    getMembersOnfloorS'                               <- getMembersOnfloorP c
    getEventIDS'                                      <- getEventIDP c
    getEventsTitleS'                                  <- getEventsTitleP c
    getProjectIDS'                                    <- getProjectIDP c
    getProjectsTitleS'                                <- getProjectsTitleP c
    getProjectsStatusS'                               <- getProjectsStatusP c
    getEvaluationIDS'                                 <- getEvaluationIDP c
    getEvaluationsMemberIDS'                          <- getEvaluationsMemberIDP c
    getEvaluationsStatusS'                            <- getEvaluationsStatusP c
    getConditionalIDS'                                <- getConditionalIDP c
    getConditionalEvaluationIDS'                      <- getConditionalEvaluationIDP c
    getFreshmanProjectIDS'                            <- getFreshmanProjectIDP c
    getPacketIDS'                                     <- getPacketIDP c
    getPacketsMemberIDS'                              <- getPacketsMemberIDP c
    getQueueIDS'                                      <- getQueueIDP c
    getQueuesMemberIDS'                               <- getQueuesMemberIDP c
    getApplicationIDS'                                <- getApplicationIDP c
    getApplicationsMemberIDS'                         <- getApplicationsMemberIDP c
    getApplicationsStatusS'                           <- getApplicationsStatusP c
    getApplicationsMemberIDStatusS'                   <- getApplicationsMemberIDStatusP c
    getMetricIDS'                                     <- getMetricIDP c
    getMetricsActiveS'                                <- getMetricsActiveP c
    getReviewerIDS'                                   <- getReviewerIDP c
    getReviewersMemberIDS'                            <- getReviewersMemberIDP c
    getReviewersApplicationIDS'                       <- getReviewersApplicationIDP c
    getReviewerMemberIDApplicationIDS'                <- getReviewerMemberIDApplicationIDP c
    getInterviewerIDS'                                <- getInterviewerIDP c
    getInterviewersMemberIDS'                         <- getInterviewersMemberIDP c
    getInterviewersApplicationIDS'                    <- getInterviewersApplicationIDP c
    getInterviewerMemberIDApplicationIDS'             <- getInterviewerMemberIDApplicationIDP c
    getQuestionIDS'                                   <- getQuestionIDP c
    getQuestionsActiveS'                              <- getQuestionsActiveP c
    getHousingEvalIDS'                                <- getHousingEvalIDP c
    getTermIDS'                                       <- getTermIDP c
    getStatementIDS'                                  <- getStatementIDP c
    getStatementSgRecordS'                            <- getStatementSgRecordP c
    getStatementsSideEffectsS'                        <- getStatementsSideEffectsP c
    getEboardsMemberIDS'                              <- getEboardsMemberIDP c
    getCurrentEboardsS'                               <- getCurrentEboardsP c
    getRoomsMemberIDS'                                <- getRoomsMemberIDP c
    getRoomsRoomNumberS'                              <- getRoomsRoomNumberP c
    getCurrentRoomsS'                                 <- getCurrentRoomsP c
    getMembershipsMemberIDS'                          <- getMembershipsMemberIDP c
    getCurrentMembershipsS'                           <- getCurrentMembershipsP c
    getEventAttendeesEventIDS'                        <- getEventAttendeesEventIDP c
    getEventAttendeeMemberIDS'                        <- getEventAttendeeMemberIDP c
    getProjectParticipantsMemberIDS'                  <- getProjectParticipantsMemberIDP c
    getProjectParticipantsProjectIDS'                 <- getProjectParticipantsProjectIDP c
    getFreshmanProjectParticipantsFreshmanProjectIDS' <- getFreshmanProjectParticipantsFreshmanProjectIDP c
    getFreshmanProjectParticipantsEvaluationIDS'      <- getFreshmanProjectParticipantsEvaluationIDP c
    getFreshmanProjectParticipantsEboardS'            <- getFreshmanProjectParticipantsEboardP c
    getSignaturesMemberIDS'                           <- getSignaturesMemberIDP c
    getSignaturesPacketIDS'                           <- getSignaturesPacketIDP c
    getSignaturesRequiredS'                           <- getSignaturesRequiredP c
    getSignaturesSignedS'                             <- getSignaturesSignedP c
    getReviewerMetricsMetricIDS'                      <- getReviewerMetricsMetricIDP c
    getReviewerMetricsReviewerIDS'                    <- getReviewerMetricsReviewerIDP c
    getInterviewerMetricsMetricIDS'                   <- getInterviewerMetricsMetricIDP c
    getInterviewerMetricsInterviewerIDS'              <- getInterviewerMetricsInterviewerIDP c
    getAnswersApplicationIDS'                         <- getAnswersApplicationIDP c
    getAnswersQuestionIDS'                            <- getAnswersQuestionIDP c
    getHousingEvaluatorsHousingEvalIDS'               <- getHousingEvaluatorsHousingEvalIDP c
    getHousingEvaluatorsMemberIDS'                    <- getHousingEvaluatorsMemberIDP c
    getDuesTermIDS'                                   <- getDuesTermIDP c
    getDuesMemberIDS'                                 <- getDuesMemberIDP c
    getStatementExecStatementIDS'                     <- getStatementExecStatementIDP c
    getStatementExecMemberIDS'                        <- getStatementExecMemberIDP c
    mkIntroMemberS'                                   <- mkIntroMemberP c
    mkExtantMemberS'                                  <- mkExtantMemberP c
    mkEventS'                                         <- mkEventP c
    mkProjectS'                                       <- mkProjectP c
    mkEvaluationS'                                    <- mkEvaluationP c
    mkConditionalS'                                   <- mkConditionalP c
    mkFreshmanProjectS'                               <- mkFreshmanProjectP c
    mkPacketS'                                        <- mkPacketP c
    mkQueueS'                                         <- mkQueueP c
    mkApplicationS'                                   <- mkApplicationP c
    mkMetricS'                                        <- mkMetricP c
    mkReviewerS'                                      <- mkReviewerP c
    mkInterviewerS'                                   <- mkInterviewerP c
    mkQuestionS'                                      <- mkQuestionP c
    mkHousingEvalS'                                   <- mkHousingEvalP c
    mkTermS'                                          <- mkTermP c
    mkStatementS'                                     <- mkStatementP c
    grEboardS'                                        <- grEboardP c
    grRoomS'                                          <- grRoomP c
    grMembershipS'                                    <- grMembershipP c
    grEventAttendeeS'                                 <- grEventAttendeeP c
    grProjectParticipantS'                            <- grProjectParticipantP c
    grFreshmanProjectParticipantS'                    <- grFreshmanProjectParticipantP c
    grSignatureS'                                     <- grSignatureP c
    grReviewerMetricS'                                <- grReviewerMetricP c
    grInterviewerMetricS'                             <- grInterviewerMetricP c
    grAnswerS'                                        <- grAnswerP c
    grHousingEvaluatorS'                              <- grHousingEvaluatorP c
    grDuesS'                                          <- grDuesP c
    grStatementExecS'                                 <- grStatementExecP c
    upMemberCommonNameS'                              <- upMemberCommonNameP c
    upMemberPasswordS'                                <- upMemberPasswordP c
    upMemberHousingPointsS'                           <- upMemberHousingPointsP c
    upMemberOnfloorStatusS'                           <- upMemberOnfloorStatusP c
    upEventTitleS'                                    <- upEventTitleP c
    upEventHeldS'                                     <- upEventHeldP c
    upEventCategoryS'                                 <- upEventCategoryP c
    upEventCommitteeS'                                <- upEventCommitteeP c
    upEventDescriptionS'                              <- upEventDescriptionP c
    upProjectTitleS'                                  <- upProjectTitleP c
    upProjectDescriptionS'                            <- upProjectDescriptionP c
    upProjectSubmittedS'                              <- upProjectSubmittedP c
    upProjectPassedS'                                 <- upProjectPassedP c
    upProjectCommitteeS'                              <- upProjectCommitteeP c
    upProjectTypeS'                                   <- upProjectTypeP c
    upProjectCommentsS'                               <- upProjectCommentsP c
    upProjectStatusS'                                 <- upProjectStatusP c
    upEvaluationCommentsS'                            <- upEvaluationCommentsP c
    upEvaluationDeadlineS'                            <- upEvaluationDeadlineP c
    upEvaluationAvailableS'                           <- upEvaluationAvailableP c
    upEvaluationStatusS'                              <- upEvaluationStatusP c
    upEvaluationTypeS'                                <- upEvaluationTypeP c
    upConditionalDeadlineS'                           <- upConditionalDeadlineP c
    upConditionalDescriptionS'                        <- upConditionalDescriptionP c
    upConditionalCommentsS'                           <- upConditionalCommentsP c
    upFreshmanProjectDescriptionS'                    <- upFreshmanProjectDescriptionP c
    upFreshmanProjectDateS'                           <- upFreshmanProjectDateP c
    upPacketDueDateS'                                 <- upPacketDueDateP c
    upPacketPercentReqS'                              <- upPacketPercentReqP c
    upQueueEnteredS'                                  <- upQueueEnteredP c
    upQueueExitedS'                                   <- upQueueExitedP c
    upApplicationCreatedS'                            <- upApplicationCreatedP c
    upApplicationStatusS'                             <- upApplicationStatusP c
    upMetricNameS'                                    <- upMetricNameP c
    upMetricActiveS'                                  <- upMetricActiveP c
    upReviewerReviewStartS'                           <- upReviewerReviewStartP c
    upReviewerReviewSubmitS'                          <- upReviewerReviewSubmitP c
    upInterviewerDateS'                               <- upInterviewerDateP c
    upQuestionActiveS'                                <- upQuestionActiveP c
    upQuestionQueryS'                                 <- upQuestionQueryP c
    upHousingEvalDateS'                               <- upHousingEvalDateP c
    upTermStartDateS'                                 <- upTermStartDateP c
    upTermEndDateS'                                   <- upTermEndDateP c
    upStatementSgRecordS'                             <- upStatementSgRecordP c
    upStatementSideEffectsS'                          <- upStatementSideEffectsP c
    upEventAttendeeHostS'                             <- upEventAttendeeHostP c
    upProjectParticipantDescriptionS'                 <- upProjectParticipantDescriptionP c
    upFreshmanProjectParticipantEboardS'              <- upFreshmanProjectParticipantEboardP c
    upFreshmanProjectParticipantResultS'              <- upFreshmanProjectParticipantResultP c
    upFreshmanProjectParticipantCommentsS'            <- upFreshmanProjectParticipantCommentsP c
    upSignatureRequiredS'                             <- upSignatureRequiredP c
    upSignatureSignedS'                               <- upSignatureSignedP c
    upReviewerMetricScoreS'                           <- upReviewerMetricScoreP c
    upInterviewerMetricScoreS'                        <- upInterviewerMetricScoreP c
    upAnswerResponseS'                                <- upAnswerResponseP c
    upHousingEvaluatorScoreS'                         <- upHousingEvaluatorScoreP c
    upHousingEvaluatorVotedS'                         <- upHousingEvaluatorVotedP c
    upDuesStatusS'                                    <- upDuesStatusP c
    return $ StatementGroup
                getMemberIDS'
                getMemberUUIDS'
                getMemberUsernameS'
                getMembersCommonnameS'
                getMembersOnfloorS'
                getEventIDS'
                getEventsTitleS'
                getProjectIDS'
                getProjectsTitleS'
                getProjectsStatusS'
                getEvaluationIDS'
                getEvaluationsMemberIDS'
                getEvaluationsStatusS'
                getConditionalIDS'
                getConditionalEvaluationIDS'
                getFreshmanProjectIDS'
                getPacketIDS'
                getPacketsMemberIDS'
                getQueueIDS'
                getQueuesMemberIDS'
                getApplicationIDS'
                getApplicationsMemberIDS'
                getApplicationsStatusS'
                getApplicationsMemberIDStatusS'
                getMetricIDS'
                getMetricsActiveS'
                getReviewerIDS'
                getReviewersMemberIDS'
                getReviewersApplicationIDS'
                getReviewerMemberIDApplicationIDS'
                getInterviewerIDS'
                getInterviewersMemberIDS'
                getInterviewersApplicationIDS'
                getInterviewerMemberIDApplicationIDS'
                getQuestionIDS'
                getQuestionsActiveS'
                getHousingEvalIDS'
                getTermIDS'
                getStatementIDS'
                getStatementSgRecordS'
                getStatementsSideEffectsS'
                getEboardsMemberIDS'
                getCurrentEboardsS'
                getRoomsMemberIDS'
                getRoomsRoomNumberS'
                getCurrentRoomsS'
                getMembershipsMemberIDS'
                getCurrentMembershipsS'
                getEventAttendeesEventIDS'
                getEventAttendeeMemberIDS'
                getProjectParticipantsMemberIDS'
                getProjectParticipantsProjectIDS'
                getFreshmanProjectParticipantsFreshmanProjectIDS'
                getFreshmanProjectParticipantsEvaluationIDS'
                getFreshmanProjectParticipantsEboardS'
                getSignaturesMemberIDS'
                getSignaturesPacketIDS'
                getSignaturesRequiredS'
                getSignaturesSignedS'
                getReviewerMetricsMetricIDS'
                getReviewerMetricsReviewerIDS'
                getInterviewerMetricsMetricIDS'
                getInterviewerMetricsInterviewerIDS'
                getAnswersApplicationIDS'
                getAnswersQuestionIDS'
                getHousingEvaluatorsHousingEvalIDS'
                getHousingEvaluatorsMemberIDS'
                getDuesTermIDS'
                getDuesMemberIDS'
                getStatementExecStatementIDS'
                getStatementExecMemberIDS'
                mkIntroMemberS'
                mkExtantMemberS'
                mkEventS'
                mkProjectS'
                mkEvaluationS'
                mkConditionalS'
                mkFreshmanProjectS'
                mkPacketS'
                mkQueueS'
                mkApplicationS'
                mkMetricS'
                mkReviewerS'
                mkInterviewerS'
                mkQuestionS'
                mkHousingEvalS'
                mkTermS'
                mkStatementS'
                grEboardS'
                grRoomS'
                grMembershipS'
                grEventAttendeeS'
                grProjectParticipantS'
                grFreshmanProjectParticipantS'
                grSignatureS'
                grReviewerMetricS'
                grInterviewerMetricS'
                grAnswerS'
                grHousingEvaluatorS'
                grDuesS'
                grStatementExecS'
                upMemberCommonNameS'
                upMemberPasswordS'
                upMemberHousingPointsS'
                upMemberOnfloorStatusS'
                upEventTitleS'
                upEventHeldS'
                upEventCategoryS'
                upEventCommitteeS'
                upEventDescriptionS'
                upProjectTitleS'
                upProjectDescriptionS'
                upProjectSubmittedS'
                upProjectPassedS'
                upProjectCommitteeS'
                upProjectTypeS'
                upProjectCommentsS'
                upProjectStatusS'
                upEvaluationCommentsS'
                upEvaluationDeadlineS'
                upEvaluationAvailableS'
                upEvaluationStatusS'
                upEvaluationTypeS'
                upConditionalDeadlineS'
                upConditionalDescriptionS'
                upConditionalCommentsS'
                upFreshmanProjectDescriptionS'
                upFreshmanProjectDateS'
                upPacketDueDateS'
                upPacketPercentReqS'
                upQueueEnteredS'
                upQueueExitedS'
                upApplicationCreatedS'
                upApplicationStatusS'
                upMetricNameS'
                upMetricActiveS'
                upReviewerReviewStartS'
                upReviewerReviewSubmitS'
                upInterviewerDateS'
                upQuestionActiveS'
                upQuestionQueryS'
                upHousingEvalDateS'
                upTermStartDateS'
                upTermEndDateS'
                upStatementSgRecordS'
                upStatementSideEffectsS'
                upEventAttendeeHostS'
                upProjectParticipantDescriptionS'
                upFreshmanProjectParticipantEboardS'
                upFreshmanProjectParticipantResultS'
                upFreshmanProjectParticipantCommentsS'
                upSignatureRequiredS'
                upSignatureSignedS'
                upReviewerMetricScoreS'
                upInterviewerMetricScoreS'
                upAnswerResponseS'
                upHousingEvaluatorScoreS'
                upHousingEvaluatorVotedS'
                upDuesStatusS'

-- | Finalize all of the 'Statements' in a 'StatementGroup'. This involves
--   aborting any currently executing statements (automatically rolling-back
--   the transaction), flushing any unconsumed results, and finally
--   de-allocating the statement PostgreSQL-side.
cleanupStatementGroup :: StatementGroup -> IO ()
cleanupStatementGroup sg = mapM_ finish [
     getMemberIDS sg
    ,getMemberUUIDS sg
    ,getMemberUsernameS sg
    ,getMembersCommonnameS sg
    ,getMembersOnfloorS sg
    ,getEventIDS sg
    ,getEventsTitleS sg
    ,getProjectIDS sg
    ,getProjectsTitleS sg
    ,getProjectsStatusS sg
    ,getEvaluationIDS sg
    ,getEvaluationsMemberIDS sg
    ,getEvaluationsStatusS sg
    ,getConditionalIDS sg
    ,getConditionalEvaluationIDS sg
    ,getFreshmanProjectIDS sg
    ,getPacketIDS sg
    ,getPacketsMemberIDS sg
    ,getQueueIDS sg
    ,getQueuesMemberIDS sg
    ,getApplicationIDS sg
    ,getApplicationsMemberIDS sg
    ,getApplicationsStatusS sg
    ,getApplicationsMemberIDStatusS sg
    ,getMetricIDS sg
    ,getMetricsActiveS sg
    ,getReviewerIDS sg
    ,getReviewersMemberIDS sg
    ,getReviewersApplicationIDS sg
    ,getReviewerMemberIDApplicationIDS sg
    ,getInterviewerIDS sg
    ,getInterviewersMemberIDS sg
    ,getInterviewersApplicationIDS sg
    ,getInterviewerMemberIDApplicationIDS sg
    ,getQuestionIDS sg
    ,getQuestionsActiveS sg
    ,getHousingEvalIDS sg
    ,getTermIDS sg
    ,getStatementIDS sg
    ,getStatementSgRecordS sg
    ,getStatementsSideEffectsS sg
    ,getEboardsMemberIDS sg
    ,getCurrentEboardsS sg
    ,getRoomsMemberIDS sg
    ,getRoomsRoomNumberS sg
    ,getCurrentRoomsS sg
    ,getMembershipsMemberIDS sg
    ,getCurrentMembershipsS sg
    ,getEventAttendeesEventIDS sg
    ,getEventAttendeeMemberIDS sg
    ,getProjectParticipantsMemberIDS sg
    ,getProjectParticipantsProjectIDS sg
    ,getFreshmanProjectParticipantsFreshmanProjectIDS sg
    ,getFreshmanProjectParticipantsEvaluationIDS sg
    ,getFreshmanProjectParticipantsEboardS sg
    ,getSignaturesMemberIDS sg
    ,getSignaturesPacketIDS sg
    ,getSignaturesRequiredS sg
    ,getSignaturesSignedS sg
    ,getReviewerMetricsMetricIDS sg
    ,getReviewerMetricsReviewerIDS sg
    ,getInterviewerMetricsMetricIDS sg
    ,getInterviewerMetricsInterviewerIDS sg
    ,getAnswersApplicationIDS sg
    ,getAnswersQuestionIDS sg
    ,getHousingEvaluatorsHousingEvalIDS sg
    ,getHousingEvaluatorsMemberIDS sg
    ,getDuesTermIDS sg
    ,getDuesMemberIDS sg
    ,getStatementExecStatementIDS sg
    ,getStatementExecMemberIDS sg
    ,mkIntroMemberS sg
    ,mkExtantMemberS sg
    ,mkEventS sg
    ,mkProjectS sg
    ,mkEvaluationS sg
    ,mkConditionalS sg
    ,mkFreshmanProjectS sg
    ,mkPacketS sg
    ,mkQueueS sg
    ,mkApplicationS sg
    ,mkMetricS sg
    ,mkReviewerS sg
    ,mkInterviewerS sg
    ,mkQuestionS sg
    ,mkHousingEvalS sg
    ,mkTermS sg
    ,mkStatementS sg
    ,grEboardS sg
    ,grRoomS sg
    ,grMembershipS sg
    ,grEventAttendeeS sg
    ,grProjectParticipantS sg
    ,grFreshmanProjectParticipantS sg
    ,grSignatureS sg
    ,grReviewerMetricS sg
    ,grInterviewerMetricS sg
    ,grAnswerS sg
    ,grHousingEvaluatorS sg
    ,grDuesS sg
    ,grStatementExecS sg
    ,upMemberCommonNameS sg
    ,upMemberPasswordS sg
    ,upMemberHousingPointsS sg
    ,upMemberOnfloorStatusS sg
    ,upEventTitleS sg
    ,upEventHeldS sg
    ,upEventCategoryS sg
    ,upEventCommitteeS sg
    ,upEventDescriptionS sg
    ,upProjectTitleS sg
    ,upProjectDescriptionS sg
    ,upProjectSubmittedS sg
    ,upProjectPassedS sg
    ,upProjectCommitteeS sg
    ,upProjectTypeS sg
    ,upProjectCommentsS sg
    ,upProjectStatusS sg
    ,upEvaluationCommentsS sg
    ,upEvaluationDeadlineS sg
    ,upEvaluationAvailableS sg
    ,upEvaluationStatusS sg
    ,upEvaluationTypeS sg
    ,upConditionalDeadlineS sg
    ,upConditionalDescriptionS sg
    ,upConditionalCommentsS sg
    ,upFreshmanProjectDescriptionS sg
    ,upFreshmanProjectDateS sg
    ,upPacketDueDateS sg
    ,upPacketPercentReqS sg
    ,upQueueEnteredS sg
    ,upQueueExitedS sg
    ,upApplicationCreatedS sg
    ,upApplicationStatusS sg
    ,upMetricNameS sg
    ,upMetricActiveS sg
    ,upReviewerReviewStartS sg
    ,upReviewerReviewSubmitS sg
    ,upInterviewerDateS sg
    ,upQuestionActiveS sg
    ,upQuestionQueryS sg
    ,upHousingEvalDateS sg
    ,upTermStartDateS sg
    ,upTermEndDateS sg
    ,upStatementSgRecordS sg
    ,upStatementSideEffectsS sg
    ,upEventAttendeeHostS sg
    ,upProjectParticipantDescriptionS sg
    ,upFreshmanProjectParticipantEboardS sg
    ,upFreshmanProjectParticipantResultS sg
    ,upFreshmanProjectParticipantCommentsS sg
    ,upSignatureRequiredS sg
    ,upSignatureSignedS sg
    ,upReviewerMetricScoreS sg
    ,upInterviewerMetricScoreS sg
    ,upAnswerResponseS sg
    ,upHousingEvaluatorScoreS sg
    ,upHousingEvaluatorVotedS sg
    ,upDuesStatusS sg
    ]

-- * Prepared Statement SQL Definitions

-- | The type of a statemnt initializer, corresponding to a single
--   'StatementGroup' constructor.
type StatementInitializer = Connection -> IO Statement

-- | Build a 'StatementInitializer' from a SQL string.
mkInit :: String -> StatementInitializer
mkInit = flip prepare

-- ** Object Lookup

-- The following prepared statements look up specific objects in the model,
-- typically via a unique identifier or sufficient combination thereof.
-- Unfortunately, Haskell's type system is powerless to ensure the correctness
-- of these queries, so the expected HDBC type(s) and semantics of the positional
-- parameters are provided.

-- *** Member

-- | ('Database.HDBC.SqlInteger') Fetch a member by ID.
getMemberIDP :: StatementInitializer
getMemberIDP = mkInit "select * from \"member\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch a member by UUID.
getMemberUUIDP :: StatementInitializer
getMemberUUIDP = mkInit "select * from \"member\" where \"uuid\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch a member by username.
getMemberUsernameP :: StatementInitializer
getMemberUsernameP = mkInit "select * from \"member\" where \"username\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch the member(s) with the given commonname.
getMembersCommonnameP :: StatementInitializer
getMembersCommonnameP = mkInit "select * from \"member\" where \"commonname\" = ?;"

-- | ('Database.HDBC.SqlBool') Fetch all members with the given on floor status.
getMembersOnfloorP :: StatementInitializer
getMembersOnfloorP = mkInit "select * from \"members\" where \"onfloor_status\" = ?;"

-- *** Event

-- | ('Database.HDBC.SqlInteger') Fetch an event by ID.
getEventIDP :: StatementInitializer
getEventIDP = mkInit "select * from \"event\" where \"id\" = ?;"

-- | ('Database.HDC.SqlByteString') Fetch all event(s) with the given title.
getEventsTitleP :: StatementInitializer
getEventsTitleP = mkInit "select * from \"event\" where \"title\" = ?;"

-- *** Project

-- | ('Database.HDCC.SqlInteger') Fetch a project by ID.
getProjectIDP :: StatementInitializer
getProjectIDP = mkInit "select * from \"project\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch all project(s) with a given title.
getProjectsTitleP :: StatementInitializer
getProjectsTitleP = mkInit "select * from \"project\" where \"title\" = ?;"

-- | ('Database.HDBC.SqlString') Fetch all project(s) with a given status.
getProjectsStatusP :: StatementInitializer
getProjectsStatusP = mkInit "select * from \"project\" where \"status\" = ?;"

-- *** Evaluation

-- | ('Database.HDBC.SqlInteger') Fetch an evaluation by ID.
getEvaluationIDP :: StatementInitializer
getEvaluationIDP = mkInit "select * from \"evaluation\" where \"id\" = ?;"

-- | ('Databse.HDBC.SqlInteger') Fetch all evaluation(s) associated with a
--   specific member.
getEvaluationsMemberIDP :: StatementInitializer
getEvaluationsMemberIDP = mkInit "select * from \"evaluation\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlString') Fetch all evaluation(s) with a given status.
getEvaluationsStatusP :: StatementInitializer
getEvaluationsStatusP = mkInit "select * from \"evaluation\" where \"eval_type\" = ?;"

-- *** Conditional

-- | ('Database.HDBC.SqlInteger') Fetch a conditional by ID.
getConditionalIDP :: StatementInitializer
getConditionalIDP = mkInit "select * from \"conditional\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all conditional(s) associated with a
--   specific evaluation.
getConditionalEvaluationIDP :: StatementInitializer
getConditionalEvaluationIDP = mkInit "select * from \"conditional\" where \"evaluation_id\" = ?;"

-- *** Freshman Project

-- | ('Database.HDBC.SqlInteger') Fetch a freshman project by ID.
getFreshmanProjectIDP :: StatementInitializer
getFreshmanProjectIDP = mkInit "select * from \"freshman_project\" where \"id\" = ?;"

-- *** Packet

-- | ('Database.HDBC.SqlInteger') Fetch a packet by ID.
getPacketIDP :: StatementInitializer
getPacketIDP = mkInit "select * from \"packet\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all packet(s) associated with a
--   specific member.
getPacketsMemberIDP :: StatementInitializer
getPacketsMemberIDP = mkInit "select * from \"packet\" where \"member_id\" = ?;"

-- *** Queue

-- | ('Database.HDBC.SqlInteger') Fetch a queue entry by ID.
getQueueIDP :: StatementInitializer
getQueueIDP = mkInit "select * \"queue\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all queue entries associated with a
--   specific member.
getQueuesMemberIDP :: StatementInitializer
getQueuesMemberIDP = mkInit "select * from \"queueu\" where \"member_id\" = ?;"

-- *** Application

-- | ('Database.HDBC.SqlInteger') Fetch an application by ID.
getApplicationIDP :: StatementInitializer
getApplicationIDP = mkInit "select * from \"application\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all application(s) associated with a
--   specific member.
getApplicationsMemberIDP :: StatementInitializer
getApplicationsMemberIDP = mkInit "select * from \"application\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlString') Fetch all application(s) with a given status.
getApplicationsStatusP :: StatementInitializer
getApplicationsStatusP = mkInit "select * from \"application\" where \"status\" = ?;"

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlString') Fetch all
--   applications associated with a specific member and of a given status.
getApplicationsMemberIDStatusP :: StatementInitializer
getApplicationsMemberIDStatusP = mkInit "select * from \"application\" where \"member_id\" = ? and \"status\" = ?;"

-- *** Metric

-- | ('Database.HDBC.SqlInteger') Fetch a metric by ID.
getMetricIDP :: StatementInitializer
getMetricIDP = mkInit "select * from \"metric\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlString') Fetch all metric(s) with the given status.
getMetricsActiveP :: StatementInitializer
getMetricsActiveP = mkInit "select * from \"metric\" where \"active\" = ?;"

-- *** Reviewer

-- | ('Database.HDBC.SqlInteger') Fetch a reviewer by ID.
getReviewerIDP :: StatementInitializer
getReviewerIDP = mkInit "select * from \"reviewer\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all reviewer(s) associated with a
--   specific member.
getReviewersMemberIDP :: StatementInitializer
getReviewersMemberIDP = mkInit "select * from \"reviewer\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all reviewer(s) associated with a
--   specific application.
getReviewersApplicationIDP :: StatementInitializer
getReviewersApplicationIDP = mkInit "select * from \"reviewer\" where \"application_id\" = ?;"

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Fetch the
--   reviewer associated with a specific member and a specific application.
getReviewerMemberIDApplicationIDP :: StatementInitializer
getReviewerMemberIDApplicationIDP = mkInit "select * from \"reviewer\" where \"member_id\" = ? and \"application_id\" = ?;"

-- *** Interviewer

-- | ('Database.HDBC.SqlInteger') Fetch an interviewer by ID.
getInterviewerIDP :: StatementInitializer
getInterviewerIDP = mkInit "select * from \"interviewer\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all interviewer(s) associated with a
--   specific member.
getInterviewersMemberIDP :: StatementInitializer
getInterviewersMemberIDP = mkInit "select * from \"interviewer\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all interviewer(s) associated with a
--   specific application.
getInterviewersApplicationIDP :: StatementInitializer
getInterviewersApplicationIDP = mkInit "select * from \"interviewer\" where \"application_id\" = ?;"

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Fetch the
--   interviewer associated with a specific member and a specific application.
getInterviewerMemberIDApplicationIDP :: StatementInitializer
getInterviewerMemberIDApplicationIDP = mkInit "select * from \"interviewer\" where \"member_id\" = ? and \"application_id\" = ?;"

-- *** Question:

-- | ('Database.HDBC.SqlInteger') Fetch a question by ID.
getQuestionIDP :: StatementInitializer
getQuestionIDP = mkInit "select * from \"question\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlString') Fetch all question(s) with a given active
--   status.
getQuestionsActiveP :: StatementInitializer
getQuestionsActiveP = mkInit "select * from \"question\" where \"active\" = ?;"

-- *** Housing Eval

-- | ('Database.HDBC.SqlInteger') Fetch a housing eval by ID.
getHousingEvalIDP :: StatementInitializer
getHousingEvalIDP = mkInit "select * from \"housing_eval\" where \"id\" = ?;"

-- *** Term

-- | ('Database.HDBC.SqlInteger') Fetch a term by ID.
getTermIDP :: StatementInitializer
getTermIDP = mkInit "select * from \"term\" where \"id\" = ?;"

-- *** Statement

-- | ('Database.HDBC.SqlInteger') Fetch a statement by ID.
getStatementIDP :: StatementInitializer
getStatementIDP = mkInit "select * from \"statement\" where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch a statement by 'StatementGroup'
--   record name (in this file).
getStatementSgRecordP :: StatementInitializer
getStatementSgRecordP = mkInit "select * from \"statement\" where \"sg_record\" = ?;"

-- | ('Database.HDBC.SqlBool') Fetch all statements with the given side effects
--   flag.
getStatementsSideEffectsP :: StatementInitializer
getStatementsSideEffectsP = mkInit "select * from \"statement\" where \"side_effects\" = ?;"

-- ** Context Lookup

-- The following prepared statements look up specific contextss in the model,
-- typically via a unique object identifier or sufficient combination thereof.
-- Unfortunately, Haskell's type system is powerless to ensure the correctness
-- of these queries, so the expected HDBC type(s) and semantics of the positional
-- parameters are provided.

-- *** Eboard

-- | ('Database.HDBC.SqlInteger') Fetch all Eboard records associated with a
--   specific member.
getEboardsMemberIDP :: StatementInitializer
getEboardsMemberIDP = mkInit "select * from \"eboard\" where \"member_id\" = ?;"

-- | () Fetch all Eboard records for the currently serving Eboard.
getCurrentEboardsP :: StatementInitializer
getCurrentEboardsP = mkInit "select * from \"eboard\" where \"end_date\" is null;"

-- *** Room

-- | ('Database.HDBC.SqlInteger') Fetch all room(s) associated with a specific
--   member.
getRoomsMemberIDP :: StatementInitializer
getRoomsMemberIDP = mkInit "select * from \"room\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlByteString') Fetch all room records with the given room
--   number.
getRoomsRoomNumberP :: StatementInitializer
getRoomsRoomNumberP = mkInit "select * from \"room\" where \"room_number\" = ?;"

-- | () Fetch all room records representing present occupancy.
getCurrentRoomsP :: StatementInitializer
getCurrentRoomsP = mkInit "select * from \"room\" where \"end_date\" is null;"

-- *** Membership

-- | ('Database.HDBC.SqlInteger') Fetch all membership records associated with
--   a specific record.
getMembershipsMemberIDP :: StatementInitializer
getMembershipsMemberIDP = mkInit "select * from \"membership\" where \"member_id\" = ?;"

-- | () Fetch all currently enforced membership statuses.
getCurrentMembershipsP :: StatementInitializer
getCurrentMembershipsP = mkInit "select * from \"membership\" where \"end_date\" is null;"

-- *** Event Attendee

-- | ('Database.HDBC.SqlInteger') Fetch all event attendee records associated
--   with a specific event.
getEventAttendeesEventIDP :: StatementInitializer
getEventAttendeesEventIDP = mkInit "select * from \"event_attendee\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all event attendee records associated
--   with a specific member.
getEventAttendeeMemberIDP :: StatementInitializer
getEventAttendeeMemberIDP = mkInit "select * from \"event_attendee\" where \"event_id\" = ?;"

-- *** Project Participant

-- | ('Database.HDBC.SqlInteger') Fetch all project participant records
--    associated with a specific member.
getProjectParticipantsMemberIDP :: StatementInitializer
getProjectParticipantsMemberIDP = mkInit "select * from \"project_participant\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all project participant records
--   associated with a specific project.
getProjectParticipantsProjectIDP :: StatementInitializer
getProjectParticipantsProjectIDP = mkInit "select * from \"project_participant\" where \"project_id\" = ?;"

-- *** Freshman Project Participant

-- | ('Database.HDBC.SqlInteger') Fetch all freshman project participant
--   records associated with a specific freshman project.
getFreshmanProjectParticipantsFreshmanProjectIDP :: StatementInitializer
getFreshmanProjectParticipantsFreshmanProjectIDP = mkInit "select * from \"freshman_project_participant\" where \"freshman_project_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all freshman project participant
--   records associated with a specific evaluation.
getFreshmanProjectParticipantsEvaluationIDP :: StatementInitializer
getFreshmanProjectParticipantsEvaluationIDP = mkInit "select * from \"freshman_project_participant\" where \"evaluation_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all freshman project participant
--   records associated with the Eboard of a specific freshman project.
getFreshmanProjectParticipantsEboardP :: StatementInitializer
getFreshmanProjectParticipantsEboardP = mkInit "select * from \"freshman_project_participant\" where \"freshman_project_id\" = ? and \"eboard\" = \'true\';"

-- *** Signature

-- | ('Database.HDBC.SqlInteger') Fetch all signatures associated with a
--   specific member.
getSignaturesMemberIDP :: StatementInitializer
getSignaturesMemberIDP = mkInit "select * from \"signatures\" where \"member_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all signatures associated with a
--   specific packet.
getSignaturesPacketIDP :: StatementInitializer
getSignaturesPacketIDP = mkInit "select * from \"signatures\" where \"packet_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all required signatures associated with
--   a specific packet.
getSignaturesRequiredP :: StatementInitializer
getSignaturesRequiredP = mkInit "select * from \"signatures\" where \"packet_id\" = ? and \"required\" = \'true\';"

-- | ('Database.HDBC.SqlInteger') Fetch all acquired signatures associated with
--   a specific packet.
getSignaturesSignedP :: StatementInitializer
getSignaturesSignedP = mkInit "select * from \"signatures\" where \"packet_id\" = ? and \"signed\" is not null;"

-- *** Reviewer Metric

-- | ('Database.HDBC.SqlInteger') Fetch all reviewer metric scores associated
--   with a specific metric.
getReviewerMetricsMetricIDP :: StatementInitializer
getReviewerMetricsMetricIDP = mkInit "select * from \"reviewer_metric\" where \"metric_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all reviewer metric scores associated
--   with a specific reviewer.
getReviewerMetricsReviewerIDP :: StatementInitializer
getReviewerMetricsReviewerIDP = mkInit "select * from \"reviewer_metric\" where \"reviewer_id\" = ?;"

-- *** Interviewer Metric

-- | ('Database.HDBC.SqlInteger') Fetch all interviewer metric scores associated
--   with a specific metric.
getInterviewerMetricsMetricIDP :: StatementInitializer
getInterviewerMetricsMetricIDP = mkInit "select * from \"interviewer_metric\" where \"metric_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all interviewer metric scores associated
--   with a specific interviewer.
getInterviewerMetricsInterviewerIDP :: StatementInitializer
getInterviewerMetricsInterviewerIDP = mkInit "select * from \"interviewer_metric\" where \"interviewer_id\" = ?;"

-- *** Answer

-- | ('Database.HDBC.SqlInteger') Fetch all application answers associated with
--   a specific application.
getAnswersApplicationIDP :: StatementInitializer
getAnswersApplicationIDP = mkInit "select * from \"answers\" where \"application_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all application answers associated with
--   a specific question.
getAnswersQuestionIDP :: StatementInitializer
getAnswersQuestionIDP = mkInit "select * from \"answers\" where \"question_id\" = ?;"

-- *** Housing Evaluator

-- | ('Database.HDBC.SqlInteger') Fetch all housing evaluators associated with
--   a specific housing eval.
getHousingEvaluatorsHousingEvalIDP :: StatementInitializer
getHousingEvaluatorsHousingEvalIDP = mkInit "select * from \"housing_evaluator\" where \"housing_eval_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all housing evaluators associated with
--   a specific member.
getHousingEvaluatorsMemberIDP :: StatementInitializer
getHousingEvaluatorsMemberIDP = mkInit "select * from \"housing_evaluator\" where \"member_id\" = ?;"

-- *** Dues

-- | ('Database.HDBC.SqlInteger') Fetch all dues records associated with a
--   specific term.
getDuesTermIDP :: StatementInitializer
getDuesTermIDP = mkInit "select * from \"dues\" where \"term_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all dues records associated with a
--   specific member.
getDuesMemberIDP :: StatementInitializer
getDuesMemberIDP = mkInit "select * from \"dues\" where \"member_id\" = ?;"

-- *** Statement Exec

-- | ('Database.HDBC.SqlInteger') Fetch all statement executions associated
--    with a specific statement.
getStatementExecStatementIDP :: StatementInitializer
getStatementExecStatementIDP = mkInit "select * from \"statement_exec\" where \"statement_id\" = ?;"

-- | ('Database.HDBC.SqlInteger') Fetch all statement executions associated
--    with a specific member.
getStatementExecMemberIDP :: StatementInitializer
getStatementExecMemberIDP = mkInit "select * from \"statement_exec\" where \"member_id\" = ?;"

-- ** Object Initializers:

-- The following prepared statements instantiate specific objects in the model.
-- Depending on the object, various specialized instantiators may be provided
-- to handle common cases. Unfortunately, Haskell's type system is powerless to
-- ensure the correctness of these queries, so the expected HDBC type(s) and
-- semantics of the positional parameters are provided.

-- *** Member

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlByteString' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlByteString') Instantiate an introductory member. The key
--   differences between this statement and 'mkExtantMemberP' are the addition
--   of password handling information and the lack of housing points or on
--   floor status information.
mkIntroMemberP :: StatementInitializer
mkIntroMemberP = mkInit "insert into \"member\" (\"uuid\", \"username\", \"commonname\", \"password_hash\", \"password_salt\") values (?, ?, ?, ?, ?);"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlBool') Instantiate a member who already exists, i.e. has
--   associated records in a previous evaluations records system.
mkExtantMemberP :: StatementInitializer
mkExtantMemberP = mkInit "insert into \"member\" (\"uuid\", \"username\", \"commonname\", \"housing_points\", \"onfloor_status\") values (?, ?, ?, ?, ?);"

-- *** Event

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlUTCTime' X
--   'Database.HDBC.SqlString' X 'Database.HDBC.SqlString' X
--   'Database.HDBC.SqlByteString') Instantiate an event.
mkEventP :: StatementInitializer
mkEventP = mkInit "insert into \"event\" (\"title\", \"held\", \"category\", \"committee\", \"description\") values (?, ?, ?, ?, ?);"

-- *** Project

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlString' X
--   'Database.HDBC.SqlString') Instantiate a project.
mkProjectP :: StatementInitializer
mkProjectP = mkInit "insert into \"project\" (\"title\", \"description\", \"submitted\", \"committee\", \"project_type\") values (?, ?, ?, ?, ?);"

-- *** Evaluation

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlUTCTime' X
--   'Database.HDBC.SqlString') Instantiate an evaluation.
mkEvaluationP :: StatementInitializer
mkEvaluationP = mkInit "insert into \"evaluation\" (\"member_id\", \"deadline\", \"eval_type\") values (?, ?, ?);"

-- *** Conditional

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlUTCTime' X
--   'Database.HDBC.SqlByteString') Instantiate a conditional.
mkConditionalP :: StatementInitializer
mkConditionalP = mkInit "insert into \"conditional\" (\"evaluation_id\", \"deadline\", \"description\") values (?, ?, ?);"

-- *** Freshman Project

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlUTCTime') Instantiate
--   a freshman project.
mkFreshmanProjectP :: StatementInitializer
mkFreshmanProjectP = mkInit "insert into \"freshman_project\" (\"description\", \"project_date\") values (?, ?);"

-- *** Packet

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlUTCTime' X
--   'Database.HDBC.SqlInteger') Instantiate a packet.
mkPacketP :: StatementInitializer
mkPacketP = mkInit "insert into \"packet\" (\"member_id\", \"due_date\", \"percent_req\") values (?, ?, ?);"

-- *** Queue

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlUTCTime') Instantiate a
--   queue entry.
mkQueueP :: StatementInitializer
mkQueueP = mkInit "insert into \"queue\" (\"member_id\", \"entered\") values (?, ?);"

-- *** Application

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlUTCTime') Instantiate an
--   application.
mkApplicationP :: StatementInitializer
mkApplicationP = mkInit "insert into \"application\" (\"member_id\", \"created\") values (?, ?);"

-- *** Metric

-- | ('Database.HDBC.SqlByteString') Instantiate a metric.
mkMetricP :: StatementInitializer
mkMetricP = mkInit "insert into \"metric\" (\"name\") values (?);"

-- *** Reviewer

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlUTCTime') Instantiate a
--   reviewer.
mkReviewerP :: StatementInitializer
mkReviewerP = mkInit "inset into \"reviewer\" (\"member_id\", \"application_id\", \"review_start\", \"review_submit\") values (?, ?, ?, ?);"

-- *** Interviewer

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlUTCTime') Instantiate an interviewer.
mkInterviewerP :: StatementInitializer
mkInterviewerP = mkInit "insert into \"interviewer\" (\"member_id\", \"application_id\", \"interview_date\") values (?, ?, ?);"

-- *** Question

-- | ('Database.HDBC.SqlByteString') Instantiate a question.
mkQuestionP :: StatementInitializer
mkQuestionP = mkInit "insert into \"question\" (\"query\") values (?);"

-- *** Housing Eval

-- | ('Database.HDBC.SqlUTCTime') Instantiate a housing evaluation.
mkHousingEvalP :: StatementInitializer
mkHousingEvalP = mkInit "insert into \"housing_eval\" (\"eval_date\") values (?);"

-- *** Term

-- | ('Database.HDBC.SqlUTCTime') Instantiate a term.
mkTermP :: StatementInitializer
mkTermP = mkInit "insert into \"term\" (\"start_date\") values (?);"

-- *** Statement

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlBool') Instantiate
--   a statement.
mkStatementP :: StatementInitializer
mkStatementP = mkInit "insert into \"statement\" (\"sg_record\", \"side_effects\") values (?, ?);"

-- ** Context Initializers

-- The following prepared statements instantiate specific contexts in the model.
-- Depending on the context, various specialized instantiators may be provided
-- to handle common cases. Unfortunately, Haskell's type system is powerless to
-- ensure the correctness of these queries, so the expected HDBC type(s) and
-- semantics of the positional parameters are provided.

-- *** Eboard

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlString' X
--   'Database.HDBC.SqlUTCTime') Grant an Eboard context to a given member.
grEboardP :: StatementInitializer
grEboardP = mkInit "insert into \"eboard\" (\"member_id\", \"committee\", \"start_date\") values (?, ?, ?);"

-- ***Room:

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlUTCTime') Grant a room occupancy context to a given
--   member.
grRoomP :: StatementInitializer
grRoomP = mkInit "insert into \"room\" (\"member_id\", \"room_number\", \"start_date\") values (?, ?, ?);"

-- *** Membership

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlString' X
--   'Database.HDBC.SqlUTCTime') Grant a membership context to a given member.
grMembershipP :: StatementInitializer
grMembershipP = mkInit "insert into \"membership\" (\"member_id\", \"status\", \"start_date\") values (?, ?, ?);"

-- *** Event Attendee

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Grant an event
--   attendee context to the given member for the given event.
grEventAttendeeP :: StatementInitializer
grEventAttendeeP = mkInit "insert into \"event_attendee\" (\"member_id\", \"event_id\") values (?, ?);"

-- *** Project Participant

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlByteString') Grant a project participant context to a
--   given member for a given project.
grProjectParticipantP :: StatementInitializer
grProjectParticipantP = mkInit "insert into \"project_participant\" (\"member_id\", \"project_id\", \"description\") values (?, ?, ?)"

-- *** Freshman Project Participant

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Grant a freshman
--   project participant context to the given evaluation instance for the given
--   freshman project.
grFreshmanProjectParticipantP :: StatementInitializer
grFreshmanProjectParticipantP = mkInit "insert into \"freshman_project_participant\" (\"freshman_project_id\", \"evaluation_id\") values (?, ?);"

-- *** Signature

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlBool') Grant a signature context to the given packet for
--   the given member, including whether or not the signature was required.
grSignatureP :: StatementInitializer
grSignatureP = mkInit "insert into \"signature\" (\"member_id\", \"packet_id\", \"required\") values (?, ?, ?);"

-- *** Reviewer Metric

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger') Grant a reviewer metric context to the given
--   metric for the given reviewer, including the score.
grReviewerMetricP :: StatementInitializer
grReviewerMetricP = mkInit "insert into \"reviewer_metric\" (\"metric_id\", \"reviewer_id\", \"score\") values (?, ?, ?);"

-- *** Interviewer Metric

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger') Grant an interviewer metric context to the
--   given metric for the given interviewer, including the score.
grInterviewerMetricP :: StatementInitializer
grInterviewerMetricP = mkInit "insert into \"interviewer_metric\" (\"metric_id\", \"interviewer_id\", \"score\") values (?, ?, ?);"

-- *** Answer

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlByteString') Grant an answer context to the given
--   question for the given application, including the response.
grAnswerP :: StatementInitializer
grAnswerP = mkInit "insert into \"answer\" (\"application_id\", \"question_id\", \"response\") values (?, ?, ?);"

-- *** Housing Evaluator

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger') Grant a housing evaluator context to the given
--   member for the given housing evaluation, including the score.
grHousingEvaluatorP :: StatementInitializer
grHousingEvaluatorP = mkInit "insert into \"housing_evaluator\" (\"housing_eval_id\", \"member_id\", \"score\") values (?, ?, ?);"

-- *** Dues

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlString') Grant a dues context to the given member for
--   the given term, including the status.
grDuesP :: StatementInitializer
grDuesP = mkInit "insert into \"dues\" (\"term_id\", \"member_id\", \"status\") values (?, ?, ?);"

-- *** Statement Exec

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlUTCTime') Grant a statement execution context to the
--   given statement for the given member, including the time stamp.
grStatementExecP :: StatementInitializer
grStatementExecP = mkInit "insert into \"statement_exec\" (\"statement_id\", \"member_id\", \"timestamp\") values (?, ?, ?);"

-- ** Object Mutators

-- The following prepared statements mutate specific objects in the model.
-- Depending on the object, various specialized mutators may be provided to
-- handle common cases. Unfortunately, Haskell's type system is powerless to
-- ensure the correctness of these queries, so the expected HDBC type(s) and
-- semantics of the positional parameters are provided.

-- *** Member

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a
--   given member's common name.
upMemberCommonNameP :: StatementInitializer
upMemberCommonNameP = mkInit "update \"member\" set \"commonname\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlByteString' X
--   'Database.HDBC.SqlInteger') Update a given member's password hash and
--   salt.
upMemberPasswordP :: StatementInitializer
upMemberPasswordP = mkInit "update \"member\" set \"password_hash\" = ?, \"password_salt\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Update a given
--   member's housing points.
upMemberHousingPointsP :: StatementInitializer
upMemberHousingPointsP = mkInit "update \"member\" set \"housing_points\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger') Update a given
--   member's on floor status.
upMemberOnfloorStatusP :: StatementInitializer
upMemberOnfloorStatusP = mkInit "update \"member\" set \"onfloor_status\" = ? where \"id\" = ?;"

-- *** Event

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a
--   given event's title.
upEventTitleP :: StatementInitializer
upEventTitleP = mkInit "update \"event\" set \"title\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   event's held time.
upEventHeldP :: StatementInitializer
upEventHeldP = mkInit "update \"event\" set \"held\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   event's category.
upEventCategoryP :: StatementInitializer
upEventCategoryP = mkInit "update \"event\" set \"category\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   event's committee.
upEventCommitteeP :: StatementInitializer
upEventCommitteeP = mkInit "update \"event\" set \"committee\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   event's description.
upEventDescriptionP :: StatementInitializer
upEventDescriptionP = mkInit "update \"event\" set \"description\" = ? where \"id\" = ?;"

-- *** Project

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a
--   given project's title.
upProjectTitleP :: StatementInitializer
upProjectTitleP = mkInit "update \"project\" set \"title\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a
--   given project's description..
upProjectDescriptionP :: StatementInitializer
upProjectDescriptionP = mkInit "update \"project\" set \"description\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   project's submission time.
upProjectSubmittedP :: StatementInitializer
upProjectSubmittedP = mkInit "update \"project\" set \"submitted\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   project's passed time.
upProjectPassedP :: StatementInitializer
upProjectPassedP = mkInit "update \"project\" set \"passed\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   project's committee.
upProjectCommitteeP :: StatementInitializer
upProjectCommitteeP = mkInit "update \"project\" set \"committee\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   project's project type.
upProjectTypeP :: StatementInitializer
upProjectTypeP = mkInit "update \"project\" set \"project_type\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   project's comments.
upProjectCommentsP :: StatementInitializer
upProjectCommentsP = mkInit "update \"project\" set \"comments\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   project's status.
upProjectStatusP :: StatementInitializer
upProjectStatusP = mkInit "update \"project\" set \"status\" = ? where \"id\" = ?;"

-- *** Evaluation

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   evaluation's comments.
upEvaluationCommentsP :: StatementInitializer
upEvaluationCommentsP = mkInit "update \"evaluation\" set \"comments\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   evaluation's deadline.
upEvaluationDeadlineP :: StatementInitializer
upEvaluationDeadlineP = mkInit "update \"evaluation\" set \"deadline\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger') Update a given
--   evaluation's public availability.
upEvaluationAvailableP :: StatementInitializer
upEvaluationAvailableP = mkInit "update \"evaluation\" set \"available\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   evaluation's status.
upEvaluationStatusP :: StatementInitializer
upEvaluationStatusP = mkInit "update \"evaluation\" set \"status\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   evaluation's type.
upEvaluationTypeP :: StatementInitializer
upEvaluationTypeP = mkInit "update \"evaluation\" set \"eval_tyep\" = ? where \"id\" = ?;"

-- *** Conditional

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   conditional's deadline.
upConditionalDeadlineP :: StatementInitializer
upConditionalDeadlineP = mkInit "update \"conditional\" set \"deadline\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   conditional's description.
upConditionalDescriptionP :: StatementInitializer
upConditionalDescriptionP = mkInit "update \"conditional\" set \"description\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   conditional's comments.
upConditionalCommentsP :: StatementInitializer
upConditionalCommentsP = mkInit "update \"conditional\" set \"comments\" = ? where \"id\" = ?;"

-- *** Freshman Project

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   freshman project's description.
upFreshmanProjectDescriptionP :: StatementInitializer
upFreshmanProjectDescriptionP = mkInit "update \"freshman_project\" set \"description\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   freshman project's date.
upFreshmanProjectDateP :: StatementInitializer
upFreshmanProjectDateP = mkInit "update \"freshman_project\" set \"project_date\" = ? where \"id\" = ?;"

-- Packet:

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   packet's due date.
upPacketDueDateP :: StatementInitializer
upPacketDueDateP = mkInit "update \"packet\" set \"due_date\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger') Update a given
--   packet's percent required.
upPacketPercentReqP :: StatementInitializer
upPacketPercentReqP = mkInit "update \"packet\" set \"percent_req\" = ? where \"id\" = ?;"

-- *** Queue

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   queue's entry date.
upQueueEnteredP :: StatementInitializer
upQueueEnteredP = mkInit "update \"queue\" set \"entered\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   queue's exit date.
upQueueExitedP :: StatementInitializer
upQueueExitedP = mkInit "update \"queue\" set \"exited\" = ? where \"id\" = ?;"

-- *** Application:

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   application's creation date.
upApplicationCreatedP :: StatementInitializer
upApplicationCreatedP = mkInit "update \"application\" set \"created\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger') Update a given
--   application's status date.
upApplicationStatusP :: StatementInitializer
upApplicationStatusP = mkInit "update \"application\" set \"status\" = ? where \"id\" = ?;"

-- *** Metric

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   metric's name.
upMetricNameP :: StatementInitializer
upMetricNameP = mkInit "update \"metric\" set \"name\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger') Update a given
--   metric's activity.
upMetricActiveP :: StatementInitializer
upMetricActiveP = mkInit "update \"metric\" set \"active\" = ? where \"id\" = ?;"

-- *** Reviewer

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   review's start date.
upReviewerReviewStartP :: StatementInitializer
upReviewerReviewStartP = mkInit "update \"reviewer\" set \"review_start\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   review's submission date.
upReviewerReviewSubmitP :: StatementInitializer
upReviewerReviewSubmitP = mkInit "update \"reviewer\" set \"review_submit\" = ? where \"id\" = ?;"

-- *** Interviewer

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   interview's date.
upInterviewerDateP :: StatementInitializer
upInterviewerDateP = mkInit "update \"interviewer\" set \"interview_date\" = ? where \"id\" = ?;"

-- *** Question

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger') Update a given
--   question's activity.
upQuestionActiveP :: StatementInitializer
upQuestionActiveP = mkInit "update \"question\" set \"active\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   question's text.
upQuestionQueryP :: StatementInitializer
upQuestionQueryP = mkInit "update \"question\" set \"query\" = ? where \"id\" = ?;"

-- *** Housing Eval

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   housing evaluation's date.
upHousingEvalDateP :: StatementInitializer
upHousingEvalDateP = mkInit "update \"housing_eval\" set \"eval_date\" = ? where \"id\" = ?;"

-- Term:

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   term's start date.
upTermStartDateP :: StatementInitializer
upTermStartDateP = mkInit "update \"term\" set \"start_date\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlUTCTime' X 'Database.HDBC.SqlInteger') Update a given
--   term's end date.
upTermEndDateP :: StatementInitializer
upTermEndDateP = mkInit "update \"term\" set \"end_date\" = ? where \"id\" = ?;"

-- *** Statement

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger') Update a given
--   'Statement's 'StatementGroup' record field name.
upStatementSgRecordP :: StatementInitializer
upStatementSgRecordP = mkInit "update \"statement\" set \"sg_record\" = ? where \"id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger') Update a given
--   'Statement's side effects flag.
upStatementSideEffectsP :: StatementInitializer
upStatementSideEffectsP = mkInit "update \"statement\" set \"side_effects\" = ? where \"id\" = ?;"

-- ** Context Mutators

-- The following prepared statements mutate specific contexts in the model.
-- Depending on the context, various specialized mutators may be provided to
-- handle common cases. Unfortunately, Haskell's type system is powerless to
-- ensure the correctness of these queries, so the expected HDBC type(s) and
-- semantics of the positional parameters are provided.

-- *** Event Attendee

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member was a host
--   of a given event.
upEventAttendeeHostP :: StatementInitializer
upEventAttendeeHostP = mkInit "update \"event_attendee\" set \"host\" = ? where \"member_id\" = ? and \"event_id\" = ?;"

-- *** Project Participant

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update a given member's project participation
--   description for a given project.
upProjectParticipantDescriptionP :: StatementInitializer
upProjectParticipantDescriptionP = mkInit "update \"project_participant\" set \"description\" = ? where \"member_id\" = ? and \"project_id\" = ?;"

-- *** Freshman Project Participant

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member served on a
--   given freshman project's Eboard.
upFreshmanProjectParticipantEboardP :: StatementInitializer
upFreshmanProjectParticipantEboardP = mkInit "update \"freshman_project_participant\" set \"eboard\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member passed a
--   given freshman project.
upFreshmanProjectParticipantResultP :: StatementInitializer
upFreshmanProjectParticipantResultP = mkInit "update \"freshman_project_participant\" set \"result\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update comments on a given member's
--   participation in a given freshman project.
upFreshmanProjectParticipantCommentsP :: StatementInitializer
upFreshmanProjectParticipantCommentsP = mkInit "update \"freshman_project_participant\" set \"comments\" = ? where \"freshman_project_id\" = ? and \"evaluation_id\" = ?;"

-- *** Signature:

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member's
--   signature was required on a given packet.
upSignatureRequiredP :: StatementInitializer
upSignatureRequiredP = mkInit "update \"signature\" set \"required\" = ? where \"member_id\" = ? and \"packet_id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member
--   signed a given packet.
upSignatureSignedP :: StatementInitializer
upSignatureSignedP = mkInit "update \"signature\" set \"signed\" = ? where \"member_id\" = ? and \"packet_id\" = ?;"

-- *** Reviewer Metric

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update a given reviewer's score with respect to
--   a given metric.
upReviewerMetricScoreP :: StatementInitializer
upReviewerMetricScoreP = mkInit "update \"reviewer_metric\" set \"score\" = ? where \"metric_id\" = ? and \"reviewer_id\" = ?;"

-- *** Interviewer Metric:

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update a given interviewer's score with respect
--   to a given metric.
upInterviewerMetricScoreP :: StatementInitializer
upInterviewerMetricScoreP = mkInit "update \"interviewer_metric\" set \"score\" = ? where \"metric_id\" = ? and \"interviewer_id\" = ?;"

-- *** Answer

-- | ('Database.HDBC.SqlByteString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update a given applicant's response to a given
--   question.
upAnswerResponseP :: StatementInitializer
upAnswerResponseP = mkInit "update \"answer\" set \"response\" = ? where \"application_id\" = ? and \"question_id\" = ?;"

-- *** Housing Evaluator

-- | ('Database.HDBC.SqlInteger' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update a given housing evaluator's score
--   received during a given housing evaluation.
upHousingEvaluatorScoreP :: StatementInitializer
upHousingEvaluatorScoreP = mkInit "update \"housing_evaluator\" set \"score\" = ? where \"housing_eval_id\" = ? and \"member_id\" = ?;"

-- | ('Database.HDBC.SqlBool' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given housing
--   voted during a given housing evaluation.
upHousingEvaluatorVotedP :: StatementInitializer
upHousingEvaluatorVotedP = mkInit "update \"housing_evaluator\" set \"voted\" = ? where \"housing_eval_id\" = ? and \"member_id\" = ?;"

-- *** Dues

-- | ('Database.HDBC.SqlString' X 'Database.HDBC.SqlInteger' X
--   'Database.HDBC.SqlInteger) Update whether or not a given member payed dues
--   during a given term
upDuesStatusP :: StatementInitializer
upDuesStatusP = mkInit "update \"dues\" set \"status\" = ? where \"term_id\" = ? and \"member_id\" = ?;"
