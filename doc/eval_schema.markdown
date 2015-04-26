# CSH Evaluations Database Database Schema

## Types

#### `committee_t

This represents the different committees. Committees are mutually
exclusive. The possible committees represented are 

*  `evals`     - Evaluations
*  `rnd`       - Research and Development
*  `social`    - Social
*  `history`   - History
*  `opcomm`    - Operational Communications
*  `imps`      - House Improvements
*  `financial` - Financial
*  `chariman`  - Chairman

#### `status_t`

The status of votes and evaluations. All votes and evaluations must have a
status, and all statuses are mutually exclusive. Possible statuses are

* pending - The vote or evaluation has not yet happened, for instance
            a submitted major project that has not yet come up, or a freshman
            who has not gone through their 10 week evaluation yet. 

* passed  - The vote or evaluation has passed.

* failed  - The vote or evaluation has failed.

#### `member_t`

Possible membership states a member can have. All members must have a 
membership status, and all membership statuses are mutually exclusive. 
Membership statuses defined here have a one-to-one coorespondence with
the membership statuses in the constitution. The possible membership
statuses are:

* `active` - from Articles Section 3.B Active Membership

* `alumni_good` - from Articles Section 3.C.2 Alumni Membership selection:
                   "Active members who depart house (i.e. resign) after passing
                    the current operating session’s Membership Evaluations are
                    considered to be Alumni in good standing."

* `alumni_bad` - from Articles Section 3.C.2 Alumni Membership selection:
                  "Active members who depart house without passing the current
                   operating session’s Membership Evaluations are considered to
                   be Alumni in bad standing."

* `honorary` - from Articles Section 3.D Honorary Membership.

* `advisory` - from Articles Section 3.E Advisory Membership.
               This should not be used to represent a reslife advisor

* `introductory` - from Articles Section 3.A Introductory Membership

* `non` - Represents a non-member who has information in the evaluations
          database. This membership status would be given to people who go
          through the evaluations process, but end up failing an evaluation
          before they have passed their first membership evaluation. 

#### `dues_t`

Represents the status of a member's owed dues. The possible values are

* `paid` - The member has paid dues.

* `exempt` - The member is exempted from paying dues.

#### `event_t`

Represents the type of an event. The possible values are

* `house` - A House Meeting.

* `social` - A social event.

* `committee` - A committee meeting.

* `seminar` - A technical seminar.

* `orientation` - A CSH Orientation event. This includes events like the House
                  Systems Seminar, which isn't a technical seminar, but has
                  attendance we need to keep for clerical reasons

#### `project_t`

Represents the type of a project. Currently, there is only one type of
project. This is built in to the schema in preperation for the points
system of evaluation, where there is the possibility of different types of
projects, such as a minor project or social project. Those types of
projects would be added to this enumeration.

* `major` - A major technical project. 

#### `eval_t`

Represents the type of an evaluation. This is enumerated to allow for
changes to the constitution modifying the current evaluations process,
or to allow for a future change in scope of the evaluations database. 

* introductory - An introductory evaluation (also refered to as a
    10-week evaluation)
* membership   - A membership evaluation

## Tables

### Objects

All objects in the database have a unique identifier of SQL type `bigserial`. 
This is an auto-incrementing field that can be used to unambiguously refer
to a specific object in the database. These can be considered the "discrete
units" that the evaluations database stores. 

#### `member`

Each entry in the member table represents a person with information stored
in the evaluations database. All people represented in the member table
are aloud to log into the evaluations database, although the information
they can access is restricted based on various attributes of the member,
such as membership status and eboard position. Attributes associated with
a member are

* `id` - unique identifier for the member in the evaluation database.
         This is the primary key for entries in the table.

* `uuid` - LDAP Universially Unique Identifier. Only members 
           with CSH LDAP entries (e.g. accounts) will have uuids. All uuids are 
           required to be unique.

* `username` - Username used to log into the evaluations database.
               This will be either the CSH LDAP username or a username chosen when
               the introductory account is created to exclusively log into the 
               evaluations database. All usernames are required to be unique.

* `commonname` - Common Name for the user (e.g. "Stephen Demos"). Corresponds to
                 a cn in LDAP

* `password_hash` - A hash of a password. This is used for members who
                     have an introductory account exclusively for logging into the
                     evaluations database. Users with LDAP entries will log in through
                     webauth will not use this field. 

* `password_salt` - Salt for the password. See `password_hash`.

* `housing_points` - The number of Housing Points accumulated by the
                      member.

* `onfloor_status` - True if the member has onfloor status as described in the
                      constitution, False otherwise.

#### `event`
#### `project`
#### `evaluation`
#### `conditional`
#### `freshman_project`
#### `packet`
#### `queue`
#### `application`
#### `metric`
#### `reviewer`
#### `interviewer`
#### `question`
#### `housing_eval`
#### `term`

## Contexts

A context is a table that does not have a unique identifier attribute. These
tables describe some aspect of the objects that they are referring to, or
give that object context. 

### Span Contexts

These tables describe spans of time that a member had a particular context.
For example, a member can be in a room for a span of time, described as a 
start date and an end date. If the end date for the span is null, the context
currently applies to the member. A member may not have two spans for one
context simultaneously. 

#### `eboard`

Stores information on the Executive Board positions of every member at 
any given time. The absence of an entry for a member on a particular date 
implies they did not hold an Executive Board position at on that date. 
A member may not hold two Executive Board positions simultaneously. The 
absence of an `end_date` attribute implies the member currently holds the
specified Executive Board position.

* `member_id` - The member that held the Executive Board position for
                 this span.

* `committee` - The committee that the Executive Board member was
                running for this span.

* `start_date` - The date that this Executive Board span started. 

* `end_date` - The date that this Executive Board span ended. If this
                attribute is null, the member currently holds this Executive Board
                position.

#### `room`
#### `membership`

### Participation Contexts

These tables describe participation. The participation can be for an event,
a project, or freshman project.

### `event_attendee`
### `project_participant`
### `freshman_project_participant`
### `housing_evaluator`

#### Other Contexts

### `signature`
### `reviewer_metric`
### `interviewer_metric`
### `answer`
### `dues`

### Logging

### `statement`
### `statement_exec`
