#!/usr/bin/env ruby

require "c:\\Users\\pschaaf\\lib\\ruby\\FileParser\\lib\\quiz.rb"
require 'date'

aQuiz = Quiz.new do |q|
  q.course           = :"ClaimCenter 5.0 Functional Certification Exam"
  q.title            = :"ClaimCenter 5.0 Functional Certification Exam"
  q.student_id       = :josephchockchiongtdassurancecom6711211
  q.student_name     = 'Joseph Chock Chiong'
  q.student_email    = :"joseph.chockchiong@tdassurance.com"
  q.student_company  = 'TD Insurance'
  q.exam_date        = Date.parse('2011-04-24')
  q.score            = 65
end


# =========
aQuiz.new_question do |q|
  q.id         = 82
  q.topic      = 'ClaimCenter Business Concepts'
  q.question   = 'Which 2 of the following statements are true about the New Claim Wizard in a base (unconfigured) installation of ClaimCenter?
'
  q.add_choice   'You cannot create a Claim against an Unverified Policy'
  q.add_choice   'A temporary Claim number is generated after completing the first step of either the full or Quick wizards.'
  q.add_choice   'You can temporarily leave the Wizard to create a different Claim without losing any of your work'
  q.add_choice   'You can complete the wizard without navigating to any of the independent steps'
  q.add_choice   'You cannot complete the wizard without navigating through every ordered step'
  q.answers    = [:B, :D]
  q.responses  = [:B, :C]
end


# =========
aQuiz.new_question do |q|
  q.id         = 82
  q.topic      = 'ClaimCenter Business Concepts'
  q.question   = 'Which 2 of the following statements are true about the New Claim Wizard in a base (unconfigured) installation of ClaimCenter?
'
  q.add_choice   'You cannot create a Claim against an Unverified Policy'
  q.add_choice   'A temporary Claim number is generated after completing the first step of either the full or Quick wizards.'
  q.add_choice   'You can temporarily leave the Wizard to create a different Claim without losing any of your work'
  q.add_choice   'You can complete the wizard without navigating to any of the independent steps'
  q.add_choice   'You cannot complete the wizard without navigating through every ordered step'
  q.answers    = [:B, :D]
  q.responses  = [:B, :C]
end


# =========
q = aQuiz.new_question(
  :id         => 78,
  :topic      => 'ClaimCenter Business Concepts',
  :question   => 'In the New Claim Wizard, which 3 of the following statements are correct?',
  :add_choice => 'To go to a previous step you must click the &quot;Back&quot; button',
  :add_choice => 'The Claim is not saved until after you click the &quot;Finish&quot; button',
  :add_choice => 'Independent New Claim Wizard Screens do not intervene with main wizard navigation and, once they become available, can be selected at any time during claim setup',
  :add_choice => 'The Claim is saved in Draft status once you complete the first step of the New Claim Wizard',
  :add_choice => 'In an unconfigured ClaimCenter you must complete (&quot;Finish&quot; or &quot;Cancel&quot;) the wizard before you can create another Claim.',
  :answers    => [:C, :D, :E],
  :responses  => [:B, :C, :D]
)


# =========
aQuiz.new_question do |q|
  q.id         = 79
  q.topic      = 'ClaimCenter Business Concepts'
  q.question   = 'Which 2 of the following statements correctly reflect the setup process that runs at the end of the New Claim Wizard?
'
  q.add_choice   'The same rule sets are run regardless of whether an automated or manual assignment is selected'
  q.add_choice   'Claim segmentation rules can only update the Segment property of the Claim'
  q.add_choice   'The Segment property is used to categorize each Claim and Exposure'
  q.add_choice   'Claim rule sets are always run before Exposure rule sets for the same rule set category'
  q.add_choice   'Validation rules are run after PostSetup rules'
  q.answers    = [:C, :E]
  q.responses  = [:B, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 109
  q.topic      = 'ClaimCenter Business Concepts'
  q.question   = 'Which 2 statements about ClaimCenter groups are correct?
'
  q.add_choice   'They must directly correspond to the organisational structure of the carrier'
  q.add_choice   'They form a hierarchical structure'
  q.add_choice   'Each group can either include users or subgroups but not both'
  q.add_choice   'Each group is associated with zero or more regions'
  q.add_choice   'Each group is associated with one or more security zones'
  q.answers    = [:B, :D]
  q.responses  = [:A, :B]
end


# =========
aQuiz.new_question do |q|
  q.id         = 81
  q.topic      = 'ClaimCenter Business Concepts'
  q.question   = 'Which 3 of the following Claim rulesets are only run during claim setup?
'
  q.add_choice   'Initialization'
  q.add_choice   'Post-setup'
  q.add_choice   'Segmentation'
  q.add_choice   'Validation'
  q.add_choice   'Workplan'
  q.answers    = [:B, :C, :E]
  q.responses  = [:C, :D, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 116
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'After using the Wizard to create a Claim with one Exposure I notice that the Exposure is at the external validation level. What validation level can I assume the Claim is at?
'
  q.add_choice   '&quot;newloss&quot;, because the Wizard set it to that (the Exposure&#39;s level is irrelevant)'
  q.add_choice   'At least &quot;newloss&quot;, otherwise I wouldn&#39;t have been able to finish the Wizard (the Exposure&#39;s level is irrelevant)'
  q.add_choice   'At most &quot;iso&quot;, because the Exposure prevents the Claim from reaching &quot;external&quot;'
  q.add_choice   'At least &quot;external&quot;, otherwise the Claim would not have permitted the Exposure to reach &quot;external&quot;'
  q.add_choice   'I can&#39;t assume anything; this Claim may not even have been validated yet'
  q.answers    = [:B]
  q.responses  = [:E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 114
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 rulesets are triggered along with the Validation rules when a Claim is updated?
'
  q.add_choice   'Event Message'
  q.add_choice   'Pre-Setup'
  q.add_choice   'Pre-Update'
  q.add_choice   'Post-Setup'
  q.add_choice   'Post-Update'
  q.answers    = [:A, :C]
  q.responses  = [:C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 104
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 of the following statements about Reserve Lines are correct?
'
  q.add_choice   'Each CheckSet is linked to one and only one ReserveLine'
  q.add_choice   'There can be only one ReserveLine for each CostCategory'
  q.add_choice   'A ReserveLine must have a CostType and a CostCategory'
  q.add_choice   'A ReserveLine must refer to an Exposure'
  q.add_choice   'Every payment must come from a ReserveLine'
  q.answers    = [:C, :E]
  q.responses  = [:C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 99
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Currently, the incident description can be edited by many people. Which 2 of the following steps must I perform so that only those who are also Claims Supervisors can update it?
'
  q.add_choice   'Add &quot;updateIncidentDescription&quot; to ApplicationPermissionType then grant that permission to Claims Supervisor role'
  q.add_choice   'Add &quot;updateIncidentDescription&quot; to SystemPermissionType then grant that permission to Claims Supervisor role'
  q.add_choice   'Add the appropriate users to that Claim&#39;s ACL'
  q.add_choice   'Add perm.System.updateIncidentDescription to the editable property of that input'
  q.answers    = [:B, :D]
  q.responses  = [:A, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 119
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'If a Claim fails validation at the newloss level while it is being imported'
  q.add_choice   'An error message will appear in the browser'
  q.add_choice   'An error message will appear in the server log'
  q.add_choice   'The Claim will not be imported'
  q.add_choice   'The Claim&#39;s validation level will be null'
  q.add_choice   'The Claim&#39;s validation level will be &quot;loadsave&quot;'
  q.answers    = [:B, :E]
  q.responses  = [:A, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 117
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 of the following requirements can be implemented with Validation Rules?
'
  q.add_choice   'Display a warning when required data is missing'
  q.add_choice   'Display an edit mask (like &quot;../../..&quot;) to guide data entry'
  q.add_choice   'Treat the same field differently on different screens'
  q.add_choice   'Treat the same field differently for imported data'
  q.add_choice   'Enable a button only when a field value is set correctly'
  q.answers    = [:A, :D]
  q.responses  = [:A, :B]
end


# =========
aQuiz.new_question do |q|
  q.id         = 107
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 4 of the following statements are true about how ClaimCenter handles financial transaction approvals?
'
  q.add_choice   'It examines every payment to see whether it requires approval'
  q.add_choice   'It examines every reserve change to see whether it requires approval'
  q.add_choice   'Some financial approval tests are done using Authority Limits'
  q.add_choice   'Some financial approval tests are done using Transaction Approval Rules'
  q.add_choice   'Some financial approval tests are done using Transaction Validation Rules'
  q.answers    = [:A, :B, :C, :D]
  q.responses  = [:A, :B, :D, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 89
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 of the following entities can be the owner of a Contact Role?
'
  q.add_choice   'Activity'
  q.add_choice   'ClaimContact'
  q.add_choice   'Contact'
  q.add_choice   'Policy'
  q.add_choice   'PropertyIncident'
  q.answers    = [:D, :E]
  q.responses  = [:B, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 98
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'If I create a new ExposureType I also need to create a dedicated DetailView that will allow me to'
  q.add_choice   'Create this type of Exposure within the New Claim Wizard'
  q.add_choice   'View this type of Exposure on any existing Claim'
  q.add_choice   'View this type of Exposure on an imported Claim'
  q.add_choice   'Create this type of Exposure within the FNOL snapshot'
  q.add_choice   'View this type of Exposure in an FNOL snapshot'
  q.answers    = [:A, :B, :E]
  q.responses  = [:C, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 111
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 statements about Validation Level in ClaimCenter are correct?
'
  q.add_choice   'You can add a new Validation Level anywhere in the list'
  q.add_choice   'You can only add a new Validation level only between loadsave and payment'
  q.add_choice   'You can remove any existing Validation level'
  q.add_choice   'You can explicitly set a Claim&#39;s validation level'
  q.add_choice   'When an entity reaches a particular validation level it cannot move back to a lower level'
  q.answers    = [:A, :E]
  q.responses  = [:B, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 95
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 3 statements about Line of Business configuration in ClaimCenter are true?
'
  q.add_choice   'Every PolicyType must be related to an Incident or one of its subtypes'
  q.add_choice   'Each CoverageType and ExposureType typecode can relate to many CoverageSubTypes, therefore the relationship between CoverageType and ExposureType is effectively many-to-many'
  q.add_choice   'The ClaimCenter Lines of Business are defined in a series of 8 typelists'
  q.add_choice   'The ClaimCenter Lines of Business are defined in a series of 6 typelists'
  q.add_choice   'Some LOB changes require corresponding changes in typelists that are not considered part of the LOB model'
  q.answers    = [:B, :D, :E]
  q.responses  = [:A, :B, :C]
end


# =========
aQuiz.new_question do |q|
  q.id         = 83
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'To prevent users from being able to search ContactCenter for an existing contact but still allow them to create new contacts when editing the "Reported by" field on the Auto Loss Details page, you would
'
  q.add_choice   'Replace the ClaimContactInput with a RangeInput'
  q.add_choice   'Disable the ContactCenter plugin'
  q.add_choice   'Set the widget&#39;s newContactMenu attribute value to &quot;BlankNewContactPickerMenuItemSet()&quot;'
  q.add_choice   'The search option is built in and cannot be removed through configuration'
  q.add_choice   'None of the above'
  q.answers    = [:E]
  q.responses  = [:C]
end


# =========
aQuiz.new_question do |q|
  q.id         = 90
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 2 XML files contain the data that maps between ClaimCenter and ContactCenter?
'
  q.add_choice   'ab-to-cc-data-mapping.xml in ClaimCenter'
  q.add_choice   'cc-to-ab-data-mapping.xml in ClaimCenter'
  q.add_choice   'data-mapping.xml in ClaimCenter'
  q.add_choice   'cc-to-ab-data-mapping.xml in ContactCenter'
  q.add_choice   'ab-to-cc-data-mapping.xml in ContactCenter'
  q.answers    = [:A, :B]
  q.responses  = [:B, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 112
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'Which 4 of the following ClaimCenter entities are validateable in an unconfigured application?
'
  q.add_choice   'Activity'
  q.add_choice   'Group'
  q.add_choice   'Contact'
  q.add_choice   'VehicleIncident'
  q.add_choice   'User'
  q.answers    = [:A, :B, :C, :E]
  q.responses  = [:A, :B, :C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 118
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'If a Claim fails validation at the loadsave level while it is being imported...'
  q.add_choice   'An error message will appear in the browser'
  q.add_choice   'An error message will appear in the server log'
  q.add_choice   'The Claim will not be imported'
  q.add_choice   'The Claim&#39;s validation level will be null'
  q.add_choice   'The Claim&#39;s validation level will be &quot;loadsave&quot;'
  q.answers    = [:B, :C]
  q.responses  = [:A, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 84
  q.topic      = 'ClaimCenter Configuration Concepts'
  q.question   = 'According to the Data Dictionary, which 2 of the following would be valid expressions for the value range of a claim contact widget whose value is Claim.PrimaryDoctor?
'
  q.add_choice   'Claim.RelatedContacts'
  q.add_choice   'Claim.RelatedDoctorArray'
  q.add_choice   'Claim.RelatedPersonArray'
  q.add_choice   'Claim.doctor'
  q.add_choice   'Claim.RelatedPersonVendorArray'
  q.answers    = [:B, :D]
  q.responses  = [:A, :B]
end


# =========
aQuiz.new_question do |q|
  q.id         = 129
  q.topic      = 'Fundamentals Data Model'
  q.question   = 'Assuming that there exists in the base datamodel extendable entities called Official and Report, which 2 steps will ensure that a given Official can be associated with one or more Reports:
'
  q.add_choice   'The Report entity must have a Foreign Key to the Official entity'
  q.add_choice   'The Official entity must have a Foreign Key to the Report entity'
  q.add_choice   'The Report entity must have a Array Key to the Official entity'
  q.add_choice   'The Official entity must have an Array Key to the Report entity'
  q.add_choice   'The Entity Name for Official must include the Report entity'
  q.answers    = [:A, :D]
  q.responses  = [:C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 126
  q.topic      = 'Fundamentals Data Model'
  q.question   = 'Which 2 of the following approaches would remedy this console database upgrade error:
[java] Caused by:
com.guidewire.pl.system.exception.UpgradeException:
com.guidewire.pl.system.exception.TransactionException:
Transaction aborted by exception:
Can\'t add non-nullable column NumberOfDependents to existing table abx_User
because the column does not have a default value and the table is not empty.
'
  q.add_choice   'Increment the version number and try again'
  q.add_choice   'Ensure that all new User records have the NumberOfDependents value populated'
  q.add_choice   'Set the User entity Exportable property to false and retry'
  q.add_choice   'Set the NumberOfDependents nullok property to true and retry'
  q.add_choice   'Set the NumberOfDependents default property to 0 and retry'
  q.answers    = [:D, :E]
  q.responses  = [:B, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 137
  q.topic      = 'Fundamentals Data Model'
  q.question   = 'A field defined in the base data model uses a typefilter on a non-internal typelist. If I wanted to change the list of permissible values for this field, which 2 of the following steps could I perform?
'
  q.add_choice   'add new typecodes to the typefilter'
  q.add_choice   'remove existing typecodes from the typefilter'
  q.add_choice   'change the field to not use a typefilter'
  q.add_choice   'change the field to use a different typefilter'
  q.add_choice   'change the field to use a different typelist'
  q.answers    = [:A, :B]
  q.responses  = [:D, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 141
  q.topic      = 'Fundamentals GScript'
  q.question   = 'If aPerson.PrimaryAddress is currently null, then aPerson.PrimaryAddress.SomeStringField
'
  q.add_choice   'Will cause an error at runtime. It should be rewritten to test for null first.'
  q.add_choice   'Is illegal; Studio will color the code in red'
  q.add_choice   'Is inefficient; it should be rewritten to test for null first'
  q.add_choice   'Returns null'
  q.add_choice   'Returns the empty string &quot;&quot;'
  q.answers    = [:D]
  q.responses  = [:A]
end


# =========
aQuiz.new_question do |q|
  q.id         = 164
  q.topic      = 'Fundamentals UI'
  q.question   = 'A ListView panel'
  q.add_choice   'is used to define a dropdown selection list of records of the same type'
  q.add_choice   'may display the results of a query of records of the same type'
  q.add_choice   'may be navigated directly from a locationref to when defined as a separate PCF'
  q.add_choice   'may be embedded in a DetailView'
  q.add_choice   'may contain a Toolbar'
  q.answers    = [:B, :D]
  q.responses  = [:B, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 163
  q.topic      = 'Fundamentals UI'
  q.question   = 'The ListViewPanel\'s RowIterator element itself'
  q.add_choice   'specifies the object that each row element will display'
  q.add_choice   'limits the maximum number of rows to display'
  q.add_choice   'limits the maximum number of columns to display'
  q.add_choice   'can prevent the elements and their columns from being editable'
  q.add_choice   'can prevent the display of elements of one or more types'
  q.answers    = [:A, :B, :D]
  q.responses  = [:A, :C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 161
  q.topic      = 'Fundamentals UI'
  q.question   = 'Which display property would be valid for an Input widget with label displaykey.Web.ContactDetail.Messages(var1, var2)
'
  q.add_choice   '{1} / {0} messages have been sent'
  q.add_choice   '{1} / {2} messages have been sent'
  q.add_choice   '{0 / 1} messages have been sent'
  q.add_choice   '{1 / 2} messages have been sent'
  q.add_choice   '{var1} / {var2} messages have been sent'
  q.answers    = [:A]
  q.responses  = [:C]
end


# =========
aQuiz.new_question do |q|
  q.id         = 170
  q.topic      = 'Fundamentals UI'
  q.question   = 'A location has two Entry Points:
(1) ContactInfoPopup(thisContact:ABContact)
(2) ContactInfoPopup(thisContact:ABContact, editable:boolean)
and defines the variable editable : Boolean with an initialValue of false. Which 2 of the following statements are true?
'
  q.add_choice   'The variable editable will always be false'
  q.add_choice   'The variable editable will always be false when called with the 1st signature'
  q.add_choice   'The variable editable will always be false when called with the 2nd signature'
  q.add_choice   'The variable editable will always be required when called with the 2nd signature'
  q.answers    = [:B, :D]
  q.responses  = [:C, :D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 158
  q.topic      = 'Fundamentals UI'
  q.question   = 'An inputset whose editable property is set to "false"
'
  q.add_choice   'will not be visible to users'
  q.add_choice   'will not be editable when the edit button is selected'
  q.add_choice   'will only be editable if the edit button is selected'
  q.add_choice   'may contain input fields that dynamically are made editable when in edit mode'
  q.add_choice   'will be editable if the parent container (e.g. DetailView) is editable and in edit mode'
  q.answers    = [:B]
  q.responses  = [:D]
end


# =========
aQuiz.new_question do |q|
  q.id         = 151
  q.topic      = 'Fundamentals UI'
  q.question   = 'Which 2 of the following scenarios would require you to use an explicit typekeyinput widget instead of a plain input widget?
'
  q.add_choice   'The value is a typekey field that should be displayed as a dropdown list'
  q.add_choice   'You want to filter the list using GScript'
  q.add_choice   'You want to filter the list using a keyfilter'
  q.add_choice   'You wish to show &quot;not entered&quot; instead of &quot;none selected&quot;'
  q.add_choice   'You want to hide the field under certain conditions'
  q.answers    = [:B, :D]
  q.responses  = [:A, :C]
end


# =========
aQuiz.new_question do |q|
  q.id         = 162
  q.topic      = 'Fundamentals UI'
  q.question   = 'The cell widget in a ListView controls'
  q.add_choice   'The data value displayed'
  q.add_choice   'Whether the values in the row are editable'
  q.add_choice   'The column header'
  q.add_choice   'The column footer (e.g. sum or label)'
  q.add_choice   'Whether the values in the column are editable'
  q.answers    = [:A, :C, :D, :E]
  q.responses  = [:A, :B, :C, :E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 160
  q.topic      = 'Fundamentals UI'
  q.question   = 'For a PanelRef to call a modal DetailViewPanel
'
  q.add_choice   'the mode must be specified in the required variables of the DetailViewPanel'
  q.add_choice   'there must be one DetailViewPanel for each possible value of the mode variable'
  q.add_choice   'the mode variable must be a subtype of the object passed to the DetailViewPanel'
  q.add_choice   'the mode expression must evaluate to the name of the required mode'
  q.add_choice   'the mode must be passed in the argument list in the PanelRef'
  q.answers    = [:D]
  q.responses  = [:E]
end


# =========
aQuiz.new_question do |q|
  q.id         = 152
  q.topic      = 'Fundamentals UI'
  q.question   = 'Which 2 type(s) of widget could be used to display a date field in a List View as it is shown here?
'
  q.add_choice   'cell'
  q.add_choice   'dateinput'
  q.add_choice   'textcell'
  q.add_choice   'datecell'
  q.add_choice   'rangecell'
  q.answers    = [:A, :D]
  q.responses  = [:A, :B]
end


# =========
aQuiz.new_question do |q|
  q.id         = 167
  q.topic      = 'Fundamentals UI'
  q.question   = 'For a ListViewPanel to support adding elements to an array, you must'
  q.add_choice   'add a ToolbarButton widget'
  q.add_choice   'add an IteratorButtons widget'
  q.add_choice   'make the ListViewPanel editable'
  q.add_choice   'provide the RowIterator&#39;s autoAdd property'
  q.add_choice   'provide the RowIterator&#39;s toAdd property'
  q.answers    = [:B, :C, :E]
  q.responses  = [:A, :C, :E]

aQuiz