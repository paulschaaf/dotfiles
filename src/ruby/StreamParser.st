'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 11, 2003 at 4:46:43 pm'!

"\FILED_OUT_USING: Harmony Source Code Management System-Release 1.0 beta 2 "!

"\SUBJECT: + (1) <LIB: 1998_0127_1458_14> [Module] app.FileParser.contexts"!
"\COPYRIGHT: Copyright (C) 1998 Versant.  All Rights Reserved."!
"\FILED_OUT_BY: P.G. Schaaf"!
"\ENVIRONMENT: c:\Smalltlk\Harmony\paul"!
"\COMMENT: NONE"!

VersantDBApplicationContext subclass: #FileImportParserContext
	instanceVariableNames: ':prePersistentObjects, '
	classVariableNames: 'PersistentObjectNotFoundSignal '
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'I manage data that should be global to a File Import session, including a cache of objects that have not yet been marked persistent (this is done for efficiency reasons).'
=end!

!FileImportParserContext methodsFor: 'accessing'!

def addPrePersistentObject: anObject 
	^(prePersistentObjects at: anObject class ifAbsentPut: [Set new])
		add: anObject
end!

def prePersistentObjects

	| coll |
	coll := OrderedCollection new.
	self prePersistentObjectsDo: [:each | coll add: each].
	^coll
end!

def prePersistentObjectsClassesDo: block0 
	prePersistentObjects keysAndValuesDo: [:key :value | block0 value: key]
end!

def prePersistentObjectsDo: block0

	prePersistentObjects 
		keysAndValuesDo: [:key :value | value do: block0]
end!

def prePersistentObjectsOfClass: aClass
	^self
		prePersistentObjectsOfClass: aClass
		useSubclasses: false
end!

def prePersistentObjectsOfClass: aClass useSubclasses: aBoolean 
	^self
		prePersistentObjectsOfClass: aClass
		useSubclasses: aBoolean
		ifNone: OrderedCollection new
end!

def prePersistentObjectsOfClass: targetClass useSubclasses: aBoolean ifNone: ifNoneBlock
	| candidates |
	candidates := OrderedCollection new.

	prePersistentObjects keysAndValuesDo: [:aClass :aCollection |
		(aClass == targetClass or: [aClass inheritsFrom: targetClass])
			ifTrue: [candidates addAll: aCollection]
	].

	^candidates
end!

def removePrePersistentObject: anObject 

	^(prePersistentObjects at: anObject class) 
		remove: anObject
end! !

!FileImportParserContext methodsFor: 'accessing.signals'!

def persistentObjectNotFoundSignal
	"self current persistentObjectNotFoundSignal"

	^self class persistentObjectNotFoundSignal
end! !

!FileImportParserContext methodsFor: 'actions'!

def createInDatabaseUsing: block0
	| newInstance |
	newInstance := block0 value.
	^self addPrePersistentObject: newInstance
end!

def flushAllNonPersistentObjects
	"self current flushAllNonPersistentObjects"

	| flushCount |
	flushCount := 0.
	prePersistentObjects copy keysAndValuesDo: [:aClass :anInstanceCollection |
		anInstanceCollection copy do: [:each |
			each isODBPersistent
				ifFalse: [
					anInstanceCollection remove: each.
					flushCount := flushCount + 1
				]
		].
		anInstanceCollection isEmpty
			ifTrue: [prePersistentObjects removeKey: aClass].
	].
	self newLineTraceWith: flushCount asString, ' objects flushed.'
end!

def persistAllNonPersistentObjects
	"self current persistAllNonPersistentObjects"

	Cursor write showWhile: [
		self prePersistentObjectsDo: [:each | 
			each isODBPersistent
				ifFalse: [self makePersistent: each]
		]
	]
end! !

!FileImportParserContext methodsFor: 'actions.basic'!

def basicBeginSessionOn: aDatabase 
	self databaseInterface beginOLSessionOn: self database
end!

def doIfInProperSession: properSessionBlock0 ifInImproperSession: improperSessionBlock0 ifInNoSession: noSessionBlock0 
	^[super
		doIfInProperSession: properSessionBlock0
		ifInImproperSession: improperSessionBlock0
		ifInNoSession: noSessionBlock0]
		valueNowOrOnUnwindDo: [self closeReferencedStreams]
end! !

!FileImportParserContext methodsFor: 'actions.db.session'!

def commit
	super commit.
	self 
		initializePrePersistentObjects;
		traceTimeStamp
end!

def commitAndClean
	super commitAndClean.
	self 
		initializePrePersistentObjects;
		traceTimeStamp
end!

def endSession
	super endSession.
	self initializePrePersistentObjects.
	Screen default ringBell
end!

def rollback
	"We only wish to rollback if there is something to be rolled back."
	"self current rollback"

	self flushAllNonPersistentObjects.

	self hasPrePersistentObjects
		ifTrue: [self initializePrePersistentObjects].

	^super rollback
end! !

!FileImportParserContext methodsFor: 'debugging'!

def closeReferencedStreams
	self traceStreams do: [:each | each close]
end!

def nextPutAll: aString 
	self traceStreams
		do: 
			[:each | 
			each == Transcript ifFalse: [each setToEnd].
			each nextPutAll: aString; flush]
end! !

!FileImportParserContext methodsFor: 'initialize-release'!

def defaultToDebugOn
	^true
end!

def initialize
	super initialize.
	self initializePrePersistentObjects.
	self addTraceStream: ('d:parse' , Timestamp now asString , '.txt') asFilename writeStream
end!

def initializePrePersistentObjects
	"self current initializePrePersistentObjects"

	prePersistentObjects := IdentityDictionary new
end! !

!FileImportParserContext methodsFor: 'inquiries'!

def hasPrePersistentObjects
	"self current hasPrePersistentObjects"

	^(prePersistentObjects
		detect: [:each | each value size > 0]
		ifNone: []
	) ~~ nil
end!

def prePersistentObjectCount
	"self current prePersistentObjectCount"

	^prePersistentObjects
		inject: 0
		into: [:sum :each | sum + each value size]
end! !

FileImportParserContext class
	instanceVariableNames: ''!

!FileImportParserContext class methodsFor: 'accessing'!

def defaultDatabase
	"self defaultDatabase"
	
	^(Dictionary new
		at: #G00046 put: 'pauldb2@bprsvr4';
		at: #E16718 put: 'CIsmall@bprsvr4';
		yourself
	) at: self currentUserID
end! !

!FileImportParserContext class methodsFor: 'accessing.signals'!

def persistentObjectNotFoundSignal
	^PersistentObjectNotFoundSignal
end! !

!FileImportParserContext class methodsFor: 'class initialization'!

def initializeSignals

	PersistentObjectNotFoundSignal := (Object errorSignal newSignalMayProceed: true)
		notifierString: 'The requested object was not found.';
		nameClass: self 
		message: #databaseContextSignal.
end! !

!FileImportParserContext class methodsFor: 'utility'!

def rootClasses
	^super rootClasses
		add: StreamParseNode;
		add: FileParser;
		yourself
end! !

'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 11, 2003 at 4:46:33 pm'!

"\FILED_OUT_USING: Harmony Source Code Management System-Release 1.0 beta 2"!

"\SUBJECT: + (2) <loc: 1998-03-07_16-54-37> [Module] app.FileParser"!
"\COPYRIGHT: Copyright (C) 1998 Versant.  All Rights Reserved."!
"\FILED_OUT_BY: P.G. Schaaf"!
"\ENVIRONMENT: c:\Smalltlk\Harmony\paul"!
"\COMMENT: NONE"!

Object subclass: #FileParser
	instanceVariableNames: ':registry, :registrar, :recordsDoneCount, :startTimeSeconds, :recordCount, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'I construct a machine of parse nodes that can interpret the contents of the file I represent.  I manage the process of interpretation and the resultant objects, including persistence issues.  

*************
If the files are not self-referential (if they are completely normalized relational tables and the ordering of the records within the file cannot affect the processing time) they can be split into an arbitrary number of pieces and divided up between images on a different machine.  If these parsings are done concurrently the run times can be made to be as low as you like.  
**************'
=end!

!FileParser methodsFor: 'accessing'!

def clientClass
	"Answer the class that I should use to create instances."

	^self class clientClass
end!

def clientCreator
	"\OWNERSHIP:	Copyright (C) 1996 P.G. Schaaf. All Rights Reserved."

	^self class clientCreator
end!

def clientTypeDescriptor
	^self clientClass printString
end!

def context
	^self class context
end!

def debuggingBlock
	"Answer a block that is to be evaluated periodically for debugging 
	purposes."

	| context sizeString |
	context := self context.
	^context debugIsOn not
		ifTrue: [ [] ]
		ifFalse: 
			[self recordsDoneCount: 0.
			sizeString := String new writeStream
				nextPutAll: '/~';
				nextPutAll: self recordCount asString;
				space;
				nextPutAll: self clientTypeDescriptor;
				nextPutAll: 's completed.';
				contents.

			[self incrementRecordsDoneCountBy: self debugCycleClientCount.
			context newLineTraceWith: self recordsDoneCount asString , sizeString]]
end!

def delimiter
	"Answer the delimiter to be used 
	between attributes in a record."

	^self class delimiter
end!

def estimatedCompletionTime

	^Time fromSeconds: (Time now asSeconds + self estimatedTimeRemaining)
end!

def estimatedTimeRemaining

	^ArithmeticValue divisionByZeroSignal
		handle: [:ex | 0]
		do: 
			[| percentageComplete secondsPassed totalSeconds |
			secondsPassed := Time now asSeconds - self startTimeSeconds.
			percentageComplete := self recordsDoneCount / self recordCount.
			totalSeconds := secondsPassed / percentageComplete.
			totalSeconds - secondsPassed]
end!

def fileName

	^self class fileName
end!

def newClient
	^self class clientCreator value
end!

def objectNotFoundRegistrarFor: aRegistry 
	"Answer an object that can track requests of the 
	persistent store that come up empty."

	^[:aSummary | 
	| client |
	client := aSummary client.
	(aRegistry at: client class ifAbsentPut: IdentityDictionary new)
		at: client identityHash put: aSummary predicate]
end!

def recordCount
	^recordCount
end!

def recordCount: aValue
	recordCount := aValue
end!

def recordsDoneCount
	^recordsDoneCount
end!

def recordsDoneCount: aValue
	recordsDoneCount := aValue
end!

def registrar
	^registrar
end!

def registrar: aValue 
	registrar := aValue
end!

def registry
	^registry
end!

def registry: aValue 
	registry := aValue
end!

def startTimeSeconds
	^startTimeSeconds
end!

def startTimeSeconds: aValue
	startTimeSeconds := aValue
end! !

!FileParser methodsFor: 'accessing.nodes'!

def booleanNodeOn: aStream 
	^self booleanNodeOn: aStream withDelimiter: self delimiter
end!

def characterNodeOn: aStream
	^self characterNodeOn: aStream withDelimiter: self delimiter
end!

def dateNodeOn: aStream 
	^self dateNodeOn: aStream withDelimiter: self delimiter
end!

def endOfLineAttributeNodeOn: aStream 

	^StreamAttributeParseNode 
		on: (self endOfLineNodeOn: aStream) 
		stReader: [:client | ] 
		stWriter: [:client :value | ] 
		odbValueTest: ''
end!

def endOfLineNodeOn: aStream 
	"\OWNERSHIP:	Copyright (C) 1996 P.G. Schaaf. All Rights Reserved."

	^self stringNodeOn: aStream withDelimiter: Character cr 
end!

def integerNodeOn: aStream 
	^self integerNodeOn: aStream withDelimiter: self delimiter
end!

def moneyNodeOn: aStream 
	^self moneyNodeOn: aStream withDelimiter: self delimiter
end!

def nilAttributeNodeOn: aStream 

	^StreamAttributeParseNode 
		on: (self nilNodeOn: aStream) 
		stReader: [:client | ] 
		stWriter: [:client :value | ] 
		odbValueTest: ''
end!

def nilNodeOn: aStream 
	^self nilNodeOn: aStream withDelimiter: self delimiter
end!

def numberNodeOn: aStream 
	^self numberNodeOn: aStream withDelimiter: self delimiter
end!

def stringNodeOn: aStream 
	^self stringNodeOn: aStream withDelimiter: self delimiter
end!

def timeNodeOn: aStream 
	^self timeNodeOn: aStream withDelimiter: self delimiter
end!

def timestampNodeOn: aStream 
	^self timestampNodeOn: aStream withDelimiter: self delimiter
end!

def titleNodeOn: aStream 
	^self titleNodeOn: aStream withDelimiter: self delimiter
end! !

!FileParser methodsFor: 'accessing.nodes.delemeted'!

def booleanNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Boolean
		delimiter: aDelimiter
end!

def characterNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Character
		delimiter: aDelimiter
end!

def dateNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Date
		delimiter: aDelimiter
end!

def integerNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Integer
		delimiter: aDelimiter
end!

def moneyNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Money
		delimiter: aDelimiter
end!

def nilNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: nil
		delimiter: aDelimiter
end!

def numberNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Number
		delimiter: aDelimiter
end!

def stringNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #String
		delimiter: aDelimiter
end!

def timeNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Time
		delimiter: aDelimiter
end!

def timestampNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Timestamp
		delimiter: aDelimiter
end!

def titleNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Title
		delimiter: aDelimiter
end! !

!FileParser methodsFor: 'accessing.nodes.fixedWidth'!

def booleanNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Boolean
		width: aWidth
end!

def characterNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Character
		width: aWidth
end!

def dateNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Date
		width: aWidth
end!

def integerNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Integer
		width: aWidth
end!

def moneyNodeOn: aStream withWidth: aWidth 

	^StreamBasicParseNode
		on: aStream
		forType: #Money
		width: aWidth
end!

def nilNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: nil
		width: aWidth
end!

def stringNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #String
		width: aWidth
end!

def timestampNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Timestamp
		width: aWidth
end!

def titleNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Title
		width: aWidth
end! !

!FileParser methodsFor: 'actions'!

def asTransactionDo: block0 

	^block0 value
end!

def commit
	"do nothing"
end!

def incrementRecordsDoneCountBy: aValue
	recordsDoneCount := recordsDoneCount + aValue
end!

def parseFile
	"Establish the transaction, parse the file, and answer the registry of 
	unsatisfied requests."

	Cursor wait showWhile: 
		[self
			preParseFile;
			parseStreamOn: self fileName;
			postParseFile].
	self displayParsingReport.

	^self
end!

def parseStreamOn: aFileName 
	"Create a stream on the file and parse it."

	| myClientCreator stream |
	(self context) 
		newLineTraceWith: 'Parsing file for ' , self clientTypeDescriptor; 
		newLineTraceWith: 'beginning...'.

	stream := aFileName readStream.
	myClientCreator := self postInitializedRootNodeOn: stream.
	[self 
			preParseStream: stream;
			parseStream: stream usingClientCreator: myClientCreator] 
		valueNowOrOnUnwindDo: [self postParseStream: stream]
end! !

!FileParser methodsFor: 'initialize-release'!

def initialize
	super initialize.
	registry := IdentityDictionary new.
	registrar := self objectNotFoundRegistrarFor: registry.
	recordCount := nil.
	recordsDoneCount := nil
end!

def postInitializeNewDomainObject: aDomainObject
	"Do whatever is necessary to further prepare objects 
	that have been read from the file.  This is a good place 
	to set any constant values in the domain object."

	^aDomainObject
end!

def preInitializeNewDomainObject: aDomainObject 
	"Do whatever is necessary to prepare objects 
	whose attributes will be read from the file."

	^aDomainObject
end! !

!FileParser methodsFor: 'override.accessing'!

def debugCycleClientCount
	"Answer the number of instances that should be created between 
	debug progress notifications.  This number should be proportional 
	to the time it takes to completely process a typical record in the file,   
	and **it should be an even factor of #maxInMemoryClientCount.**  
	As its purpose is to assure the user that progress is being made, the 
	higher it is set the longer an interested observer will be made to wait.  
	As screen writes are not cheap, if set too low the display update will 
	consume too much of the processing, and skew the performance 
	statistics."
	"| i |
	i := self basicNew.
	(i maxInMemoryClientCount rem: i debugCycleClientCount) = 0"

	^self maxInMemoryClientCount / 5.0
end!

def debugTraceIncrement
	"Answer the number of instances that should be created between 
	debug progress notifications.  This number should be proportional 
	to the time it takes to completely process a typical record in the file,   
	and **it should be an even factor of #maxInMemoryClientCount.**  
	As its purpose is to assure the user that progress is being made, the 
	higher it is set the longer an interested observer will be made to wait.  
	As screen writes are not cheap, if set too low the display update will 
	consume too much of the processing, and skew the performance 
	statistics."
	"| i |
	i := self basicNew.
	(i maxInMemoryClientCount rem: i debugTraceIncrement) = 0"

	^self maxInMemoryClientCount / 5.0
end!

def maxInMemoryClientCount
	"Answer the total number of clients permitted to exist in memory.  This 
	number should be proportional to the size of a typical client.  When this 
	boundary is reached, the clients are committed and removed from memory.  
	If set too high there will not be enough memory to support all of the tasks 
	and garbage collection thrashing will occur, increasing the processing time.  
	If set too low the commits will happen more frequently than they need to, 
	increasing the processing time."

	^3000
end!

def rootNodeType

	^StreamDescriptionParseNode
end! !

!FileParser methodsFor: 'override.actions'!

def postParseFile
	"do nothing"
end!

def postParseStream: aStream 
	"do whatever work is postrequisite to parsing the file"

	aStream close
end!

def preParseFile
	"Do whatever work is prerequisite to parsing the file."

	self context initialize.
	startTimeSeconds := Time now asSeconds.
	recordCount := self class recordCount
end!

def preParseStream: aStream 
	"do whatever work is prerequisite to parsing the file"

	"do nothing"
end! !

!FileParser methodsFor: 'override.associations'!

def selectorNodeFor: aClass in: aDatabase 
	"Answer an object that can create new clients. Subclasses should override this to add attributes and 
	relations."

	^StreamPersistentObjectDescriptionNode on: aClass new database: aDatabase.
end!

def selectorNodeOn: aStream for: aClass using: anAttributeNameSymbol
	"Answer an object that can create new clients. Subclasses should override 	this to add attributes and 
	relations."

	^(self selectorNodeFor: aClass in: self database)
	  at: anAttributeNameSymbol put: (self stringNodeOn: aStream);
	yourself
end! !

!FileParser methodsFor: 'override.testing'!

def shouldUseDelimitedFormat
	"Answer whether the records in my file are delimited."

	^true
end! !

!FileParser methodsFor: 'printing'!

def displayParsingReport

	| processingTime str |
	processingTime := Time now subtractTime: (Time fromSeconds: self startTimeSeconds).
	str := String new writeStream.
	str nextPutAll: 'Completed in '.

	(TimestampPrintPolicy defaultInstance copy
		shortPolicyString: 'hh:mm')
		printShort: processingTime 
		on: str.

	str
		nextPutAll: ', throughput: ';
		print: (self recordCount * 60 / (processingTime asSeconds max: 1)) asFloat;
		nextPutAll: ' records/minute.'.
	self context 
		traceWith: str contents
		isNewLine: true
end!

def printTreeOn: aStream 
	"Print a textual description of the parse tree on aStream."

	^(self rootNodeOn: nil)
		printTreeOn: aStream
end! !

!FileParser methodsFor: 'private'!

def parseStream: aStream usingClientCreator: aCreator 
	"\OWNERSHIP:	Copyright (C) 1996, 1997 P.G. Schaaf. All Rights Reserved."

	| debugCycleClientCount debugCyclesPerCommit |
	debugCycleClientCount := self debugCycleClientCount.
	debugCyclesPerCommit := (self maxInMemoryClientCount / debugCycleClientCount) floor max: 1.
	
	[debugCyclesPerCommit timesRepeat: 
		[debugCycleClientCount timesRepeat: 
			[aStream atEnd
				ifTrue: [^self commit; yourself]
				ifFalse: [aCreator value]]].
		self commit] repeat
end! !

!FileParser methodsFor: 'private.accessing'!

def postInitializedRootNodeOn: aStream 
	"Answer an object that can create new clients. Subclasses should 
	override this to add attributes and relations."

	| debugBlock |
	debugBlock := self debuggingBlock.
	^StreamTranslationParseNode 
		on: (self rootNodeOn: aStream)
		translator: [:client | 
			self postInitializeNewDomainObject: client.
			debugBlock value.
			client]
end!

def preInitializedRootNodeOn: aStream 
	"Answer an object that can create new clients. Subclasses should 
	override this to add attributes and relations."

	^StreamTranslationParseNode 
		on: self clientCreator
		translator: [:client | 
			self preInitializeNewDomainObject: client.
			client]
end!

def rootNodeOn: aStream 
	"Answer an object that can create new clients. Subclasses should override 
	this to add attributes and relations."
	"\OWNERSHIP:	Copyright (C) 1996, 1997 P.G. Schaaf. All Rights Reserved."

	^self rootNodeType on: (self preInitializedRootNodeOn: aStream)
end! !

FileParser class
	instanceVariableNames: ''!

!FileParser class methodsFor: 'accessing'!

def attributeCount
	"Answer the number of attributes contained in each record 
	of the file.  This is only used by the utilities and not for 
	parsing (where this value is defined implicitly).  Utilities 
	dependent upon file structure will not work properly on 
	files whose records have a variable number of attributes."
	"Check whether the expected attributeCount equals the 
	attributeCount described by the parse tree defined by an 
	instance of this class."
	"(self attributeCount) = (self new rootNodeOn: attributeCount)"

	self subclassResponsibility
end!

def clientClass
	"Answer the class that I should use to create instances."
	"Concrete subclasses should answer a class, Abstract 
	subclasses should answer nil."

	^nil
end!

def clientClassToParserMapping
	"Answer a mapping from the client classes to their FileParsers."
	"self clientClassToParserMapping"

	| dict |
	dict := IdentityDictionary new: 10.
	self allSubclasses do: 
		[:each | 
		| key |
		(key := each clientClass) == nil ifFalse: [dict at: key put: each]].
	^dict
end!

def clientCreator
	"Answer a block that can create new instances 
	of my client class."

	| myClientClass |
	myClientClass := self clientClass.
	^[myClientClass new]
end!

def context
	"Answer the context in which my calculations should occur."

	^FileImportParserContext current
end!

def currentUserID
	"Answer the login ID of the current user."

	^self context currentUserID
end!

def defaultFileDrive
	"Answer the default for the name of the drive 
	in which I should look for my file.  This method 
	should be patched to include drive letter info for 
	new users."
	"self defaultFileDrive"

	| userToDriveLetterMapping driveLetter |
	userToDriveLetterMapping := Dictionary new
		at: #G00046 put: $i;
		at: #E16718 put: $j;
		yourself.
	driveLetter := userToDriveLetterMapping 
		at: self currentUserID
		ifAbsent: $c.
	^String 
		with: driveLetter 
		with: $:
end!

def defaultFileName
	"Answer the default name of the file that I should parse.
	This should only include path information that distinguishes 
	it from the other files that are being processed."
	"self defaultFileSpecification"
	"self editFile"

	^self defaultFilePath
end!

def defaultFilePath
	"Answer the default for the name of the directory 
	in which I should look for my file.  This should 
	begin and end with a directory separator and 
	should not include the file name."

	^self defaultFileDrive, ''
end!

def defaultToTraceOn
	"Answer whether parses for my instances should trace their output."

	^true
end!

def delimiter
	"Answer the character(s) used to delimit the 
	attributes used by my file."

	^Character cr
end!

def fileName
	"Answer an instance of Filename that points to my file."
	"CIAccountParser fileName"

	| fileName |
	fileName := self defaultFileName.
	
	[fileName := fileName asFilename.
	fileName exists
		ifTrue: [^fileName]
		ifFalse: [fileName := self requestFileSpecFromUser: fileName].
	fileName size == 0 ifTrue: [^nil]] 
		repeat
end!

def fileNotFoundSignal

	^OSErrorHolder nonexistentSignal
end!

def requestFileSpecFromUser: aSpec 
	"Answer a string as supplied by the user."
	"self requestFileSpecFromUser: 'c:\nowhere'"

	^Dialog 
		requestFileName: 'Where can I find the file representing:\' withCRs , self clientClass asString , 's?' 
		default: aSpec
end! !

!FileParser class methodsFor: 'actions'!

def parseFile
	"Parse the file using the default trace mode."

	^self parseFileTracing: self defaultToTraceOn
end!

def parseFileTracing: aBoolean

	^aBoolean 
		ifTrue: [self parseFileWithTracing]
		ifFalse: [self parseFileWithoutTracing]
end!

def parseFileWithoutTracing

	^self context doNotEvaluateDebugCodeWhile: [self new parseFile]
end!

def parseFileWithTracing
	^self context evaluateDebugCodeWhile: [self new parseFile]
end!

def printTree

	| str |
	str := String new writeStream.
	self printTreeOn: str.
	^str contents
end!

def printTreeOn: aStream
	"Transcript clear.
	CIAddressParser printTreeOn: Transcript.
	Transcript endEntry"

	self context doNotEvaluateDebugCodeWhile: 
		[self new printTreeOn: aStream]
end!

def traceTree

	self printTreeOn: Transcript.
	Transcript endEntry
end! !

!FileParser class methodsFor: 'instance creation'!

def new
	^self basicNew initialize
end! !

!FileParser class methodsFor: 'utility'!

def editFile
	"Open, at the user's discretion, either a file editor on the entire contents 
	or a workspace on a copy of a subset of the contents of my file."

	| file fileSize largeFileSizeLowerBound |
	largeFileSizeLowerBound := 50000.
	file := self fileName.
	file exists 
		ifFalse: [
			self error: file asString, 
				' does not exist.\' withCRs, 
				'Proceed to create it.'
		].
	(fileSize := file fileSize) > largeFileSizeLowerBound
		ifTrue: [
			(Dialog confirm: 
				'The file is ', 	fileSize asString,
				' bytes.  View the entire file?'
			)
				ifTrue: [file edit]
				ifFalse: [self editSampleFromFile: file]
		]
		ifFalse: [file edit]
end!

def editSampleFromFile: aFile
	"Open a workspace on a sample taken from my file."

	ComposedTextView 
		open: (self fileSampleFrom: aFile) contents asValue
		label: self clientClass name, '--', aFile asString
end!

def fileSample
	"Answer a sample from my file."

	^self fileSampleFrom: self fileName
end!

def fileSampleFrom: aFile
	"Answer a sample taken from aFile."

	^self 
		fileSampleFrom: aFile 
		recordCount: 50
end!

def fileSampleFrom: aFile recordCount: recordCount 
	"Answer a sample taken from aFile that includes recordCount complete records."

	| aStream file delimiter attributesRequestedCount |
	delimiter := self delimiter.
	attributesRequestedCount := recordCount * self attributeCount.
	aStream := ReadWriteStream on: (String new: attributesRequestedCount).
	file := aFile readStream.
	[attributesRequestedCount timesRepeat: [aStream nextPutAll: (file through: delimiter)]]
		valueNowOrOnUnwindDo: [file close].
	^aStream reset; yourself
end!

def recordCount
	"Answer the number of records in my file."

	^Object subclassResponsibilitySignal 
		handle: [:ex | ex returnWith: 0]
		do: [self recordCountIn: self fileName]
end!

def recordCountIn: aFile
	"Answer the number of records in aFile.  Note that for 
	an accurate number to be answered all records in the 
	file must have the same number of attributes."
	"CIAddressParser editFile"
	"CIAddressParser recordCount"
	"CIProductItemParser recordCount"

	| cr fieldCount |
	cr := Character cr.
	fieldCount := 0.
	aFile readStream do: [:each | 
		each == cr 
			ifTrue: [fieldCount := fieldCount + 1]
	].
	^(fieldCount * 1.0 / self attributeCount) ceiling
end! !

!FileParser class methodsFor: 'utility.batch processing'!

def batchProcessJobListFromUser
	"self batchProcessJobListFromUser"

	| batchList start |
	batchList := self batchProcessJobList copy.
	start := Dialog 
		choose: 'Begin processing at which level?' 
		fromList: (batchList collect: [:each | each name])
		values: batchList
		lines: 8 
		cancel: [^OrderedCollection new].
	[batchList first == start] whileFalse: [batchList removeFirst].
	^batchList
end!

def process: aJob 
	aJob ifDefined: [:job | job parseFileTracing: true]
end!

def processBatch
	"self processBatch"

	self processBatch: self batchProcessJobList
end!

def processBatch: aList

	aList do: [:each | each parseFileTracing: true]
end!

def processBatchFromUser
	"Allows the user to begin the batch processing at 
	any point in an ordered list of parsing jobs."
	"self processBatchFromUser"

	self processBatch: self batchProcessJobListFromUser
end!

def processBatchThenQuit
	"self processBatchThenQuit"

	| list |
	list := self batchProcessJobList.
	list isEmpty 
		ifFalse: [self processBatchThenQuit: list]
end!

def processBatchThenQuit: aBatch
	"self processBatchThenQuit"

	[Object errorSignal 
		handle: 
			[:ex | 
			| errorString |
			errorString := ex = VError errorSignal
				ifTrue: [VError description]
				ifFalse: [ex errorString].
			(FileParser context) 
				newLineTraceWith: 'EXCEPTION: ' , errorString; 
				rollback]
		do: [self processBatch: aBatch]] valueNowOrOnUnwindDo: 
			[FileParser context endSession.
			ObjectMemory quit]
end!

def processBatchThenQuitFromUser
	"Allows the user to begin the batch processing at 
	any point in an ordered list of parsing jobs. End the 
	session and quit the image (without saving) after all 
	jobs are completed. If at any point there is an un-
	handled exception, end job processing, log the ex-
	ception to the current log file, rollback, and then end 
	the session and quit without saving."
	"self processBatchThenQuitFromUser"

	| list |
	list := self batchProcessJobListFromUser.
	list isEmpty ifFalse: [self processBatchThenQuit: list]
end!

def processFromUser
	"Allows the user to select from an ordered list of parsing jobs."
	"self processBatchFromUser"

	self process: (self batchProcessJobListFromUser at: 1 ifAbsent: nil)
end! !

!FileParser class methodsFor: 'utility.session'!

def beginOLSession
	^self context beginOLSession
end!

def beginSession
	^self context beginSession
end!

def commit
	^self context commit
end!

def connectToDatabaseFromUser
	"self connectToDatabaseFromUser"

	| choice list |
	list := self favoriteDBList 
		remove: self context database ifAbsent: nil;
		yourself.
	choice := Dialog
				choose: 'Connect to which database?'
				fromList: list
				values: list
				buttons: #('other...')
				values: (Array with: [self databaseNameFromUserIfNone: []])
				lines: 8
				cancel: [nil].
	^(choice := choice value) isNil
		ifTrue: [false]
		ifFalse: [self context connectToDatabase: choice]
end!

def databaseNameFromUserIfNone: block0
	"self databaseNameFromUserIfNone: [nil]"

	| choice |
	choice := Dialog request: 'Connect to what database (bprsvr4\is the default server)?' withCRs.
	^(choice includes: $@)
		ifTrue: [choice]
		ifFalse: 
			[choice isEmpty
				ifTrue: [block0 value]
				ifFalse: [choice := choice , '@bprsvr4']]
end!

def disconnectFromDatabaseFromUser
	"self disconnectFromDatabaseFromUser"

	| connectedDatabases choice |
	connectedDatabases := self context connectedDatabases asSet asOrderedCollection.
	connectedDatabases isEmpty ifTrue: [^nil].
	choice := Dialog 
		choose: 'Disconnect from which database?' 
		fromList: connectedDatabases
		values: connectedDatabases 
		lines: 8 
		cancel: [nil].
	^choice isNil
		ifTrue: [nil]
		ifFalse: [self context disconnectFromDatabase: choice]
end!

def endSession
	^self context endSession
end!

def favoriteDBList
	^(OrderedCollection new) 
		add: 'chuckdb@bprsvr4'; 
		add: 'pauldb@bprsvr4'; 
		add: 'paulrefdb@bprsvr4'; 
		add: 'refdb1016@bprsvr4';
		yourself
end!

def rollback
	^self context rollback
end! !

!FileParser class methodsFor: 'utility.transaction management'!

def manageInSessionTransactionFromUser
	"self manageInSessionTransactionFromUser"

	| choice options |
	options := #(#connectToDatabaseFromUser #rollback #commit #endSession).
	self context connectedDatabases isEmpty
		ifFalse: [options := options copyWith: #disconnectFromDatabaseFromUser].
	choice := Dialog
				choose: 'Session database: ' , self context database printString
				fromList: (options collect: [:each | each asString])
				values: options
				lines: 5
				cancel: [nil].
	^choice isNil
		ifTrue: [nil]
		ifFalse: [self perform: choice]
end!

def manageNotInSessionTransactionFromUser
	"self manageNotInSessionTransactionFromUser"

	| choice options |
	options := #(#beginSession #beginOLSession #cancel).
	choice := Dialog
				choose: 'Default database: ' , self context database printString
				labels: (options collect: [:each | each asString])
				values: options
				default: #cancel.
	^choice == #cancel
		ifTrue: [nil]
		ifFalse: [self perform: choice]
end!

def manageTransactionFromUser
	"Allow the user to perform a variety of context-sensitive 
	session management options."
	"self manageTransactionFromUser"

	^self context isInSession
		ifTrue: [self manageInSessionTransactionFromUser]
		ifFalse: [self manageNotInSessionTransactionFromUser]
end! !

FileParser subclass: #VersantFileParser
	instanceVariableNames: ':database, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!VersantFileParser methodsFor: 'accessing'!

def persistentObjectNotFoundRegistrarFor: aRegistry 
	"Answer an object that can track requests of the 
	persistent store that come up empty."

	^[:aSummary | 
	| client |
	client := aSummary client.
	(aRegistry at: client class ifAbsentPut: IdentityDictionary new)
		at: client identityHash put: aSummary predicate]
end!

def referencedClasses
	"Answer all of the classes that are directly referenced during this parsing."

	^self classToRequiredDatabaseMapping keys
end!

def supplementaryDatabases
	"Answer all of the databases that are used during this parsing."

	^self classToRequiredDatabaseMapping values
end! !

!VersantFileParser methodsFor: 'actions'!

def asTransactionDo: block0 

	^self context asTransactionDo: 
		[self context 
			connectToDatabases: self supplementaryDatabases 
			while: block0]
end!

def commit
	| myContext |
	myContext := self context.
	myContext persistAllNonPersistentObjects.
	self handleNonOLReadyClassWhile: [myContext commit].
	self context debugIsOn 
		ifTrue: 
			[self displayDatabaseToSizeMap.
			self context newLineTraceWith: 'Estimated completion time: ', self estimatedCompletionTime asString]
end!

def databaseToSizeMap
	| myContext map |
	myContext := self context.
	map := Dictionary new.
	((myContext connectedDatabases asSet) 
		add: self database; 
		yourself) do: 
		[:each | 
		map 
			at: each 
			put: (myContext databaseSize: each)].
	^map
end!

def handleNonOLReadyClassWhile: doBlock0 
	self
		handleNonOLReadyClassWith: 
			[:ex | 
			| aClass aDatabase |
			aClass := ex parameter key.
			aDatabase := ex parameter value.
			nil halt.
			(aClass turnOnOLReadyIn: aDatabase)
				ifTrue: [ex restart]
				ifFalse: [ex reject]]
		do: doBlock0
end!

def handleNonOLReadyClassWith: handleBlock do: doBlock 
	^VError errorSignal 
		handle: 
			[:ex | 
			(VError errorSymbol == #VSI_ERROR and: ['* has different OL-ready status.' match: VError description])
				ifTrue: [
					| aClass aDatabase aStream |
					"This is a wicked hack but I know no other way of getting 
					this information without changing the whole Versant error process."
					aStream := VError description readStream.
					aStream skip: 'Database class ' size.
					aClass := Smalltalk at: (aStream upToAll: ' in database ') asSymbol.
					aStream skip: ' in database ' size.
					aDatabase := aStream upTo: Character space.
					ex parameter: aClass->aDatabase.
					handleBlock value: ex]
				ifFalse: [ex reject]]
		do: doBlock
end!

def parseFile
	"\OWNERSHIP:	Copyright (C) 1996 P.G. Schaaf. All Rights Reserved."

	| result |
	result := self asTransactionDo: [super parseFile].
	ObjectMemory garbageCollect.
	^result
end!

def syncClassDefinitionsToDatabaseMappings
	"For each of the classes used in this parsing, set its specific 
	database attribute to that database, if necessary."

	| commitAfterward |
	commitAfterward := false.
	self classToRequiredDatabaseMapping keysAndValuesDo: 
		[:aClass :db | 
		(aClass isDefinedOn: db) ifFalse: 
			[self context 
				do: [aClass describeToOdb: db]
				newLineTraceWith: 'Describing ' , aClass name , ' to ' , db.
			commitAfterward := true]].
	commitAfterward ifTrue: [self context commit]
end! !

!VersantFileParser methodsFor: 'initialize-release'!

def initialize
	super initialize.
	database := self context database
end!

def postInitializeNewDomainObject: aDomainObject
	"Do whatever is necessary to further prepare objects 
	that have been read from the file.  This is a good place 
	to set any constant values in the domain object.  If you 
	override this in the subclass, make sure to evaluate this 
	method after you evaluate the new one."

	^self context 
		makePersistent: aDomainObject 
		in: self database
end!

def preInitializeNewDomainObject: aDomainObject 
	"Do whatever is necessary to prepare objects 
	whose attributes will be read from the file."

	super preInitializeNewDomainObject: aDomainObject.
	^self context addPrePersistentObject: aDomainObject
end! !

!VersantFileParser methodsFor: 'override.accessing'!

def classToRequiredDatabaseMapping
	"Answer a mapping from each of the classes referred to by this node 
	to the databases in which they can be found."
	"Subclasses should extend this."

	| dict |
	dict := IdentityDictionary new.
	self clientClass ifDefined: [:aClass | dict at: aClass put: self database].
	^dict
end!

def database
	"Answer the database to be used for read and write operations on 
	instances of my client class."

	^self context database
end!

def rootNodeOn: aStream for: aFileParser
	"Answer an object that can create new clients."

	| rootNode |
	rootNode := (super rootNodeOn: aStream).
	aFileParser aspects
	do:
		[:each|
		rootNode at: each ident asSymbol put:
			(each parserNode on: aStream
			forType: each type
			delimiter: each delimiter).
		].
	^rootNode
end! !

!VersantFileParser methodsFor: 'override.actions'!

def preParseStream: aStream 
	"do whatever work is prerequisite to parsing the file"

	super preParseStream: aStream.
	self syncClassDefinitionsToDatabaseMappings
end! !

!VersantFileParser methodsFor: 'printing'!

def displayDatabaseToSizeMap
	| myContext |
	myContext := self context.
	self databaseToSizeMap keysAndValuesDo: 
		[:name :size | myContext newLineTraceWith: name , ' size is ' , size asString]
end! !

!VersantFileParser methodsFor: 'private'!

def parseStream: aStream usingClientCreator: aCreator 
	"\OWNERSHIP:	Copyright (C) 1996 P.G. Schaaf. All Rights Reserved."

	self displayDatabaseToSizeMap.
	self context persistentObjectNotFoundSignal
		handle: 
			[:ex | 
			self registrar value: ex parameter.
			self context newLineTraceWith: 'Persistent object not found.'.
			ex proceed]
		do: [super parseStream: aStream usingClientCreator: aCreator]
end! !

VersantFileParser class
	instanceVariableNames: ''!

Object subclass: #PersistentObjectNotFoundSummary
	instanceVariableNames: ':client, :initializer, :predicate, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!PersistentObjectNotFoundSummary methodsFor: 'accessing'!

def client
	^client
end!

def client: aClient
	client := aClient
end!

def initializer
	^initializer
end!

def initializer: aValue 
	initializer := aValue
end!

def predicate
	^predicate
end!

def predicate: aValue 
	predicate := aValue
end! !

!PersistentObjectNotFoundSummary methodsFor: 'initialize-release'!

def initialize
end! !

PersistentObjectNotFoundSummary class
	instanceVariableNames: ''!

!PersistentObjectNotFoundSummary class methodsFor: 'instance creation'!

def client: aClient initializer: anInitializer predicate: aPredicate 
	^(self new) 
		client: aClient;
		initializer: anInitializer;
		predicate: aPredicate
end! !

'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 11, 2003 at 4:46:53 pm'!

"\FILED_OUT_USING: Harmony Source Code Management System-Release 1.0 beta 2"!

"\SUBJECT: + (2) <loc: 1998-03-12_10-09-23> [Module] dom.ParseNodes"!
"\COPYRIGHT: Copyright (C) 1998 Versant.  All Rights Reserved."!
"\FILED_OUT_BY: P.G. Schaaf"!
"\ENVIRONMENT: c:\Smalltlk\Harmony\paul"!
"\COMMENT: NONE"!

Object subclass: #StreamParseNode
	instanceVariableNames: ''
	classVariableNames: 'UnexpectedValueSignal '
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'A StreamParseNode is a machine that knows how to read and interpret data from the current position in its stream.  This process is initiated by asking the node for its #value.  Subclasses specialize this behavior to either apply a transformation to the data (such as mapping, interpretation, etc.), or to coordinate the component nodes that do the actual parsing.  
'
=end!

!StreamParseNode methodsFor: 'accessing'!

def attributeClass
	"Answer the class of the attribute wrapper that I require."

	^self component attributeClass
end!

def component
	"Answer the node that provides my input."

	self subclassResponsibility
end!

def components
	^OrderedCollection with: self component
end!

def context
	"Answer the context in which my calculations should occur."

	^ApplicationContext current
end!

def unexpectedValueSignal
	^self class unexpectedValueSignal
end!

def value
	"Answer the result of 1 computational cycle of my machinery."

	^self component value
end! !

!StreamParseNode methodsFor: 'converting'!

def asAttributeNamed: aName 
	"Answer myself wrapped with an attribute node."

	^self attributeClass 
		on: self 
		slotName: aName asSymbol
end! !

!StreamParseNode methodsFor: 'debugging'!

def asPostValueHaltNode
	"Answer myself wrapped with a node that will halt after I am evaluated."

	^StreamComponentParseNode on: [self value; halt; yourself]
end!

def asPostValueHaltNode: test1 
	"Answer myself wrapped with a node that will halt after I am evaluated 
	if test1 on myself is true."

	^StreamComponentParseNode
		on: 
			[| myValue |
			myValue := self value.
			(test1 value: myValue)
				ifTrue: [self halt].
			myValue]
end!

def asPreValueHaltNode
	"Answer myself wrapped by a node that will halt before I am evaluated."

	^StreamComponentParseNode on: [self halt value]
end!

def asPreValueHaltNode: block0 
	"Answer myself wrapped by a node that will halt 
	before I am evaluated if the value of block0 is true."

	^StreamComponentParseNode
		on: 
			[block0 value ifTrue: [nil halt].
			self value]
end! !

!StreamParseNode methodsFor: 'initialize-release'!

def initialize
	"do nothing"
end! !

!StreamParseNode methodsFor: 'inquiries'!

def attributeCount
	"Answer the numer of attributes of which I am composed."

	^self components 
		inject: 0 
		into: 
			[:count :each | each species == StreamParseNode
				ifTrue: [count + each attributeCount]
				ifFalse: [count]]
end!

def mapsDirectlyToFile
	^false
end!

def species
	^StreamParseNode
end! !

!StreamParseNode methodsFor: 'printing'!

def printOn: aStream level: aLevel

	aStream
		crtab: aLevel;
		print: self
end!

def printSubcomponents: subcomponents on: aStream level: aLevel

	aStream nextPutAll: ' ('.
	subcomponents
		do: [:each | 
			self 
				printValue: each 
				on: aStream 
				level: aLevel
		].
	(subcomponents size == 1 
		and: [subcomponents first species == self species
		and: [subcomponents first mapsDirectlyToFile]]
	)
		ifFalse: [aStream crtab: aLevel - 1].
	aStream nextPut: $)
end!

def printSubcomponentsOn: aStream level: aLevel

	| mySubcomponents |
	mySubcomponents := self components.
	mySubcomponents isEmpty
		ifFalse: [
			self 
				printSubcomponents: mySubcomponents
				on: aStream 
				level: aLevel
		]
end!

def printTree

	| str |
	str := WriteStream on: String new.
	self printTreeOn: str.
	^str contents
end!

def printTreeOn: aStream 

	self 
		printTreeOn: aStream 
		level: 0
end!

def printTreeOn: aStream level: aLevel

	self 
		printOn: aStream 
			level: aLevel;
		printSubcomponentsOn: aStream
			level: aLevel + 1
end!

def printValue: aValue on: aStream level: aLevel 

	aValue species == self species
		ifTrue: [
			aValue 
				printTreeOn: aStream 
				level: aLevel
		]
		ifFalse: [
			aStream 
				crtab: aLevel; 
				nextPutAll: aValue asString
		]
end!

def traceTree
	self printTreeOn: Transcript.
	Transcript endEntry
end! !

StreamParseNode class
	instanceVariableNames: ''!

!StreamParseNode class methodsFor: 'accessing'!

def dataType
	^#Object
end!

def unexpectedValueSignal
	"Answer the signal used when the value found in my
	text field is somehow unexpected."

	UnexpectedValueSignal == nil
		ifTrue: [self initializeSignals].
	^UnexpectedValueSignal
end! !

!StreamParseNode class methodsFor: 'class initialization'!

def initialize
	"self initialize"

	self initializeSignals
end!

def initializeSignals
	UnexpectedValueSignal := (Object errorSignal newSignalMayProceed: true)
		notifierString: 'An unexpected value has been found in this field';
		nameClass: self message: #unexpectedValueErrorSignal
end! !

!StreamParseNode class methodsFor: 'instance creation'!

def new
	^self basicNew initialize
end! !

StreamParseNode subclass: #StreamComponentParseNode
	instanceVariableNames: ':component, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me are used to make a non-node object appear to be a node (a la the Decorator pattern).  If you implement

	Object>>value
		^self

then you can use the number 1234 as if it were a parse node by wrapping it with a value node:

	a := StreamComponentParseNode
		on: 1234

Notice that ''a'' is a parse node whose value is constant.  

If you do not implement Object>>value, then the implementation of ''a'' would be: 

	a := StreamComponentParseNode
		on: [1234]'
=end!

!StreamComponentParseNode methodsFor: 'accessing'!

def component
	"Answer the node that provides my input."

	^component
end!

def component: aValue
	component := aValue
end! !

!StreamComponentParseNode methodsFor: 'initialize-release'!

def on: aValue
	self component: aValue
end! !

StreamComponentParseNode class
	instanceVariableNames: ''!

!StreamComponentParseNode class methodsFor: 'instance creation'!

def on: aValue
	^self new on: aValue
end! !

StreamComponentParseNode subclass: #StreamLoopFeedbackParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'This node replaces its component with the value of its component whenever it is asked for its value (i.e. it uses its own output from invocation n as its input on invocation n+1).  For example:  
	If we evaluate the following:
		node := StreamLoopFeedbackParseNode on: [ [ ''hello'' ] ].
		component0 := node component.		"= [ [ ''hello'' ] ], no change yet"

		value1 := node value.
		"= [ ''hello'' ], the outermost block has been evaluated and the result 
		placed in component and answered"
		component1 := node component.			"value1 == component1"

		value2 := node value.
		"= ''hello'', the outermost block has been evaluated and the result 
		placed in component and answered"
		component2 := node component.			"value2 == component2"

		value3 := node value.
		"= ''hello'', the string has been evaluated (it answers itself) and the result 
		placed in component and answered"
		component3 := node component.			"value3 == component3"

		"We could continue this forever, but `node value` would always equal ''hello'', 
		because `''hello'' value` equals ''hello''. "

A more typical use of this node would be for deferred computation of a constant value as such:

	node := StreamLoopFeedbackParseNode on: (self computePi)

The first time this is evaluated, pi is computed and then answered.  All subsequent times it is simply answered directly without being re-computed.
'
=end!

!StreamLoopFeedbackParseNode methodsFor: 'accessing'!

def value
	"Answer the value that I represent, and recycle that value."

	| myValue |
	myValue := super value.
	self component: myValue.
	^myValue
end! !

StreamLoopFeedbackParseNode class
	instanceVariableNames: ''!

!StreamLoopFeedbackParseNode class methodsFor: 'documentation'!

def testNode
	"self testNode"
	"| node oc |
	node := self testNode.
	oc := OrderedCollection new.
	5 timesRepeat: [oc add: node value].
	oc"

	^self on: [ [7 asValue] asValue ]
end! !

StreamComponentParseNode subclass: #StreamTranslationParseNode
	instanceVariableNames: ':translator, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me answer the value of their component after having applied a transformation function.  For instance, my owner wishes to know the name of the person answered by another node.  Assuming that personNode is a node whose value is a person, create the translator as follows: 

	StreamTranslationParseNode 
		on: (personNode)
		translator: [:aPerson | aPerson name]

This translator is passed the output of the component as a parameter, but it need not do anything with that value.  The following node evaluates personNode but discards the result.  When the node is evaluated, it always answers ''John''.  

	StreamTranslationParseNode 
		on: (personNode)
		translator: [:aPerson | ''John'']

Although this is permitted (mainly because I have no means of detecting it) it is not recommended style.  Why do the work that personNode represents if it is simply to be thrown away?  

It is possible to use Translation nodes to simulate Branch and Enumeration nodes--just as it is possible to use a screwdriver as a hammer.  In both situations you are far better off to use the more specialized tool.  '
=end!

!StreamTranslationParseNode methodsFor: 'accessing'!

def translationOf: aValue ifNone: block0
	^block0 value
end!

def translator
	^translator
end!

def translator: block1
	translator := block1
end!

def value
	"Answer the translation of my super value."

	| myValue |
	myValue := super value.
	^self 
		translationOf: myValue 
		ifNone: [self translator value: myValue]
end! !

!StreamTranslationParseNode methodsFor: 'initialize-release'!

def initialize
	super initialize.
	translator := [:client | client]
end! !

StreamTranslationParseNode class
	instanceVariableNames: ''!

!StreamTranslationParseNode class methodsFor: 'instance creation'!

def on: aGenerator translator: block1 
	^(self on: aGenerator)
		translator: block1
end! !

StreamTranslationParseNode subclass: #StreamEnumeratedTypeParseNode
	instanceVariableNames: ':conditions, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me maintain an enumeration of permissible values.  When an instance is evaluated, it evaluates its component and tests to see if that value is in the enumeration.  If it is, answer it.  If it is not, answer the value of ifNoneBlock.

	StreamEnumeratedTypeParseNode
		on: (integerNode)
		enumeration: (1 to: 75)
		ifNone: [:value | Dialog warn: value asString, '' is not in the permissible range!!'']

If the ifNone block is not provided, the default one is used, which raises the unexpectedValueSignal.  You will usually not want to override this.  Instead, handle the signal and process it appropriately.  If you just open a Dialog (as above) the whole parsing process will be suspended until the user acknowledges the dialog.  '
=end!

!StreamEnumeratedTypeParseNode methodsFor: 'accessing'!

def conditions
	^conditions
end!

def conditions: aValue 
	conditions := aValue
end!

def ifNone
	^self translator
end!

def ifNone: aBlock
	self translator: aBlock
end!

def translationOf: aValue ifNone: block0

	^(self conditions includes: aValue)
		ifTrue: [aValue]
		ifFalse: [block0 value]
end! !

!StreamEnumeratedTypeParseNode methodsFor: 'initialize-release'!

def initialize
	super initialize.
	self ifNone: [:key | self unexpectedValueSignal raiseWith: key]
end! !

!StreamEnumeratedTypeParseNode methodsFor: 'printing'!

def printOn: aStream level: aLevel

	"do nothing"
end!

def printSubcomponentsOn: aStream level: aLevel

	| wstr |
	wstr := String new writeStream.
	wstr nextPut: ${.
	self conditions
		do: [:each | each printOn: wstr]
		separatedBy: [wstr space].
	wstr nextPut: $}.

	self 
		printValue: wstr contents
		on: aStream
		level: aLevel
end! !

StreamEnumeratedTypeParseNode class
	instanceVariableNames: ''!

!StreamEnumeratedTypeParseNode class methodsFor: 'instance creation'!

def on: aNode enumeration: aCollection

	^(self on: aNode)
		conditions: aCollection
end!

def on: aNode enumeration: aCollection ifNone: block1 
	^(self on: aNode enumeration: aCollection)
		ifNone: block1
end! !

StreamEnumeratedTypeParseNode subclass: #StreamBranchParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'My instances serve as a case statement.  They first evaluate their keyNode, search their testConditions for the *first* one that that value satisfies, and answer the value of the corresponding branchNode.  If the value fails every testCondition, evaluate ifNoneBlock with the value as an argument.  

Do not use an instance of me to constrain the permissible input values to an enumerated set, use a StreamEnumeratedTypeNode instead.  '
=end!

!StreamBranchParseNode methodsFor: 'accessing'!

def components
	^super components 
		addAll: self values;
		yourself
end!

def keys

	^self conditions collect: [:each | each key]
end!

def translationOf: aValue ifNone: block0 
	"Answer the parsedValue of the node whose test condition 
	is satisfied by my parsedValue."

	^(self conditions
		detect: [:cond | cond key value: aValue]
		ifNone: [^block0 value]) value value
end!

def values

	^self conditions collect: [:each | each value]
end! !

!StreamBranchParseNode methodsFor: 'actions'!

def canUseEquivalenceTestOn: aValue

	^aValue isCharacter 
		or: [aValue isSmallInteger
		or: [aValue == true
		or: [aValue == false
		or: [aValue isNil
		or: [aValue isSymbol]]]]]
end!

def mapIfEquals: aValue to: aNode 

	| satisfactionBlock |
	satisfactionBlock := aValue isImmediateValue
		ifTrue: [ [:parsedValue | aValue == parsedValue] ]
		ifFalse: [ [:parsedValue | aValue = parsedValue] ].

	self 
		mapIfSatisfies: satisfactionBlock 
		to: aNode
end!

def mapIfSatisfies: block1 to: aNode 
	conditions add: block1 -> aNode
end! !

!StreamBranchParseNode methodsFor: 'initialize-release'!

def initialize

	super initialize.
	conditions := OrderedCollection new
end! !

!StreamBranchParseNode methodsFor: 'inquiries'!

def attributeCount
	"Answer the count of attributes of which I am composed.  This 
	number will be incorrect if the number of nodes in each branch 
	differs."

	^(OrderedCollection 
		with: self component
		with: self conditions someElement key
	)
		inject: 0
		into: [:count :each |
			each species == StreamParseNode 
				ifTrue: [count + each attributeCount]
				ifFalse: [count]
		]
end!

def expectedValues
	^self conditions asArray
		collect: [:each | each copiedValues]
end! !

!StreamBranchParseNode methodsFor: 'printing'!

def printOn: aStream level: aLevel

	self
		printValue: 'Branch: '
			on: aStream 
			level: aLevel;
		printSubcomponents: (Array with: self component)
			on: aStream 
			level: aLevel + 1;
		printValue: 'Values: '
			on: aStream 
			level: aLevel
end!

def printSubcomponentsOn: aStream level: aLevel

	self
		printSubcomponents: self values
			on: aStream 
			level: aLevel
end! !

StreamBranchParseNode class
	instanceVariableNames: ''!

!StreamBranchParseNode class methodsFor: 'instance creation'!

def ifNone: block1 
	^self new ifNone: block1
end!

def on: aValue ifNone: block1 

	^(self on: aValue)
		ifNone: block1
end! !

StreamComponentParseNode subclass: #StreamCompositionParseNode
	instanceVariableNames: ':components, :valueBlock, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Do not use this in place of a StreamDescriptionParseNode, StreamTranslationParseNode, or a StreamBranchParseNode.  

Instances of me contain a collection of component nodes and a valueBlock whose argument count is the number of nodes.  When an instance is evaluated it evaluates each of the components and then evaluates the block using the component values as parameters.  The value answered by my instance is the value answered by its block.  

This node is polymorphic with the others in the sense that it has a component node (established using #on:).  The only distinction between the *component* and the *components* is that the component is passed into the valueBlock as the first argument.  You may choose to distinguish the component in the valueBlock, but this node does not care.  '
=end!

!StreamCompositionParseNode methodsFor: 'accessing'!

def add: aComponent 
	"Add a component node to my collection."

	^self components add: aComponent
end!

def components
	^components
end!

def components: aCollection
	components := aCollection
end!

def value
	"Answer the value of my valueBlock evaluated with 
	each of my components' values."

	| str |
	str := (Array new: (self components size + 1)) writeStream.
	str nextPut: self component value.
	self components do: [:each | str nextPut: each value].
	^self valueBlock valueWithArguments: str contents
end!

def valueBlock
	^valueBlock
end!

def valueBlock: aBlock
	valueBlock := aBlock
end! !

!StreamCompositionParseNode methodsFor: 'initialize-release'!

def initialize
	super initialize.
	components := OrderedCollection new.
	valueBlock := []
end! !

StreamCompositionParseNode class
	instanceVariableNames: ''!

!StreamCompositionParseNode class methodsFor: 'instance creation'!

def on: one with: two

	^(self on: one)
		add: two;
		yourself
end!

def on: one with: two with: three

	^(self on: one)
		add: two;
		add: three;
		yourself
end!

def on: one with: two with: three with: four

	^(self on: one)
		add: two;
		add: three;
		add: four;
		yourself
end! !

StreamComponentParseNode subclass: #StreamDescriptionParseNode
	instanceVariableNames: ':attributes, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'A StreamDescriptionParseNode takes the object answered by the evaluation of its component, and makes that object conform to the description given by the collection of attribute nodes.  The initialized object is answered as a result.'
=end!

!StreamDescriptionParseNode methodsFor: 'accessing'!

def attributes
	^attributes
end!

def attributes: aValue
	attributes := aValue
end!

def components
	^super components 
		addAll: self attributes;
		yourself
end!

def value
	^self processNewComponent: super value
end! !

!StreamDescriptionParseNode methodsFor: 'actions'!

def processNewComponent: aValue 
	^self initializeNewComponent: aValue
end! !

!StreamDescriptionParseNode methodsFor: 'addition/removal'!

def add: aValue 
	^self attributes add: aValue
end!

def at: aKey put: aValue 

	^self add: (aValue asAttributeNamed: aKey)
end!

def remove: aValue
	^self attributes remove: aValue
end! !

!StreamDescriptionParseNode methodsFor: 'initialize-release'!

def initialize
	super initialize.
	attributes := OrderedCollection new
end!

def initializeNewComponent: aComponent 
	self attributes do: [:attrib | attrib initializeClient: aComponent with: attrib value].
	^aComponent
end! !

!StreamDescriptionParseNode methodsFor: 'printing'!

def printOn: aStream level: aLevel

	"do nothing"
end! !

StreamDescriptionParseNode class
	instanceVariableNames: ''!

StreamDescriptionParseNode subclass: #StreamPersistentObjectDescriptionNode
	instanceVariableNames: ':database, :ifOneBlock, :ifManyBlock, :ifFoundBlock, :ifNoneBlock, :newClientTranslator, :predicate, :shouldUseSubclasses, :variableNames, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'An StreamPersistentObjectDescriptionNode takes the object answered by the evaluation of its component, and asks for a persistent object of the same class which conforms to the description given by the collection of attribute nodes.  If one is found, answer the value of the ifFound block (see below for a more thorough description of the blocks).  If many are found, use the ifMany block as a filter against the answer set.  If none is found, answer the value of the ifNone block.  

IfMany
	This block takes the new client and the answer set as its parameter.  It should answer as small a subset of that collection as it can.  The default is to answer the supplied collection:
	[:client :collection | collection]
If it answers an empty or a one-element collection, proceed as if that had been the databases'' response (evaluate the ifNone or ifFound block, respectively).  If it answers a collection of any other size, log the error, select one arbitrarily, and proceed (without evaluating either the ifFound or ifNone blocks).  

ifFound
	This block takes the persistent object as a parameter.  The default is to just answer that parameter directly:
		[:value | value]
	Although some translation could be performed within the block:
		[:value | value isOutOfDate 
			ifTrue: [self defaultValue]
			ifFalse: [value]]
	for nontrivial translations it is better to just use the default ifFound block, and do the translation in a 
	StreamTranslationParseNode that contains the StreamPersistentObjectDescriptionNode.  

ifNone
	This block takes two parameters.  The first parameter is the value of my component (the template object).  The second parameter is a one-argument block that can be used to initialize an object of the same type such that it satisfies the descriptions given by my attributes.  For example, the ifNone block:
		[:component :initializer | initializer value: component]
would initialize the component and answer it as my value.  The default ifNone block raises an exception providing the two block parameters as exception parameters.  '
=end!

!StreamPersistentObjectDescriptionNode methodsFor: 'accessing'!

def attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeReferenceParseNode
end!

def database
	^database
end!

def database: aDatabase
	database := aDatabase
end!

def defaultDatabase
	^database
end!

def ifFound
	^ifFoundBlock
end!

def ifFound: aValue
	ifFoundBlock := aValue
end!

def ifMany
	^ifManyBlock
end!

def ifMany: aBlock 
	ifManyBlock := aBlock
end!

def ifNone
	^ifNoneBlock
end!

def ifNone: aValue
	ifNoneBlock := aValue
end!

def newComponentInitializer
	^[:aClient | self initializeNewComponent: aClient]
end!

def predicate
	^predicate
end!

def predicate: aValue
	predicate := aValue
end!

def shouldUseSubclasses
	^shouldUseSubclasses
end!

def shouldUseSubclasses: aValue
	shouldUseSubclasses := aValue
end! !

!StreamPersistentObjectDescriptionNode methodsFor: 'actions'!

def processNewComponent: aValue 
	"Answer the instance of aValue that is uniquely 
	described by my attributes."

	"evaluate each of the attributes first for consistency"
	self attributes do: [:each | each value].
	^self singleInstanceOf: aValue
end!

def useIfNoneThenNilBlock
	self ifNone: self ifNoneThenNilBlock
end!

def useIfNoneThenPrototypeWithErrorBlock
	self ifNone: self ifNoneThenPrototypeWithErrorBlock
end!

def useIfNoneThenPrototypeWithoutErrorBlock
	self ifNone: self ifNoneThenPrototypeWithoutErrorBlock
end! !

!StreamPersistentObjectDescriptionNode methodsFor: 'initialize-release'!

def ifNoneThenNilBlock
	^[:client :initializer | 
	self persistentObjectNotFoundBlock 
		value: client 
		value: initializer.
	self context traceWith: '  Proceeding with ''nil''.'.
	nil]
end!

def ifNoneThenPrototypeWithErrorBlock
	^[:client :initializer | 
	self persistentObjectNotFoundBlock 
		value: client 
		value: initializer.
	self context traceWith: '  Proceeding with prototype.'.
	self ifNoneThenPrototypeWithoutErrorBlock 
		value: client 
		value: initializer]
end!

def ifNoneThenPrototypeWithoutErrorBlock
	^[:client :initializer | initializer value: client]
end!

def initialize
	super initialize.
	database := self defaultDatabase.
	ifManyBlock := [:client :collection | collection].
	ifFoundBlock := #yourself.
	ifNoneBlock := self ifNoneThenPrototypeWithErrorBlock.
	shouldUseSubclasses := true
end!

def initializeNewComponent: aClient 
	self attributes do: 
		[:each | 
		each 
			initializeClient: aClient 
			with: each lastValue].
	"aClient becomePersistentIn: aClient class specificDatabase."
aClient becomePersistentIn: self database.
	^aClient
end!

def persistentObjectNotFoundBlock
	^[:client :initializer | 
	self context persistentObjectNotFoundSignal 
		raiseRequestWith: 
			(PersistentObjectNotFoundSummary
				client: client
				initializer: initializer
				predicate: self predicate)]
end! !

!StreamPersistentObjectDescriptionNode methodsFor: 'utility'!

def handleUndefinedClassWith: handleBlock do: doBlock 
	^VError errorSignal 
		handle: 
			[:ex | 
			(VError errorSymbol == #VSI_WARNING and: [VError description = 'Class not found'])
				ifTrue: [handleBlock value: ex]
				ifFalse: [ex reject]]
		do: doBlock
end!

def persistentInstancesOf: newClient 
	| attributeCount query keys values |
	attributeCount := self attributes size.
	query := OrderedCollection new: attributeCount.
	keys := OrderedCollection new: attributeCount.
	values := OrderedCollection new: attributeCount.
	self attributes do: [:attrib | attrib
			addQueryTo: query
			keyTo: keys
			valueTo: values].
	^self 
		handleUndefinedClassWith: [:ex | Array new]
		do: [| myPredicate |
			myPredicate := VPredicate from: (self searchStringFrom: query).
			myPredicate valuesAt: keys put: values.
			self predicate: myPredicate.
			newClient class selectFrom: self database where: myPredicate]
end!

def searchStringFrom: aQueryList

	| str |
	str := (String new: 100) writeStream.
	aQueryList 
		do: [:each | str nextPutAll: each]
		separatedBy: [str nextPutAll: ' & '].
	^str contents
end!

def singleElementFrom: aCollection ifMany: manyBlock ifFound: foundBlock ifNone: noneBlock
	| hits |
	hits := aCollection size > 1
		ifTrue: [manyBlock value]
		ifFalse: [aCollection].

	^hits size == 1
		ifTrue: [foundBlock value: hits someElement]
		ifFalse: 
			[hits size == 0
				ifTrue: [noneBlock value]
				ifFalse: 
					[self context 
						newLineTraceWith: 'Unable to resolve instance.  Choosing one arbitrarily.'.
					hits someElement]]
end!

def singleInstanceOf: newClient 

	| persistentInstances initializer |
	persistentInstances := self persistentInstancesOf: newClient.
	initializer := self newComponentInitializer.
	^self 
		singleElementFrom: persistentInstances
		ifMany: [self ifMany value: (initializer value: newClient) value: persistentInstances]
		ifFound: self ifFound
		ifNone: [self ifNone value: newClient value: initializer]
end! !

StreamPersistentObjectDescriptionNode class
	instanceVariableNames: ''!

!StreamPersistentObjectDescriptionNode class methodsFor: 'instance creation'!

def on: aNode database: aDatabase
	^(self on: aNode)
		database: aDatabase
end!

def on: aNode database: aDatabase ifFound: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifFound: block
end!

def on: aNode database: aDatabase ifFound: block1 ifNone: block2
	^(self 
		on: aNode
		database: aDatabase
	)
		ifFound: block1;
		ifNone: block2
end!

def on: aNode database: aDatabase ifMany: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifMany: block
end!

def on: aNode database: aDatabase ifMany: manyBlock2 ifFound: foundBlock1 ifNone: block2
	^(self 
		on: aNode
		database: aDatabase
	)
		ifMany: manyBlock2;
		ifFound: foundBlock1;
		ifNone: block2
end!

def on: aNode database: aDatabase ifNone: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifNone: block
end!

def on: aNode ifFound: block
	^(self on: aNode)
		ifFound: block
end!

def on: aNode ifNone: block
	^(self on: aNode)
		ifNone: block
end! !

StreamPersistentObjectDescriptionNode subclass: #StreamPersistentObjectCachedDescriptionNode
	instanceVariableNames: ':clientCache, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me specialize the behavior of my superclass by providing a cache of past answers.  All previous matches are cached, and the cache is checked before the database.  Since nothing in the cache can be garbage collected (until the instance holding the cache is), this has the added benefit of maintaining all complete dereferenced paths from those elements from being garbage collected, thereby assuring that the attribute data need not be fetched multiple times.  

Instances of this class should be used sparingly.  As with all caching strategies, the decision of whether and what to cache can be difficult.  Considerations include:
	the larger the cache
		the less application memory available
		the more overhead it requires (storage of indexes, empty slots for growth, etc.)
		the more expensive it is to search (therefore the more expensive a miss)
	the smaller the cache
		the larger the percentage of cache resources consumed by overhead
		the more frequent the miss

Success involves identifying a small subset of the possible data that is most frequently requested by the client, and caching only that.  '
=end!

!StreamPersistentObjectCachedDescriptionNode methodsFor: 'accessing'!

def value
	"Add the newly retrieved element to the cache, and answer it."
	"Since the cache is an IdentitySet it does not matter if we add 
	an element that is already in the cache."

	^self getClientCache add: super value
end! !

!StreamPersistentObjectCachedDescriptionNode methodsFor: 'initialize-release'!

def initialize
	super initialize.
	self initializeClientCache
end!

def initializeClientCache
	clientCache := IdentitySet new
end! !

!StreamPersistentObjectCachedDescriptionNode methodsFor: 'private'!

def cachedInstancesOf: newClient
	"Answer the elements in the cache completely described 
	by my attributes"

	^self getClientCache reject: 
		[:aClient | 
		self attributes contains: 
			[:attrib | (attrib stReader value: aClient) ~= (attrib lastValue)]]
end!

def cachedInstancesOf: newClient ifNone: block0 
	"Answer the elements in the cache completely described 
	by my attributes, or if there are none, the value of the ifNone block."

	| instances |
	^(instances := self cachedInstancesOf: newClient) size == 0
		ifTrue: [block0 value]
		ifFalse: [instances]
end!

def getClientCache
	^clientCache
end!

def persistentInstancesOf: newClient 
	"Answer the elements in the cache completely described 
	by my attributes, or if there are none, try in the database."

	^self 
		cachedInstancesOf: newClient 
		ifNone: [super persistentInstancesOf: newClient]
end! !

StreamPersistentObjectCachedDescriptionNode class
	instanceVariableNames: ''!

StreamPersistentObjectCachedDescriptionNode subclass: #StreamPersistentObjectLimitedCachedDescriptionNode
	instanceVariableNames: ':maxSize, :queryHistory, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Just like instances of my superclass, instances of me maintain a cache.  My cache, however, is of a fixed size.  Whenever the addition of a new element causes the cache to grow beyond that size I remove from my cache the oldest element.  '
=end!

!StreamPersistentObjectLimitedCachedDescriptionNode methodsFor: 'accessing'!

def add: aValue 
	
	"is it already in the cache?"
	(self getClientCache includes: aValue)
		ifTrue: 
			["move it to the front of the history list"
			queryHistory 
				remove: aValue;
				addFirst: aValue]
		ifFalse: 
			[super add: aValue.
			queryHistory addFirst: aValue.
			self recalibrate].
	^aValue
end!

def maxSize
	^maxSize
end!

def maxSize: aValue
	maxSize := aValue.
	self recalibrate
end! !

!StreamPersistentObjectLimitedCachedDescriptionNode methodsFor: 'actions'!

def recalibrate
	"If necessary, reduce the cache size to maxSize by 
	removing the oldest elements."

	(queryHistory size - self maxSize) 
		timesRepeat: [clientCache remove: (queryHistory removeLast)]
end! !

!StreamPersistentObjectLimitedCachedDescriptionNode methodsFor: 'initialize-release'!

def initializeClientCache
	super initializeClientCache.
	queryHistory := OrderedCollection new
end! !

!StreamPersistentObjectLimitedCachedDescriptionNode methodsFor: 'private'!

def getQueryHistory
	^queryHistory
end! !

StreamPersistentObjectLimitedCachedDescriptionNode class
	instanceVariableNames: ''!

StreamComponentParseNode subclass: #StreamAttributeParseNode
	instanceVariableNames: ':lastValue, :odbReader, :stReader, :stWriter, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me have a component that answers an object, and a set of functions that map that value to a slot.  I do not perform the actual mapping, I simply store the information on how to do it.  The different read and write syntaxes for the database and Smalltalk collections require four separate functions: #stReader, stWriter, odbReader, and odbWriter.  My two subclasses are specialized to handle the two different types of database read queries: value and reference.  '
=end!

!StreamAttributeParseNode methodsFor: 'accessing'!

def addQueryTo: aCollection keyTo: aKeyCollection valueTo: aValueCollection 
	^aCollection add: self odbReader
end!

def lastValue
	^lastValue
end!

def lastValue: aValue
	lastValue := aValue
end!

def odbReader
	^odbReader
end!

def odbReader: aValue
	odbReader := aValue
end!

def odbVariableName
	^nil
end!

def slotName
	^self stReader isSymbol
		ifTrue: [self stReader]
		ifFalse: [nil]
end!

def stReader
	^stReader
end!

def stReader: aValue
	stReader := aValue
end!

def stReader: block1 stWriter: block2 odbReader: odbQuery 
	self 
		stReader: block1;
		stWriter: block2;
		odbReader: odbQuery
end!

def stWriter
	^stWriter
end!

def stWriter: aValue
	stWriter := aValue
end!

def value
	"Store then answer the result of 1 computational cycle of my machinery."

	| myValue |
	self lastValue: (myValue := super value).
	^myValue
end! !

!StreamAttributeParseNode methodsFor: 'converting'!

def asAttributeNamed: aName

	^self error: 'A ', self class name, ' cannot be converted to an attribute node.'
end! !

!StreamAttributeParseNode methodsFor: 'initialize-release'!

def initializeClient: aClient with: aValue
	self stWriter 
		value: aClient
		value: aValue
end! !

!StreamAttributeParseNode methodsFor: 'printing'!

def printOdbReaderOn: aStream 
	^aStream 
		nextPutAll: self odbReader;
		yourself
end!

def printOn: aStream
	aStream
		nextPut: $?
end! !

!StreamAttributeParseNode methodsFor: 'testing'!

def needsOdbParameter
	"Answer whether the query that I represent has a 
	replaceable parameter."

	^false
end! !

StreamAttributeParseNode class
	instanceVariableNames: ''!

!StreamAttributeParseNode class methodsFor: 'instance creation'!

def on: aValue slotName: aName 
	self subclassResponsibility
end!

def on: aValue stReader: block1 stWriter: block2 odbReferenceTest: odbQuery odbVariableName: aVariableName 

	^(StreamAttributeReferenceParseNode on: aValue)
		stReader: block1
		stWriter: block2
		odbReferenceTest: odbQuery
		odbVariableName: aVariableName
end!

def on: aValue stReader: block1 stWriter: block2 odbValueTest: odbQuery 

	^(StreamAttributeValueParseNode on: aValue)
		stReader: block1
		stWriter: block2
		odbEquivalenceTest: odbQuery
end! !

!StreamAttributeParseNode class methodsFor: 'private'!

def stAccessorForSlotNamed: aName
	"self stAccessorForSlotNamed: 'foo'"

	"| blockSource |
	blockSource := (String new: 30) writeStream
		nextPutAll: '[:client | client ';
		nextPutAll: aName;
		nextPut: $];
		contents.
	^Compiler evaluate: blockSource"

	^aName asSymbol
end!

def stMutatorForSlotNamed: aName
	"self stMutatorForSlotNamed: 'foo'"

	"| blockSource |
	blockSource := (String new: 30) writeStream
		nextPutAll: '[:client :newValue | client ';
		nextPutAll: aName;
		nextPutAll: ': newValue]';
		contents.
	^Compiler evaluate: blockSource"

	^(aName, ':') asSymbol
end! !

StreamAttributeParseNode subclass: #StreamAttributeValueParseNode
	instanceVariableNames: ':genericOdbReader, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me are specialized to perform database searches for object whose slot (which I identify) contains a value that is similar to the value of my component.  This is typically a string, or a number.  '
=end!

!StreamAttributeValueParseNode methodsFor: 'accessing'!

def genericOdbReader
	^genericOdbReader
end!

def genericOdbReader: aValue
	genericOdbReader := aValue
end!

def odbReader
	"Since the syntax of my odb query depends upon my value, 
	I make that adjustment now."

	^odbReader == nil
		ifTrue: [odbReader := (self printOdbReaderOn: String new writeStream) contents]
		ifFalse: [odbReader]
end!

def stReader: block1 stWriter: block2 odbEquivalenceTest: odbQuery 
	self
		stReader: block1
			stWriter: block2
			odbReader: '';
		genericOdbReader: odbQuery
end!

def value
	"Answer the result of 1 computational cycle of my machinery."

	self odbReader: nil.	"invalidate my cached odbReader"
	^super value
end! !

!StreamAttributeValueParseNode methodsFor: 'printing'!

def printOdbReaderOn: aStream 
	^aStream 
		nextPutAll: self genericOdbReader; 
		print: self lastValue;
		yourself
end!

def printOn: aStream

	super printOn: aStream.
	self genericOdbReader == nil
		ifTrue: [aStream print: self stReader]
		ifFalse: [aStream print: self genericOdbReader]
end! !

StreamAttributeValueParseNode class
	instanceVariableNames: ''!

!StreamAttributeValueParseNode class methodsFor: 'instance creation'!

def on: aValue slotName: aName

	^self 
		on: aValue 
		stReader: (self stAccessorForSlotNamed: aName) 
		stWriter: (self stMutatorForSlotNamed: aName) 
		odbValueTest: aName copy, ' = '
end! !

StreamAttributeParseNode subclass: #StreamAttributeReferenceParseNode
	instanceVariableNames: ':odbVariableName, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me are specialized to perform database searches for object whose slot (which I identiry) contains a reference to the value of my component (which is expected to be a persistent object).  '
=end!

!StreamAttributeReferenceParseNode methodsFor: 'accessing'!

def addQueryTo: aCollection keyTo: aKeyCollection valueTo: aValueCollection 
	aKeyCollection add: self odbVariableName.
	aValueCollection add: lastValue.
	^super
		addQueryTo: aCollection
		keyTo: aKeyCollection
		valueTo: aValueCollection
end!

def odbVariableName
	^odbVariableName
end!

def stReader: block1 stWriter: block2 odbReferenceTest: odbQuery odbVariableName: aVariableName 
	self
		stReader: block1
		stWriter: block2
		odbReader: odbQuery.
	odbVariableName := aVariableName
end! !

!StreamAttributeReferenceParseNode methodsFor: 'printing'!

def printOn: aStream
	super printOn: aStream.
	aStream
		print: self odbReader
end! !

!StreamAttributeReferenceParseNode methodsFor: 'testing'!

def needsOdbParameter
	"Answer whether the query that I represent has a 
	replaceable parameter."

	^true
end! !

StreamAttributeReferenceParseNode class
	instanceVariableNames: ''!

!StreamAttributeReferenceParseNode class methodsFor: 'instance creation'!

def on: aValue slotName: aName

	| aNameString |
	aNameString := aName asString copy.
	^self 
		on: aValue 
		stReader: (self stAccessorForSlotNamed: aName) 
		stWriter: (self stMutatorForSlotNamed: aName) 
		odbReferenceTest: aNameString, ' = :', aNameString
		odbVariableName: aName
end! !

StreamParseNode subclass: #StreamBasicParseNode
	instanceVariableNames: ':delimiter, :sourceStream, :width, '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!
=begin
'Instances of me know how to read "terminal" values from the database.  By "terminal", I mean objects that are not normally considered to have links to other objects, for instance integers, strings, dates, etc.  This is of course independent upon whether they are implemented that way.  (For instance, a String is actually a collection of pointers to characters, but that is ignored in most problem domains.)  I am prepared to deal with files containing either fixed-width or delimited columns.  '
=end!

!StreamBasicParseNode methodsFor: 'accessing'!

def attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeValueParseNode
end!

def component
	"Answer the node that provides my input."
	"For database storage reasons the string needs to 
	be a ByteString"

	^ByteString new: self width
end!

def components
	^OrderedCollection new
end!

def dataType
	^self class dataType
end!

def delimitedValue

	^self delimiter size > 1
		ifTrue: [self vectorDelimitedValue]
		ifFalse: [self scalarDelimitedValue]
end!

def delimiter
	^delimiter
end!

def delimiter: aValue

	aValue == nil
		ifTrue: [delimiter := nil]
		ifFalse: [
			delimiter := aValue.
			self width: nil
		]
end!

def scalarDelimitedValue
	^self sourceStream upTo: self delimiter
end!

def sourceStream
	^sourceStream
end!

def sourceStream: aReadStream
	sourceStream := aReadStream
end!

def value
	"Answer the string value that I represent."

	^self shouldUseDelimitedFormat
		ifTrue: [self delimitedValue]
		ifFalse: [self widthValue]
end!

def vectorDelimitedValue

	| myDelimiter myStream value |
	myStream := self sourceStream.
	myDelimiter := self delimiter.
	value := myStream upToAll: myDelimiter.
	myStream skip: myDelimiter size.
	^value
end!

def width

	^width
end!

def width: aValue

	aValue == nil
		ifTrue: [width := nil]
		ifFalse: [
			width := aValue.
			self delimiter: nil
		]
end!

def widthValue
	"Answer the value I represent, constraining 
	the number of characters."

	^self sourceStream 
		next: self width
		into: super value
		startingAt: 1
end! !

!StreamBasicParseNode methodsFor: 'inquiries'!

def attributeCount
	"Answer the count of attributes of which I am composed.  I am 
	exactly 1 attribute."

	^1
end!

def mapsDirectlyToFile
	^true
end!

def shouldUseDelimitedFormat
	^self delimiter ~~ nil
end!

def shouldUseFixedWidthFormat
	^self shouldUseDelimitedFormat not
end! !

!StreamBasicParseNode methodsFor: 'printing'!

def printOn: aStream

	aStream print: self dataType.
	self shouldUseFixedWidthFormat
		ifTrue: [
			aStream 
				nextPut: $[;
				print: self width;
				nextPut: $]
		]
		ifFalse: [
			aStream nextPutAll: ', <'.
			self delimiter == Character cr
				ifTrue: [aStream nextPutAll: 'CR']
				ifFalse: [
					self delimiter == Character tab
						ifTrue: [aStream nextPutAll: 'TAB']
						ifFalse: [aStream print: self delimiter]
				].
			aStream nextPut: $>
		]
end!

def printOn: aStream level: aLevel

	aStream
		print: self
end! !

StreamBasicParseNode class
	instanceVariableNames: ''!

!StreamBasicParseNode class methodsFor: 'accessing'!

def defaultDelimiter
	"Answer the delimiter I should assume when I 
	am not provided one."

	^Character cr
end! !

!StreamBasicParseNode class methodsFor: 'instance creation'!

def on: aReadStream forType: aDataType
	"Answer a delimited node specialized for dealing 
	with aDataType."

	| aClassOrNil |
	aClassOrNil := self classForType: aDataType.
	^aClassOrNil == nil
		ifTrue: [nil]
		ifFalse: [
			aClassOrNil new
				sourceStream: aReadStream;
				delimiter: self defaultDelimiter
		]
end!

def on: aReadStream forType: aDataType delimiter: aDelimiter 
	"Answer a a delimited node specialized for dealing 
	with aDataType."

	^(self on: aReadStream forType: aDataType)
		delimiter: aDelimiter
end!

def on: aReadStream forType: aDataType width: anInteger 
	"Answer a fixed-width node specialized for dealing 
	with aDataType."

	^(self on: aReadStream forType: aDataType)
		width: anInteger
end! !

!StreamBasicParseNode class methodsFor: 'utility'!

def classForType: aDataType 
	"Answer the subclass that is designed to handle data of type 'aDataType'."

	^self allSubclasses 
		detect: [:each | each dataType == aDataType]
		ifNone: []
end! !

StreamBasicParseNode subclass: #CharacterParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!CharacterParseNode methodsFor: 'accessing'!

def scalarDelimitedValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	^(value := stream next) = self delimiter
		ifTrue: [nil]
		ifFalse: [
			stream skipThrough: self delimiter.
			value
		]
end!

def vectorDelimitedValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	value := stream upToAll: self delimiter.
	stream skip: self delimiter size.
	nil halt: 'I should check if it is scalar, not if it is size == 1'.
	^value size > 1
		ifTrue: [value first]
		ifFalse: [value]
end!

def widthValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	value := stream next.
	stream skip: self width - 1.
	^value
end! !

CharacterParseNode class
	instanceVariableNames: ''!

!CharacterParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Character
end! !

CharacterParseNode subclass: #BooleanParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!BooleanParseNode methodsFor: 'accessing'!

def value
	"Answer the boolean value that I represent."

	| booleanChar |
	booleanChar := super value asUppercase.
	^(booleanChar == $T or: [booleanChar == $Y])
		ifTrue: [true]
		ifFalse: [
			(booleanChar == $F or: [booleanChar == $N])
				ifTrue: [false]
				ifFalse: [
					self unexpectedValueSignal 
						raiseRequestErrorString: ' ', 
						booleanChar printString, 
						' <- One of {YyTtFfNn} expected.'	
			]
		].
end! !

BooleanParseNode class
	instanceVariableNames: ''!

!BooleanParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Boolean
end! !

StreamBasicParseNode subclass: #StringParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!StringParseNode methodsFor: 'accessing'!

def changeWidthTo: aWidth while: block0 
	| value oldWidth |
	
	[oldWidth := self width.
	self width: aWidth.
	value := block0 value]
		valueNowOrOnUnwindDo: [self width: oldWidth].
	^value
end!

def value
	"Answer the string value that I represent."

	^super value withoutLeadingOrTrailingWhitespace
end!

def widthValue
	"Answer the string contained in the next fieldWidth characters on 
	my sourceStream."

	| aWidth space |
	"do not read leading spaces"
	aWidth := self width.
	space := Character space.
	[aWidth > 0 and: [sourceStream next == space]] 
		whileTrue: [aWidth := aWidth - 1].
	aWidth == 0
		ifFalse: [self sourceStream skip: -1].

	"The template we create for this client must be smaller than the default 
	width.  We return it to the original state to prepare for the next client."
	^self 
		changeWidthTo: aWidth 
		while: [super widthValue]
end! !

StringParseNode class
	instanceVariableNames: ''!

!StringParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process.
	Subclasses should override this."

	^#String
end! !

StringParseNode subclass: #TitleParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!TitleParseNode methodsFor: 'accessing'!

def value
	"Answer the title value that I represent."

	^super value capitalizeWords
end! !

TitleParseNode class
	instanceVariableNames: ''!

!TitleParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Title
end! !

StreamBasicParseNode subclass: #TimeParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!TimeParseNode methodsFor: 'accessing'!

def value
	^Time readFrom: (super value) readStream
end! !

TimeParseNode class
	instanceVariableNames: ''!

!TimeParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Time
end! !

StreamBasicParseNode subclass: #NumberParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!NumberParseNode methodsFor: 'accessing'!

def clientClass
	^Number
end!

def value
	"Answer the number value that I represent."

	^self clientClass readFrom: super value readStream
end! !

NumberParseNode class
	instanceVariableNames: ''!

!NumberParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Number
end! !

NumberParseNode subclass: #MoneyParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!MoneyParseNode methodsFor: 'accessing'!

def value
	"Answer the money value that I represent.
	To avoid rounding errors I answer money as 
	number of cents."

	^(super value * 10) asInteger
end! !

MoneyParseNode class
	instanceVariableNames: ''!

!MoneyParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Money
end! !

NumberParseNode subclass: #IntegerParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!IntegerParseNode methodsFor: 'accessing'!

def clientClass

	^Integer
end! !

IntegerParseNode class
	instanceVariableNames: ''!

!IntegerParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Integer
end! !

StreamBasicParseNode subclass: #NullParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!NullParseNode methodsFor: 'accessing'!

def dataType
	^#nil
end!

def delimitedValue
	super delimitedValue.
	^nil
end!

def scalarDelimitedValue

	self sourceStream skipThrough: self delimiter.
	^nil
end!

def vectorDelimitedValue

	self sourceStream 
		skipToAll: self delimiter;
		skip: self delimiter size.
	^nil
end!

def widthValue
	"Ignore the information stored in the next field."

	self sourceStream skip: self width.
	^nil
end! !

NullParseNode class
	instanceVariableNames: ''!

!NullParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^nil
end! !

StreamBasicParseNode subclass: #TimestampParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!TimestampParseNode methodsFor: 'accessing'!

def value
	"Answer the timestamp value that I represent."

	| tsString |
	^(tsString := super value asUppercase) = 'MAXIMUM'
		ifTrue: [Timestamp openEnded]
		ifFalse: [tsString = 'MINIMUM'
					ifTrue: [Timestamp arbitraryBegin]
					ifFalse: [Timestamp readFrom: tsString readStream]]
end! !

TimestampParseNode class
	instanceVariableNames: ''!

!TimestampParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Timestamp
end! !

StreamBasicParseNode subclass: #DateParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'!

!DateParseNode methodsFor: 'accessing'!

def specialMappings
	"Answer mappings from the special date forms to the dates 
	they represent."

	^OrderedCollection 
		add: 'MAXIMUM' -> Date openEnded;
		add: 'MINIMUM' -> Date arbitraryBegin;
		add: nil -> nil;
		yourself
end!

def value
	"Answer the date value that I represent."

	| dateString |
	dateString := super value asUppercase.
	^self specialMappings 
		detect: [:each | 
			dateString = each key
				ifTrue: [each value value]]
		ifNone: [Date readFrom: dateString readStream]
end! !

DateParseNode class
	instanceVariableNames: ''!

!DateParseNode class methodsFor: 'accessing'!

def dataType
	"Answer the data type that I am designed to process."

	^#Date
end! !

!Object methodsFor: 'accessing'!

def attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeValueParseNode
end! !

!Object methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^false
end! !

!Boolean methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true
end! !

!UndefinedObject methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true
end! !

!Character methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true
end! !

!SmallInteger methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true
end! !

!CharacterArray methodsFor: 'converting'!

def capitalizeWords
	"'paul SCHAAF' capitalizeWords"
	"P.G. Schaaf, 06/09/95"

	| capitalize |
	capitalize := true.
	^self collect: [:each | 
		(each isWhite or: [each isDigit or: [each isLeftParenthetical]])
			ifTrue: 
				[capitalize := true.
				each]
			ifFalse: 
				[(capitalize and: [each isAlphabetic])
					ifTrue: 
						[capitalize := false.
						each asUppercase]
					ifFalse: [each asLowercase]]]
end! !

!Symbol methodsFor: 'testing'!

def isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true
end! !
StreamParseNode initialize!


