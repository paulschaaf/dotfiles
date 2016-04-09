'From VisualWorks(R), Release 2.5 of 26-September-1995 on 17-December-1996 at 5:51:03 pm'!



Application create: #TextFileParser with: 
    (#( VersantApplicationContexts)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

TextFileParser becomeDefault!

VersantDBApplicationContext subclass: #FileImportParserContext
	instanceVariableNames: 'prePersistentObjects '
	classVariableNames: 'PersistentObjectNotFoundSignal '
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	ApplicationContext
		traceIsOn traceIndent debugIsOn traceStreams
	DBApplicationContext
		database isIgnoringDatabase
	VersantDBApplicationContext
		connectedDatabases
	FileImportParserContext
		prePersistentObjects
"!

FileImportParserContext comment: 'I manage data that should be global to a File Import session, including a cache of objects that have not yet been marked persistent (this is done for efficiency reasons).'!

TextFileParser becomeDefault!

Object subclass: #FileParser
	instanceVariableNames: 'registry registrar recordsDoneCount startTimeSeconds recordCount '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	FileParser
		registry registrar recordsDoneCount startTimeSeconds recordCount
"!

FileParser comment: 'I construct a machine of parse nodes that can interpret the contents of the file I represent.  I manage the process of interpretation and the resultant objects, including persistence issues.  

*************
If the files are not self-referential (if they are completely normalized relational tables and the ordering of the records within the file cannot affect the processing time) they can be split into an arbitrary number of pieces and divided up between images on a different machine.  If these parsings are done concurrently the run times can be made to be as low as you like.  
**************'!

TextFileParser becomeDefault!

Object subclass: #PersistentObjectNotFoundSummary
	instanceVariableNames: 'client initializer predicate '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	PersistentObjectNotFoundSummary
		client initializer predicate
"!

TextFileParser becomeDefault!

Object subclass: #StreamParseNode
	instanceVariableNames: ''
	classVariableNames: 'UnexpectedValueSignal '
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
"!

StreamParseNode comment: 'A StreamParseNode is a machine that knows how to read and interpret data from the current position in its stream.  This process is initiated by asking the node for its #value.  Subclasses specialize this behavior to either apply a transformation to the data (such as mapping, interpretation, etc.), or to coordinate the component nodes that do the actual parsing.  
'!

TextFileParser becomeDefault!

StreamParseNode subclass: #StreamBasicParseNode
	instanceVariableNames: 'delimiter sourceStream width '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
"!

StreamBasicParseNode comment: 'Instances of me know how to read "terminal" values from the database.  By "terminal", I mean objects that are not normally considered to have links to other objects, for instance integers, strings, dates, etc.  This is of course independent upon whether they are implemented that way.  (For instance, a String is actually a collection of pointers to characters, but that is ignored in most problem domains.)  I am prepared to deal with files containing either fixed-width or delimited columns.  '!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #CharacterParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	CharacterParseNode
"!

TextFileParser becomeDefault!

CharacterParseNode subclass: #BooleanParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	CharacterParseNode
	BooleanParseNode
"!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #DateParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	DateParseNode
"!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #NullParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	NullParseNode
"!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #NumberParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	NumberParseNode
"!

TextFileParser becomeDefault!

NumberParseNode subclass: #IntegerParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	NumberParseNode
	IntegerParseNode
"!

TextFileParser becomeDefault!

NumberParseNode subclass: #RealNumberParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	NumberParseNode
	RealNumberParseNode
"!

TextFileParser becomeDefault!

RealNumberParseNode subclass: #MoneyParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	NumberParseNode
	RealNumberParseNode
	MoneyParseNode
"!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #StringParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	StringParseNode
"!

TextFileParser becomeDefault!

StringParseNode subclass: #EndOfLineParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	StringParseNode
	EndOfLineParseNode
"!

EndOfLineParseNode comment: 'This class is unused and untested.  It should be used to model the end-of-line (EOLN) character; either in a fixed-width file, or a delimited file that is either not delimited with EOLNs or that is and has an extra EOLN after the last attribute of each record.  '!

TextFileParser becomeDefault!

StringParseNode subclass: #TitleParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	StringParseNode
	TitleParseNode
"!

TextFileParser becomeDefault!

StreamBasicParseNode subclass: #TimestampParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamBasicParseNode
		delimiter sourceStream width
	TimestampParseNode
"!

TextFileParser becomeDefault!

StreamParseNode subclass: #StreamComponentParseNode
	instanceVariableNames: 'component '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
"!

StreamComponentParseNode comment: 'Instances of me are used to make a non-node object appear to be a node (a la the Decorator pattern).  If you implement

	Object>>value
		^self

then you can use the number 1234 as if it were a parse node by wrapping it with a value node:

	a := StreamComponentParseNode
		on: 1234

Notice that ''a'' is a parse node whose value is constant.  

If you do not implement Object>>value, then the implementation of ''a'' would be: 

	a := StreamComponentParseNode
		on: [1234]'!

TextFileParser becomeDefault!

StreamComponentParseNode subclass: #StreamAttributeParseNode
	instanceVariableNames: 'lastValue odbReader stReader stWriter '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamAttributeParseNode
		lastValue odbReader stReader stWriter
"!

StreamAttributeParseNode comment: 'Instances of me have a component that answers an object, and a set of functions that map that value to a slot.  I do not perform the actual mapping, I simply store the information on how to do it.  The different read and write syntaxes for the database and Smalltalk collections require four separate functions: #stReader, stWriter, odbReader, and odbWriter.  My two subclasses are specialized to handle the two different types of database read queries: value and reference.  '!

TextFileParser becomeDefault!

StreamAttributeParseNode subclass: #StreamAttributeReferenceParseNode
	instanceVariableNames: 'odbVariableName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamAttributeParseNode
		lastValue odbReader stReader stWriter
	StreamAttributeReferenceParseNode
		odbVariableName
"!

StreamAttributeReferenceParseNode comment: 'Instances of me are specialized to perform database searches for object whose slot (which I identiry) contains a reference to the value of my component (which is expected to be a persistent object).  '!

TextFileParser becomeDefault!

StreamAttributeParseNode subclass: #StreamAttributeValueParseNode
	instanceVariableNames: 'genericOdbReader '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamAttributeParseNode
		lastValue odbReader stReader stWriter
	StreamAttributeValueParseNode
		genericOdbReader
"!

StreamAttributeValueParseNode comment: 'Instances of me are specialized to perform database searches for object whose slot (which I identify) contains a value that is similar to the value of my component.  This is typically a string, or a number.  '!

TextFileParser becomeDefault!

StreamComponentParseNode subclass: #StreamCompositionParseNode
	instanceVariableNames: 'components valueBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamCompositionParseNode
		components valueBlock
"!

StreamCompositionParseNode comment: 'Do not use this in place of a StreamDescriptionParseNode, StreamTranslationParseNode, or a StreamBranchParseNode.  

Instances of me contain a collection of component nodes and a valueBlock whose argument count is the number of nodes.  When an instance is evaluated it evaluates each of the components and then evaluates the block using the component values as parameters.  The value answered by my instance is the value answered by its block.  

This node is polymorphic with the others in the sense that it has a component node (established using #on:).  The only distinction between the *component* and the *components* is that the component is passed into the valueBlock as the first argument.  You may choose to distinguish the component in the valueBlock, but this node does not care.  '!

TextFileParser becomeDefault!

StreamComponentParseNode subclass: #StreamDescriptionParseNode
	instanceVariableNames: 'attributes '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamDescriptionParseNode
		attributes
"!

StreamDescriptionParseNode comment: 'A StreamDescriptionParseNode takes the object answered by the evaluation of its component, and makes that object conform to the description given by the collection of attribute nodes.  The initialized object is answered as a result.'!

TextFileParser becomeDefault!

StreamDescriptionParseNode subclass: #StreamPersistentObjectDescriptionNode
	instanceVariableNames: 'database ifOneBlock ifManyBlock ifFoundBlock ifNoneBlock newClientTranslator predicate shouldUseSubclasses variableNames '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamDescriptionParseNode
		attributes
	StreamPersistentObjectDescriptionNode
		database ifOneBlock ifManyBlock ifFoundBlock ifNoneBlock newClientTranslator predicate shouldUseSubclasses variableNames
"!

StreamPersistentObjectDescriptionNode comment: 'An StreamPersistentObjectDescriptionNode takes the object answered by the evaluation of its component, and asks for a persistent object of the same class which conforms to the description given by the collection of attribute nodes.  If one is found, answer the value of the ifFound block (see below for a more thorough description of the blocks).  If many are found, use the ifMany block as a filter against the answer set.  If none is found, answer the value of the ifNone block.  

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
would initialize the component and answer it as my value.  The default ifNone block raises an exception providing the two block parameters as exception parameters.  '!

TextFileParser becomeDefault!

StreamPersistentObjectDescriptionNode subclass: #StreamPersistentObjectCachedDescriptionNode
	instanceVariableNames: 'clientCache '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamDescriptionParseNode
		attributes
	StreamPersistentObjectDescriptionNode
		database ifOneBlock ifManyBlock ifFoundBlock ifNoneBlock newClientTranslator predicate shouldUseSubclasses variableNames
	StreamPersistentObjectCachedDescriptionNode
		clientCache
"!

StreamPersistentObjectCachedDescriptionNode comment: 'Instances of me specialize the behavior of my superclass by providing a caching facility.  
Specialize the behavior of my superclass to provide a cache of past answers.  All previous matches are cached, and the cache is checked before the database.  Since nothing in the cache can be garbage collected (until the instance holding the cache is), this has the added benefit of maintaining all complete dereferenced paths from those elements from being garbage collected, thereby assuring that the attribute data need not be fetched multiple times.  

Instances of this class should be used sparingly.  As with all caching strategies, the decision of whether and what to cache can be difficult.  Considerations include:
	the larger the cache
		the less application memory available
		the more overhead it requires (storage of indexes, empty slots for growth, etc.)
		the more expensive it is to search (therefore the more expensive a miss)
	the smaller the cache
		the larger the percentage of cache resources consumed by overhead
		the more frequent the miss

Success involves identifying a small subset of the possible data that is most frequently requested by the client, and caching only that.  '!

TextFileParser becomeDefault!

StreamPersistentObjectCachedDescriptionNode subclass: #StreamPersistentObjectLimitedCachedDescriptionNode
	instanceVariableNames: 'maxSize queryHistory '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamDescriptionParseNode
		attributes
	StreamPersistentObjectDescriptionNode
		database ifOneBlock ifManyBlock ifFoundBlock ifNoneBlock newClientTranslator predicate shouldUseSubclasses variableNames
	StreamPersistentObjectCachedDescriptionNode
		clientCache
	StreamPersistentObjectLimitedCachedDescriptionNode
		maxSize queryHistory
"!

StreamPersistentObjectLimitedCachedDescriptionNode comment: 'Just like instances of my superclass, instances of me maintain a cache.  My cache, however, is of a fixed size.  Whenever the addition of a new element causes the cache to grow beyond that size I remove from my cache the oldest element.  '!

TextFileParser becomeDefault!

StreamComponentParseNode subclass: #StreamLoopFeedbackParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamLoopFeedbackParseNode
"!

StreamLoopFeedbackParseNode comment: 'This node replaces its component with the value of its component whenever it is asked 
for its value (i.e. it uses its own output from invocation n as its input on invocation n+1).  For example:  
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

The first time this is evaluated, the pi is computed and then answered.  All subsequent times 
it is simply answered directly without being re-computed.
'!

TextFileParser becomeDefault!

StreamComponentParseNode subclass: #StreamTranslationParseNode
	instanceVariableNames: 'translator '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamTranslationParseNode
		translator
"!

StreamTranslationParseNode comment: 'Instances of me answer the value of their component after having applied a transformation function.  For instance, my owner wishes to know the name of the person answered by another node.  Assuming that personNode is a node whose value is a person, create the translator as follows: 

	StreamTranslationParseNode 
		on: (personNode)
		translator: [:aPerson | aPerson name]

This translator is passed the output of the component as a parameter, but it need not do anything with that value.  The following node evaluates personNode but discards the result.  When the node is evaluated, it always answers ''John''.  

	StreamTranslationParseNode 
		on: (personNode)
		translator: [:aPerson | ''John'']

Although this is permitted (mainly because I have no means of detecting it) it is not recommended style.  Why do the work that personNode represents if it is simply to be thrown away?  

It is possible to use Translation nodes to simulate Branch and Enumeration nodes--just as it is possible to use a screwdriver as a hammer.  In both situations you are far better off to use the more specialized tool.  '!

TextFileParser becomeDefault!

StreamTranslationParseNode subclass: #StreamEnumeratedTypeParseNode
	instanceVariableNames: 'conditions '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamTranslationParseNode
		translator
	StreamEnumeratedTypeParseNode
		conditions
"!

StreamEnumeratedTypeParseNode comment: 'Instances of me maintain an enumeration of permissible values.  When an instance is evaluated, it evaluates its component and tests to see if that value is in the enumeration.  If it is, answer it.  If it is not, answer the value of ifNoneBlock.

	StreamEnumeratedTypeParseNode
		on: (integerNode)
		enumeration: (1 to: 75)
		ifNone: [:value | Dialog warn: value asString, '' is not in the permissible range!!'']

If the ifNone block is not provided, the default one is used, which raises the unexpectedValueSignal.  You will usually not want to override this.  Instead, handle the signal and process it appropriately.  If you just open a Dialog (as above) the whole parsing process will be suspended until the user acknowledges the dialog.  '!

TextFileParser becomeDefault!

StreamEnumeratedTypeParseNode subclass: #StreamBranchParseNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	StreamParseNode
	StreamComponentParseNode
		component
	StreamTranslationParseNode
		translator
	StreamEnumeratedTypeParseNode
		conditions
	StreamBranchParseNode
"!

StreamBranchParseNode comment: 'My instances serve as a case statement.  They first evaluate their keyNode, search their testConditions for the *first* one that that value satisfies, and answer the value of the corresponding branchNode.  If the value fails every testCondition, evaluate ifNoneBlock with the value as an argument.  

Do not use an instance of me to constrain the permissible input values to an enumerated set, use a StreamEnumeratedTypeNode instead.  '!

TextFileParser becomeDefault!

Application subclass: #TextFileParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TextFileParser'

"	Object
	SubApplication
	Application
	TextFileParser
"!

TextFileParser becomeDefault!

!Boolean publicMethodsFor: 'testing'!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true! !

!BooleanParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Boolean! !

!BooleanParseNode publicMethodsFor: 'accessing'!

value
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
		].! !

!Character publicMethodsFor: 'testing'!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true! !

!CharacterParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Character! !

!CharacterParseNode publicMethodsFor: 'accessing'!

scalarDelimitedValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	^(value := stream next) = self delimiter
		ifTrue: [nil]
		ifFalse: [
			stream skipThrough: self delimiter.
			value
		]!

vectorDelimitedValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	value := stream upToAll: self delimiter.
	stream skip: self delimiter size.
	nil halt: 'I should check if it is scalar, not if it is size == 1'.
	^value size > 1
		ifTrue: [value first]
		ifFalse: [value]!

widthValue
	"Answer the first character contained in 
	the string that I represent."

	| stream value |
	stream := self sourceStream.
	value := stream next.
	stream skip: self width - 1.
	^value! !

!ClassDescription publicMethodsFor: 'printing'!

inheritanceComment
	"ApplicationModel inheritanceComment"

	| str |
	str := WriteStream on: (String new: 300).
	self printInheritanceCommentOn: str.
	^str contents!

printInheritanceCommentOn: aStream
	"ApplicationModel printInheritanceCommentOn: Transcript.  Transcript flush"

	aStream nextPut: $".
	self withAllSuperclasses reverse
		do: [:each | 
			aStream tab; print: each.
			each instVarNames size == 0
				ifFalse: 
					[aStream cr; tab; tab.
					each instVarNames 
						do: [:iVarName | aStream nextPutAll: iVarName]
						separatedBy: [aStream space]]]
		separatedBy: [aStream cr].
	aStream 
		cr; 
		nextPut: $"! !

!DateParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Date! !

!DateParseNode publicMethodsFor: 'accessing'!

value
	"Answer the date value that I represent."

	| dateString |
	^(dateString := super value asUppercase) = 'MAXIMUM'
		ifTrue: [Date openEnded]
		ifFalse: [dateString = 'MINIMUM'
					ifTrue: [Date arbitraryBegin]
					ifFalse: [Date readFrom: dateString readStream]]! !

!EndOfLineParseNode publicMethodsFor: 'accessing'!

value
	"Answer the EOLN value that I represent."

	^self sourceStream skipUpTo: Character cr! !

!FileImportParserContext class publicMethodsFor: 'accessing'!

defaultDatabase
	"self defaultDatabase"
	
	^(Dictionary new
		at: #G00046 put: 'pauldb2@bprsvr4';
		at: #E16718 put: 'chuckdb@bprsvr4';
		yourself
	) at: self currentUserID! !

!FileImportParserContext class publicMethodsFor: 'accessing.signals'!

persistentObjectNotFoundSignal
	^PersistentObjectNotFoundSignal! !

!FileImportParserContext class publicMethodsFor: 'class initialization'!

initializeSignals

	PersistentObjectNotFoundSignal := (Object errorSignal newSignalMayProceed: true)
		notifierString: 'The requested object was not found.';
		nameClass: self 
		message: #databaseContextSignal.! !

!FileImportParserContext class publicMethodsFor: 'utility'!

rootClasses
	^super rootClasses
		add: StreamParseNode;
		add: FileParser;
		yourself! !

!FileImportParserContext publicMethodsFor: 'accessing'!

addPrePersistentObject: anObject 
	^(prePersistentObjects at: anObject class ifAbsentPut: [Set new])
		add: anObject!

prePersistentObjects

	| coll |
	coll := OrderedCollection new.
	self prePersistentObjectsDo: [:each | coll add: each].
	^coll!

prePersistentObjectsClassesDo: block0 
	prePersistentObjects keysAndValuesDo: [:key :value | block0 value: key]!

prePersistentObjectsDo: block0

	prePersistentObjects 
		keysAndValuesDo: [:key :value | value do: block0]!

prePersistentObjectsOfClass: aClass
	^self
		prePersistentObjectsOfClass: aClass
		useSubclasses: false!

prePersistentObjectsOfClass: aClass useSubclasses: aBoolean 
	^self
		prePersistentObjectsOfClass: aClass
		useSubclasses: aBoolean
		ifNone: OrderedCollection new!

prePersistentObjectsOfClass: targetClass useSubclasses: aBoolean ifNone: ifNoneBlock
	| candidates |
	candidates := OrderedCollection new.

	prePersistentObjects keysAndValuesDo: [:aClass :aCollection |
		(aClass == targetClass or: [aClass inheritsFrom: targetClass])
			ifTrue: [candidates addAll: aCollection]
	].

	^candidates!

removePrePersistentObject: anObject 

	^(prePersistentObjects at: anObject class) 
		remove: anObject! !

!FileImportParserContext publicMethodsFor: 'accessing.signals'!

persistentObjectNotFoundSignal
	"self current persistentObjectNotFoundSignal"

	^self class persistentObjectNotFoundSignal! !

!FileImportParserContext publicMethodsFor: 'actions'!

createInDatabaseUsing: block0
	| newInstance |
	newInstance := block0 value.
	^self addPrePersistentObject: newInstance!

flushAllNonPersistentObjects
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
	self newLineTraceWith: flushCount asString, ' objects flushed.'!

persistAllNonPersistentObjects
	"self current persistAllNonPersistentObjects"

	Cursor write showWhile: [
		self prePersistentObjectsDo: [:each | 
			each isODBPersistent
				ifFalse: [self makePersistent: each]
		]
	]! !

!FileImportParserContext publicMethodsFor: 'actions.basic'!

basicBeginSessionOn: aDatabase 
	self databaseInterface beginOLSessionOn: self database!

doIfInProperSession: properSessionBlock0 ifInImproperSession: improperSessionBlock0 ifInNoSession: noSessionBlock0 
	^[super
		doIfInProperSession: properSessionBlock0
		ifInImproperSession: improperSessionBlock0
		ifInNoSession: noSessionBlock0]
		valueNowOrOnUnwindDo: [self closeReferencedStreams]! !

!FileImportParserContext publicMethodsFor: 'actions.db.session'!

commit
	super commit.
	self 
		initializePrePersistentObjects;
		traceTimeStamp!

commitAndClean
	super commitAndClean.
	self 
		initializePrePersistentObjects;
		traceTimeStamp!

endSession
	super endSession.
	self initializePrePersistentObjects.
	Screen default ringBell!

rollback
	"We only wish to rollback if there is something to be rolled back."
	"self current rollback"

	self flushAllNonPersistentObjects.

	self hasPrePersistentObjects
		ifTrue: [self initializePrePersistentObjects].

	^super rollback! !

!FileImportParserContext publicMethodsFor: 'debugging'!

closeReferencedStreams
	self traceStreams do: [:each | each close]!

nextPutAll: aString 
	self traceStreams
		do: 
			[:each | 
			each == Transcript ifFalse: [each setToEnd].
			each nextPutAll: aString; flush]! !

!FileImportParserContext publicMethodsFor: 'initialize-release'!

defaultToDebugOn
	^true!

initialize
	super initialize.
	self initializePrePersistentObjects.
	self addTraceStream: ('d:parse' , Timestamp now asString , '.txt') asFilename writeStream!

initializePrePersistentObjects
	"self current initializePrePersistentObjects"

	prePersistentObjects := IdentityDictionary new! !

!FileImportParserContext publicMethodsFor: 'inquiries'!

hasPrePersistentObjects
	"self current hasPrePersistentObjects"

	^(prePersistentObjects
		detect: [:each | each value size > 0]
		ifNone: []
	) ~~ nil!

prePersistentObjectCount
	"self current prePersistentObjectCount"

	^prePersistentObjects
		inject: 0
		into: [:sum :each | sum + each value size]! !

!FileParser class publicMethodsFor: 'accessing'!

clientClassToParserMapping
	"Answer a mapping from the client classes to their FileParsers."
	"self clientClassToParserMapping"

	| dict |
	dict := IdentityDictionary new: 10.
	self allSubclasses do: 
		[:each | 
		| key |
		(key := each clientClass) == nil ifFalse: [dict at: key put: each]].
	^dict!

context
	"Answer the context in which my calculations should occur."

	^FileImportParserContext current!

currentUserID
	"Answer the login ID of the current user."

	^self context currentUserID!

delimiter
	"Answer the character(s) used to delimit the 
	attributes used by my file."

	^Character cr!

fileNotFoundSignal

	^OSErrorHolder nonexistentSignal!

parserToPrerequisiteParserMap
	"self parserToPrerequisiteParserMap"

	| dict clientToParserMap |
	clientToParserMap := FileParser clientClassToParserMapping.
	dict := IdentityDictionary new.
	self allSubclasses do: 
		[:each | 
		dict 
			at: each 
			put: (each prerequisiteParsersIn: clientToParserMap)].
	^dict!

prerequisiteParsers
	"CIAccountParser prerequisiteParsers"
	"CICommunicationPathParser prerequisiteParsers"

	^self prerequisiteParsersIn: FileParser clientClassToParserMapping!

prerequisiteParsersIn: aMapping
	"CIAccountParser prerequisiteParsers"
	"CICommunicationPathParser prerequisiteParsers"

	| set referencedClasses |
	set := IdentitySet new.
	referencedClasses := (self basicNew referencedClasses) 
			remove: self clientClass ifAbsent: nil; 
			yourself.
	referencedClasses do: 
		[:each | 
		set add: (aMapping at: each ifAbsent: nil)].
	^set asArray!

prerequisites

	| mapping |
	mapping := self clientClassToParserMapping.
	self basicNew classToRequiredDatabaseMapping keys.! !

!FileParser class publicMethodsFor: 'accessing.defaults'!

defaultFileSpecification
	"Answer the absolute file name, including its path."

	^(String new: 30) writeStream
		nextPutAll: self defaultFileDrive;
		nextPutAll: self defaultFilePath;
		nextPutAll: self defaultFileName;
		contents!

defaultToTraceOn
	"Answer whether parses for my instances should trace their output."

	^true!

fileName
	"Answer an instance of Filename that points to my file."
	"CIAccountParser fileName"

	| fileSpecification fileName |
	fileSpecification := self defaultFileSpecification.
	
	[fileName := fileSpecification asFilename.
	fileName exists
		ifTrue: [^fileName]
		ifFalse: [fileSpecification := self requestFileSpecFromUser: fileSpecification].
	fileSpecification size == 0 ifTrue: [^nil]] 
		repeat!

requestFileSpecFromUser: aSpec 
	"Answer a string as supplied by the user."
	"self requestFileSpecFromUser: 'c:\nowhere'"

	^Dialog 
		requestFileName: 'Where can I find the file representing:\' withCRs , self clientClass asString , 's?' 
		default: aSpec! !

!FileParser class publicMethodsFor: 'actions'!

parseFile
	"Parse the file using the default trace mode."

	^self parseFileTracing: self defaultToTraceOn!

parseFileTracing: aBoolean

	^aBoolean 
		ifTrue: [self parseFileWithTracing]
		ifFalse: [self parseFileWithoutTracing]!

parseFileWithoutTracing

	^self context doNotEvaluateDebugCodeWhile: [self new parseFile]!

parseFileWithTracing
	self context evaluateDebugCodeWhile: [self new parseFile]!

printTree

	| str |
	str := String new writeStream.
	self printTreeOn: str.
	^str contents!

printTreeOn: aStream
	"Transcript clear.
	CIAddressParser printTreeOn: Transcript.
	Transcript endEntry"

	self context doNotEvaluateDebugCodeWhile: 
		[self new printTreeOn: aStream]!

safeHalt
	"Halt if the 'alt' key is depressed."

	InputState default altDown
		ifTrue: [self halt].
	^nil!

traceTree

	self printTreeOn: Transcript.
	Transcript endEntry! !

!FileParser class publicMethodsFor: 'instance creation'!

new
	^self basicNew initialize! !

!FileParser class publicMethodsFor: 'override.accessing'!

attributeCount
	"Answer the number of attributes contained in each record 
	of the file.  This is only used by the utilities and not for 
	parsing (where this value is defined implicitly).  Utilities 
	dependent upon file structure will not work properly on 
	files whose records have a variable number of attributes."
	"Check whether the expected attributeCount equals the 
	attributeCount described by the parse tree defined by an 
	instance of this class."
	"(self attributeCount) = (self new rootNodeOn: attributeCount)"

	self subclassResponsibility!

clientClass
	"Answer the class that I should use to create instances."
	"Concrete subclasses should answer a class, Abstract 
	subclasses should answer nil."

	^nil!

clientCreator
	"Answer a block that can create new instances 
	of my client class."

	^[self clientClass new]!

defaultFileDrive
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
		with: $:!

defaultFileName
	"Answer the default name of the file that I should parse.
	This should only include path information that distinguishes 
	it from the other files that are being processed."
	"self defaultFileSpecification"
	"self editFile"

	self subclassResponsibility!

defaultFilePath
	"Answer the default for the name of the directory 
	in which I should look for my file.  This should 
	begin and end with a directory separator and 
	should not include the file name."

	^'\users\default\'! !

!FileParser class publicMethodsFor: 'utility'!

chooseClass
	"self chooseClass"

	| classes |
	classes := self withAllSubclasses.
	^Dialog 
		choose: 'Which one do you want?' 
		fromList: classes
		values: classes
		lines: 8 
		cancel: [nil]!

editFile
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
		ifFalse: [file edit]!

editSampleFromFile: aFile
	"Open a workspace on a sample taken from my file."

	ComposedTextView 
		open: (self fileSampleFrom: aFile) contents asValue
		label: self clientClass name, '--', aFile asString!

fileSample
	"Answer a sample from my file."

	^self fileSampleFrom: self fileName!

fileSampleFrom: aFile
	"Answer a sample taken from aFile."

	^self 
		fileSampleFrom: aFile 
		recordCount: 50!

fileSampleFrom: aFile recordCount: recordCount 
	"Answer a sample taken from aFile that includes recordCount complete records."

	| aStream file delimiter attributesRequestedCount |
	delimiter := self delimiter.
	attributesRequestedCount := recordCount * self attributeCount.
	aStream := ReadWriteStream on: (String new: attributesRequestedCount).
	file := aFile readStream.
	[attributesRequestedCount timesRepeat: [aStream nextPutAll: (file through: delimiter)]]
		valueNowOrOnUnwindDo: [file close].
	^aStream reset; yourself!

recordCount
	"Answer the number of records in my file."

	^Object subclassResponsibilitySignal 
		handle: [:ex | ex returnWith: 0]
		do: [self recordCountIn: self fileName]!

recordCountIn: aFile
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
	^(fieldCount * 1.0 / self attributeCount) ceiling!

typeMapping
	^Dictionary new!

typeTestFailures
	"self typeTestFailuresFor: self chooseClass"
	"self typeTestFailures"

	| typeMapping |
	typeMapping := self typeMapping.
	^self context asTransactionDo: 
		[self clientClass selectFromOdb select: 
			[:each | typeMapping contains: 
				[:assoc | (each instVarNamed: assoc key)
					isKindOf: assoc value]]]!

typeTestFailuresFor: aClass
	"self typeTestFailures"

	^aClass typeTestFailures! !

!FileParser class publicMethodsFor: 'utility.batch processing'!

batchProcessJobList
	"self batchProcessJobList"

	^(OrderedCollection new) 
		add: CIAccountParser; 
		add: CICommunicationPathParser; 
		add: CISpecialRateParser; 
		addAll: CIAccountItemParser batchProcessJobList;
		addAll: Deposit batchProcessJobList;
		add: CIAccountItemBilledAccountLinkParser;
"		addAll: Taxes batchProcessJobList;"
		yourself!

batchProcessJobListFromUser
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
	^batchList!

process: aJob 
	aJob ifDefinedDo: [:job | job parseFileTracing: true]!

processBatch
	"self processBatch"

	self processBatch: self batchProcessJobList!

processBatch: aList

	aList do: [:each | each parseFileTracing: true]!

processBatchFromUser
	"Allows the user to begin the batch processing at 
	any point in an ordered list of parsing jobs."
	"self processBatchFromUser"

	self processBatch: self batchProcessJobListFromUser!

processBatchThenQuit
	"self processBatchThenQuit"

	| list |
	list := self batchProcessJobList.
	list isEmpty 
		ifFalse: [self processBatchThenQuit: list]!

processBatchThenQuit: aBatch
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
			ObjectMemory quit]!

processBatchThenQuitFromUser
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
	list isEmpty ifFalse: [self processBatchThenQuit: list]!

processFromUser
	"Allows the user to select from an ordered list of parsing jobs."
	"self processBatchFromUser"

	self process: (self batchProcessJobListFromUser at: 1 ifAbsent: nil)! !

!FileParser class publicMethodsFor: 'utility.session'!

beginOLSession
	^self context beginOLSession!

beginSession
	^self context beginSession!

commit
	^self context commit!

connectToDatabaseFromUser
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
		ifFalse: [self context connectToDatabase: choice]!

databaseNameFromUserIfNone: block0
	"self databaseNameFromUserIfNone: [nil]"

	| choice |
	choice := Dialog request: 'Connect to what database (bprsvr4\is the default server)?' withCRs.
	^(choice includes: $@)
		ifTrue: [choice]
		ifFalse: 
			[choice isEmpty
				ifTrue: [block0 value]
				ifFalse: [choice := choice , '@bprsvr4']]!

disconnectFromDatabaseFromUser
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
		ifFalse: [self context disconnectFromDatabase: choice]!

endSession
	^self context endSession!

favoriteDBList
	^(OrderedCollection new) 
		add: 'chuckdb@bprsvr4'; 
		add: 'pauldb@bprsvr4'; 
		add: 'paulrefdb@bprsvr4'; 
		add: 'refdb1016@bprsvr4';
		yourself!

rollback
	^self context rollback! !

!FileParser class publicMethodsFor: 'utility.transaction management'!

manageInSessionTransactionFromUser
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
		ifFalse: [self perform: choice]!

manageNotInSessionTransactionFromUser
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
		ifFalse: [self perform: choice]!

manageTransactionFromUser
	"Allow the user to perform a variety of context-sensitive 
	session management options."
	"self manageTransactionFromUser"

	^self context isInSession
		ifTrue: [self manageInSessionTransactionFromUser]
		ifFalse: [self manageNotInSessionTransactionFromUser]! !

!FileParser publicMethodsFor: 'accessing'!

clientClass
	"Answer the class that I should use to create instances."

	^self class clientClass!

clientTypeDescriptor
	^self clientClass printString!

context
	^self class context!

debuggingBlock
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

			[self incrementRecordsDoneCountBy: self debugTraceIncrement.
			context newLineTraceWith: self recordsDoneCount asString , sizeString]]!

defaultFileSpecification

	^self class defaultFileSpecification!

estimatedCompletionTime

	^Time fromSeconds: (Time now asSeconds + self estimatedTimeRemaining)!

estimatedTimeRemaining

	^ArithmeticValue divisionByZeroSignal
		handle: [:ex | 0]
		do: 
			[| percentageComplete secondsPassed totalSeconds |
			secondsPassed := Time now asSeconds - self startTimeSeconds.
			percentageComplete := self recordsDoneCount / self recordCount.
			totalSeconds := secondsPassed / percentageComplete.
			totalSeconds - secondsPassed]!

fileName

	^self class fileName!

incrementRecordsDoneCountBy: aValue
	recordsDoneCount := recordsDoneCount + aValue!

newClient
	^self class clientCreator value!

persistentObjectNotFoundRegistrarFor: aRegistry 
	"Answer an object that can track requests of the 
	persistent store that come up empty."

	^[:aSummary | 
	| client |
	client := aSummary client.
	(aRegistry at: client class ifAbsentPut: IdentityDictionary new)
		at: client identityHash put: aSummary predicate]!

recordCount
	^recordCount!

recordCount: aValue
	recordCount := aValue!

recordsDoneCount
	^recordsDoneCount!

recordsDoneCount: aValue
	recordsDoneCount := aValue!

referencedClasses
	"Answer all of the classes that are directly referenced during this parsing."

	^self classToRequiredDatabaseMapping keys!

registrar
	^registrar!

registrar: aValue 
	registrar := aValue!

registry
	^registry!

registry: aValue 
	registry := aValue!

startTimeSeconds
	^startTimeSeconds!

startTimeSeconds: aValue
	startTimeSeconds := aValue!

supplementaryDatabases
	"Answer all of the databases that are used during this parsing."

	^self classToRequiredDatabaseMapping values! !

!FileParser publicMethodsFor: 'accessing.nodes'!

booleanNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self booleanNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self booleanNodeOn: aStream withWidth: self nodeWidth]!

characterNodeOn: aStream
	^self shouldUseDelimitedFormat
		ifTrue: [self characterNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self characterNodeOn: aStream withWidth: self nodeWidth]!

dateNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self dateNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self dateNodeOn: aStream withWidth: self nodeWidth]!

integerNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self integerNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self integerNodeOn: aStream withWidth: self nodeWidth]!

moneyNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self moneyNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self moneyNodeOn: aStream withWidth: self nodeWidth]!

nilAttributeNodeOn: aStream 

	^StreamAttributeParseNode 
		on: (self nilNodeOn: aStream) 
		stReader: [:client | ] 
		stWriter: [:client :value | ] 
		odbValueTest: ''!

nilNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self nilNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self nilNodeOn: aStream withWidth: self nodeWidth]!

realNumberNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self realNumberNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self realNumberNodeOn: aStream withWidth: self nodeWidth]!

stringNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self stringNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self stringNodeOn: aStream withWidth: self nodeWidth]!

timestampNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self timestampNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self timestampNodeOn: aStream withWidth: self nodeWidth]!

titleNodeOn: aStream 
	^self shouldUseDelimitedFormat
		ifTrue: [self titleNodeOn: aStream withDelimiter: self delimiter]
		ifFalse: [self titleNodeOn: aStream withWidth: self nodeWidth]! !

!FileParser publicMethodsFor: 'accessing.nodes.delemeted'!

booleanNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Boolean
		delimiter: aDelimiter!

characterNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Character
		delimiter: aDelimiter!

dateNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Date
		delimiter: aDelimiter!

delimiter
	"Answer the delimiter to be used 
	between attributes in a record."

	^self class delimiter!

integerNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Integer
		delimiter: aDelimiter!

moneyNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Money
		delimiter: aDelimiter!

nilNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: nil
		delimiter: aDelimiter!

realNumberNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #RealNumber
		delimiter: aDelimiter!

stringNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #String
		delimiter: aDelimiter!

timestampNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Timestamp
		delimiter: aDelimiter!

titleNodeOn: aStream withDelimiter: aDelimiter 
	^StreamBasicParseNode
		on: aStream
		forType: #Title
		delimiter: aDelimiter! !

!FileParser publicMethodsFor: 'accessing.nodes.fixedWidth'!

booleanNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Boolean
		width: aWidth!

characterNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Character
		width: aWidth!

dateNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Date
		width: aWidth!

integerNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Integer
		width: aWidth!

moneyNodeOn: aStream withWidth: aWidth 

	^StreamBasicParseNode
		on: aStream
		forType: #Money
		width: aWidth!

nilNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: nil
		width: aWidth!

nodeWidth
	"Answer the width to be used as a default 
	for node creation when none is provided."

	^10!

realNumberNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #RealNumber
		width: aWidth!

stringNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #String
		width: aWidth!

timestampNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Timestamp
		width: aWidth!

titleNodeOn: aStream withWidth: aWidth 
	^StreamBasicParseNode
		on: aStream
		forType: #Title
		width: aWidth! !

!FileParser publicMethodsFor: 'actions'!

asTransactionDo: block0 

	^self context asTransactionDo: 
		[self context 
			connectToDatabases: self supplementaryDatabases 
			while: block0]!

commit
	| myContext |
	myContext := self context.
	myContext persistAllNonPersistentObjects.
	self handleNonOLReadyClassWhile: [myContext commit].
	self context debugIsOn 
		ifTrue: 
			[self displayDatabaseToSizeMap.
			self context newLineTraceWith: 'Estimated completion time: ', self estimatedCompletionTime asString]!

databaseToSizeMap
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
	^map!

ensureClass: aClass hasSpecificDatabase: aDatabase 
	"This is done to avoid the Argos problem of forgetting 
	that we are dealing with a Versant database (and not 
	a memory database), and to ensure that new instances 
	of the class are created in the database that we wish 
	them to be, and not simply their default database."

	aClass 
		setDBMS: #VERSANT; 
		setSpecificDatabase: aDatabase!

handleNonOLReadyClassWhile: doBlock0 
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
		do: doBlock0!

handleNonOLReadyClassWith: handleBlock do: doBlock 
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
		do: doBlock!

parseFile
	"Establish the transaction, parse the file, and answer the registry of 
	unsatisfied requests."

	self 
		startTimeSeconds: Time now asSeconds;
		asTransactionDo: 
			[Cursor wait showWhile: 
				[self
					preParseFile;
					parseStreamOn: self fileName;
					postParseFile]];
		displayParsingReport.

	ObjectMemory garbageCollect.
	^self registry!

parseStreamOn: aFileName 
	"Create a stream on the file and parse it."

	| stream |
	(self context) 
		newLineTraceWith: 'Parsing file for ' , self clientTypeDescriptor; 
		newLineTraceWith: 'beginning...'.

	[stream := aFileName readStream.
	self 
		preParseStream: stream;
		parseStream: stream usingClientCreator: (self rootNodeOn: stream);
		postParseStream: stream]
			valueNowOrOnUnwindDo: [stream close]!

postParseStream: aStream 
	"do whatever work is postrequisite to parsing the file"

	"do nothing"!

preParseStream: aStream 
	"do whatever work is prerequisite to parsing the file"

	self syncClassDefinitionsToDatabaseMappings!

printTreeOn: aStream 
	"Print a textual description of the parse tree on aStream."

	^(self rootNodeOn: nil)
		printTreeOn: aStream!

safeHalt
	^self class safeHalt!

syncClassDefinitionsToDatabaseMappings
	"For each of the classes used in this parsing, set its specific 
	database attribute to that database, if necessary."

	| commitAfterward |
	commitAfterward := false.
	self classToRequiredDatabaseMapping keysAndValuesDo: 
		[:aClass :db | 
		self ensureClass: aClass hasSpecificDatabase: db.
		(aClass isDefinedOn: db) ifFalse: 
			[self context 
				do: [aClass describeToOdb: db]
				newLineTraceWith: 'Describing ' , aClass name , ' to ' , db.
			commitAfterward := true]].
	commitAfterward ifTrue: [self context commit]! !

!FileParser publicMethodsFor: 'override.accessing'!

classToRequiredDatabaseMapping
	"Answer a mapping from each of the classes referred to by this node 
	to the databases in which they can be found."
	"Subclasses should extend this."

	| dict |
	dict := IdentityDictionary new.
	self clientClass ifDefinedDo: 
		[:aClass | dict at: aClass put: self database].
	^dict!

database
	"Answer the database to be used for read and write operations on 
	instances of my client class."

	^self context database!

debugTraceIncrement
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

	^self maxInMemoryClientCount / 5.0!

maxInMemoryClientCount
	"Answer the total number of clients permitted to exist in memory.  This 
	number should be proportional to the size of a typical client.  When this 
	boundary is reached, the clients are committed and removed from memory.  
	If set too high there will not be enough memory to support all of the tasks 
	and garbage collection thrashing will occur, increasing the processing time.  
	If set too low the commits will happen more frequently than they need to, 
	increasing the processing time."

	^3000!

referenceDatabase
	"Answer the database that holds the reference data."

	^'paulrefdb@bprsvr4'! !

!FileParser publicMethodsFor: 'override.accessing.nodes'!

rootNodeOn: aStream 
	"Answer an object that can create new clients."
	"Subclasses should override this to add attributes and relations. 
	Make sure to call this method (via 'super') first."

	^self rootNodeType on: [self preInitializeNewDomainObject: self newClient]!

rootNodeType

	^StreamDescriptionParseNode! !

!FileParser publicMethodsFor: 'override.actions'!

postParseFile
	"do nothing"!

preParseFile
	"Do whatever work is prerequisite to parsing the file.  
	Make sure that any subimplementors also call 'super'."

	| myDatabase |
	self context initialize.
	self recordCount: self class recordCount.

	"Here we force every domain object to point to my database.
	Later we may change which specific database they use, but 
	we must be sure not to miss any so we cover them all now."
	myDatabase := self database.
	Concept allSubclasses do: [:each | 
		self ensureClass: each hasSpecificDatabase: myDatabase]! !

!FileParser publicMethodsFor: 'override.initialize-release'!

initialize
	registry := IdentityDictionary new.
	registrar := self persistentObjectNotFoundRegistrarFor: registry.
	recordCount := nil.
	recordsDoneCount := nil!

postInitializeNewDomainObject: aDomainObject
	"Do whatever is necessary to further prepare objects 
	that have been read from the file.  This is a good place 
	to set any constant values in the domain object.  If you 
	override this in the subclass, make sure to evaluate this 
	method after you evaluate the new one."

	^self context makePersistent: aDomainObject!

preInitializeNewDomainObject: aDomainObject 
	"Do whatever is necessary to prepare objects 
	whose attributes will be read from the file."

	^self context addPrePersistentObject: aDomainObject! !

!FileParser publicMethodsFor: 'override.testing'!

shouldUseDelimitedFormat
	"Answer whether the records in my file are delimited."

	^true! !

!FileParser publicMethodsFor: 'printing'!

displayDatabaseToSizeMap
	| myContext |
	myContext := self context.
	self databaseToSizeMap keysAndValuesDo: 
		[:name :size | myContext newLineTraceWith: name , ' size is ' , size asString]!

displayParsingReport

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
		print: (self recordCount * 60 / processingTime asSeconds) asFloat;
		nextPutAll: ' records/minute.'.
	self context 
		traceWith: str contents
		isNewLine: true! !

!FileParser publicMethodsFor: 'private'!

parseStream: aStream usingClientCreator: aCreator 
	| debugCycle debugBlock debugCyclesPerCommit |
	debugCycle := self debugTraceIncrement.
	debugBlock := self debuggingBlock.
	debugCyclesPerCommit := (self maxInMemoryClientCount / debugCycle) floor max: 1.

	self displayDatabaseToSizeMap.
	self context persistentObjectNotFoundSignal 
		handle: 
			[:ex | 
			self registrar value: ex parameter.
			self context newLineTraceWith: 'Persistent object not found.'.
			ex proceed]
		do: 
			[[debugCyclesPerCommit timesRepeat: 
				[debugCycle timesRepeat: 
					[aStream atEnd
						ifTrue: [^self commit; yourself]
						ifFalse: [self postInitializeNewDomainObject: aCreator value]].
				debugBlock value].
			self commit] repeat]! !

!FileParser publicMethodsFor: 'testing'!

databaseSize
	"Answer the size in megabytes of my database."

	^self context databaseSize!

shouldUseFixedWidthFormat
	^self shouldUseDelimitedFormat not! !

!IntegerParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Integer! !

!IntegerParseNode publicMethodsFor: 'accessing'!

clientClass

	^Integer! !

!MoneyParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Money! !

!MoneyParseNode publicMethodsFor: 'accessing'!

value
	"Answer the money value that I represent.
	To avoid rounding errors I answer money as 
	number of cents."

	^(super value * 10) asInteger! !

!NullParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^nil! !

!NullParseNode publicMethodsFor: 'accessing'!

dataType
	^#nil!

delimitedValue
	super delimitedValue.
	^nil!

scalarDelimitedValue

	self sourceStream skipThrough: self delimiter.
	^nil!

vectorDelimitedValue

	self sourceStream 
		skipToAll: self delimiter;
		skip: self delimiter size.
	^nil!

widthValue
	"Ignore the information stored in the next field."

	self sourceStream skip: self width.
	^nil! !

!NumberParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Number! !

!NumberParseNode publicMethodsFor: 'accessing'!

clientClass
	^Number!

value
	"Answer the number value that I represent."

	^self clientClass readFrom: super value readStream! !

!Object publicMethodsFor: 'accessing'!

attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeValueParseNode!

value
	"Answer the value that I represent.  This is necessary for polymorphism."

	^self! !

!Object publicMethodsFor: 'testing'!

ifDefinedDo: block1 
	^block1 value: self!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^false! !

!PersistentObjectNotFoundSummary class publicMethodsFor: 'instance creation'!

client: aClient initializer: anInitializer predicate: aPredicate 
	^(self new) 
		client: aClient;
		initializer: anInitializer;
		predicate: aPredicate! !

!PersistentObjectNotFoundSummary publicMethodsFor: 'accessing'!

client
	^client!

client: aClient
	client := aClient!

initializer
	^initializer!

initializer: aValue 
	initializer := aValue!

predicate
	^predicate!

predicate: aValue 
	predicate := aValue! !

!PersistentObjectNotFoundSummary publicMethodsFor: 'initialize-release'!

initialize! !

!RealNumberParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#RealNumber! !

!RealNumberParseNode publicMethodsFor: 'accessing'!

clientClass
	^LimitedPrecisionReal! !

!SmallInteger publicMethodsFor: 'testing'!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true! !

!StreamAttributeParseNode class publicMethodsFor: 'instance creation'!

on: aValue slotName: aName 
	self subclassResponsibility!

on: aValue stReader: block1 stWriter: block2 odbReferenceTest: odbQuery odbVariableName: aVariableName 

	^(StreamAttributeReferenceParseNode on: aValue)
		stReader: block1
		stWriter: block2
		odbReferenceTest: odbQuery
		odbVariableName: aVariableName!

on: aValue stReader: block1 stWriter: block2 odbValueTest: odbQuery 

	^(StreamAttributeValueParseNode on: aValue)
		stReader: block1
		stWriter: block2
		odbEquivalenceTest: odbQuery! !

!StreamAttributeParseNode class publicMethodsFor: 'private'!

stAccessorForSlotNamed: aName
	"self stAccessorForSlotNamed: 'foo'"

	"| blockSource |
	blockSource := (String new: 30) writeStream
		nextPutAll: '[:client | client ';
		nextPutAll: aName;
		nextPut: $];
		contents.
	^Compiler evaluate: blockSource"

	^aName asSymbol!

stMutatorForSlotNamed: aName
	"self stMutatorForSlotNamed: 'foo'"

	"| blockSource |
	blockSource := (String new: 30) writeStream
		nextPutAll: '[:client :newValue | client ';
		nextPutAll: aName;
		nextPutAll: ': newValue]';
		contents.
	^Compiler evaluate: blockSource"

	^(aName, ':') asSymbol! !

!StreamAttributeParseNode publicMethodsFor: 'accessing'!

addQueryTo: aCollection keyTo: aKeyCollection valueTo: aValueCollection 
	^aCollection add: self odbReader!

lastValue
	^lastValue!

lastValue: aValue
	lastValue := aValue!

odbReader
	^odbReader!

odbReader: aValue
	odbReader := aValue!

odbVariableName
	^nil!

slotName
	^self stReader isSymbol
		ifTrue: [self stReader]
		ifFalse: [nil]!

stReader
	^stReader!

stReader: aValue
	stReader := aValue!

stReader: block1 stWriter: block2 odbReader: odbQuery 
	self 
		stReader: block1;
		stWriter: block2;
		odbReader: odbQuery!

stWriter
	^stWriter!

stWriter: aValue
	stWriter := aValue!

value
	"Store then answer the result of 1 computational cycle of my machinery."

	| myValue |
	self lastValue: (myValue := super value).
	^myValue! !

!StreamAttributeParseNode publicMethodsFor: 'converting'!

asAttributeNamed: aName

	^self error: 'A ', self class name, ' cannot be converted to an attribute node.'! !

!StreamAttributeParseNode publicMethodsFor: 'initialize-release'!

initializeClient: aClient with: aValue
	self stWriter 
		value: aClient
		value: aValue! !

!StreamAttributeParseNode publicMethodsFor: 'printing'!

printOdbReaderOn: aStream 
	^aStream 
		nextPutAll: self odbReader;
		yourself!

printOn: aStream
	aStream
		nextPut: $?! !

!StreamAttributeParseNode publicMethodsFor: 'testing'!

needsOdbParameter
	"Answer whether the query that I represent has a 
	replaceable parameter."

	^false! !

!StreamAttributeReferenceParseNode class publicMethodsFor: 'instance creation'!

on: aValue slotName: aName

	| aNameString |
	aNameString := aName asString copy.
	^self 
		on: aValue 
		stReader: (self stAccessorForSlotNamed: aName) 
		stWriter: (self stMutatorForSlotNamed: aName) 
		odbReferenceTest: aNameString, ' = :', aNameString
		odbVariableName: aName! !

!StreamAttributeReferenceParseNode publicMethodsFor: 'accessing'!

addQueryTo: aCollection keyTo: aKeyCollection valueTo: aValueCollection 
	aKeyCollection add: self odbVariableName.
	aValueCollection add: lastValue.
	^super
		addQueryTo: aCollection
		keyTo: aKeyCollection
		valueTo: aValueCollection!

odbVariableName
	^odbVariableName!

stReader: block1 stWriter: block2 odbReferenceTest: odbQuery odbVariableName: aVariableName 
	self
		stReader: block1
		stWriter: block2
		odbReader: odbQuery.
	odbVariableName := aVariableName! !

!StreamAttributeReferenceParseNode publicMethodsFor: 'printing'!

printOn: aStream
	super printOn: aStream.
	aStream
		print: self odbReader! !

!StreamAttributeReferenceParseNode publicMethodsFor: 'testing'!

needsOdbParameter
	"Answer whether the query that I represent has a 
	replaceable parameter."

	^true! !

!StreamAttributeValueParseNode class publicMethodsFor: 'instance creation'!

on: aValue slotName: aName

	^self 
		on: aValue 
		stReader: (self stAccessorForSlotNamed: aName) 
		stWriter: (self stMutatorForSlotNamed: aName) 
		odbValueTest: aName copy, ' = '! !

!StreamAttributeValueParseNode publicMethodsFor: 'accessing'!

genericOdbReader
	^genericOdbReader!

genericOdbReader: aValue
	genericOdbReader := aValue!

odbReader
	"Since the syntax of my odb query depends upon my value, 
	I make that adjustment now."

	^odbReader == nil
		ifTrue: [odbReader := (self printOdbReaderOn: String new writeStream) contents]
		ifFalse: [odbReader]!

stReader: block1 stWriter: block2 odbEquivalenceTest: odbQuery 
	self
		stReader: block1
			stWriter: block2
			odbReader: '';
		genericOdbReader: odbQuery!

value
	"Answer the result of 1 computational cycle of my machinery."

	self odbReader: nil.	"invalidate my cached odbReader"
	^super value! !

!StreamAttributeValueParseNode publicMethodsFor: 'printing'!

printOdbReaderOn: aStream 
	^aStream 
		nextPutAll: self genericOdbReader; 
		print: self lastValue;
		yourself!

printOn: aStream

	super printOn: aStream.
	self genericOdbReader == nil
		ifTrue: [aStream print: self stReader]
		ifFalse: [aStream print: self genericOdbReader]! !

!StreamBasicParseNode class publicMethodsFor: 'accessing'!

defaultDelimiter
	"Answer the delimiter I should assume when I 
	am not provided one."

	^Character cr! !

!StreamBasicParseNode class publicMethodsFor: 'instance creation'!

on: aReadStream forType: aDataType
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
		]!

on: aReadStream forType: aDataType delimiter: aDelimiter 
	"Answer a a delimited node specialized for dealing 
	with aDataType."

	^(self on: aReadStream forType: aDataType)
		delimiter: aDelimiter!

on: aReadStream forType: aDataType width: anInteger 
	"Answer a fixed-width node specialized for dealing 
	with aDataType."

	^(self on: aReadStream forType: aDataType)
		width: anInteger! !

!StreamBasicParseNode class publicMethodsFor: 'utility'!

classForType: aDataType 
	"Answer the subclass that is designed to handle data of type 'aDataType'."

	^self allSubclasses 
		detect: [:each | each dataType == aDataType]
		ifNone: []! !

!StreamBasicParseNode publicMethodsFor: 'accessing'!

attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeValueParseNode!

component
	"Answer the node that provides my input."
	"For database storage reasons the string needs to 
	be a ByteString"

	^ByteString new: self width!

components
	^OrderedCollection new!

dataType
	^self class dataType!

delimitedValue

	^self delimiter size > 1
		ifTrue: [self vectorDelimitedValue]
		ifFalse: [self scalarDelimitedValue]!

delimiter
	^delimiter!

delimiter: aValue

	aValue == nil
		ifTrue: [delimiter := nil]
		ifFalse: [
			delimiter := aValue.
			self width: nil
		]!

scalarDelimitedValue
	^self sourceStream upTo: self delimiter!

sourceStream
	^sourceStream!

sourceStream: aReadStream
	sourceStream := aReadStream!

value
	"Answer the string value that I represent."

	^self shouldUseDelimitedFormat
		ifTrue: [self delimitedValue]
		ifFalse: [self widthValue]!

vectorDelimitedValue

	| myDelimiter myStream value |
	myStream := self sourceStream.
	myDelimiter := self delimiter.
	value := myStream upToAll: myDelimiter.
	myStream skip: myDelimiter size.
	^value!

width

	^width!

width: aValue

	aValue == nil
		ifTrue: [width := nil]
		ifFalse: [
			width := aValue.
			self delimiter: nil
		]!

widthValue
	"Answer the value I represent, constraining 
	the number of characters."

	^self sourceStream 
		next: self width
		into: super value
		startingAt: 1! !

!StreamBasicParseNode publicMethodsFor: 'inquiries'!

attributeCount
	"Answer the count of attributes of which I am composed.  I am 
	exactly 1 attribute."

	^1!

mapsDirectlyToFile
	^true!

shouldUseDelimitedFormat
	^self delimiter ~~ nil!

shouldUseFixedWidthFormat
	^self shouldUseDelimitedFormat not! !

!StreamBasicParseNode publicMethodsFor: 'printing'!

printOn: aStream

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
		]!

printOn: aStream level: aLevel

	aStream
		print: self! !

!StreamBranchParseNode class publicMethodsFor: 'instance creation'!

ifNone: block1 
	^self new ifNone: block1!

on: aValue ifNone: block1 

	^(self on: aValue)
		ifNone: block1! !

!StreamBranchParseNode publicMethodsFor: 'accessing'!

components
	^super components 
		addAll: self values;
		yourself!

keys

	^self conditions collect: [:each | each key]!

translationOf: aValue ifNone: block0 
	"Answer the parsedValue of the node whose test condition 
	is satisfied by my parsedValue."

	^(self conditions
		detect: [:cond | cond key value: aValue]
		ifNone: [^block0 value]) value value!

values

	^self conditions collect: [:each | each value]! !

!StreamBranchParseNode publicMethodsFor: 'actions'!

canUseEquivalenceTestOn: aValue

	^aValue isCharacter 
		or: [aValue isSmallInteger
		or: [aValue == true
		or: [aValue == false
		or: [aValue isNil
		or: [aValue isSymbol]]]]]!

mapIfEquals: aValue to: aNode 

	| satisfactionBlock |
	satisfactionBlock := aValue isImmediateValue
		ifTrue: [ [:parsedValue | aValue == parsedValue] ]
		ifFalse: [ [:parsedValue | aValue = parsedValue] ].

	self 
		mapIfSatisfies: satisfactionBlock 
		to: aNode!

mapIfSatisfies: block1 to: aNode 
	conditions add: block1 -> aNode! !

!StreamBranchParseNode publicMethodsFor: 'initialize-release'!

initialize

	super initialize.
	conditions := OrderedCollection new! !

!StreamBranchParseNode publicMethodsFor: 'inquiries'!

attributeCount
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
		]!

expectedValues
	^self conditions asArray
		collect: [:each | each copiedValues]! !

!StreamBranchParseNode publicMethodsFor: 'printing'!

printOn: aStream level: aLevel

	self
		printValue: 'Branch: '
			on: aStream 
			level: aLevel;
		printSubcomponents: (Array with: self component)
			on: aStream 
			level: aLevel + 1;
		printValue: 'Values: '
			on: aStream 
			level: aLevel!

printSubcomponentsOn: aStream level: aLevel

	self
		printSubcomponents: self values
			on: aStream 
			level: aLevel! !

!StreamComponentParseNode class publicMethodsFor: 'instance creation'!

on: aValue
	^self new on: aValue! !

!StreamComponentParseNode publicMethodsFor: 'accessing'!

component
	"Answer the node that provides my input."

	^component!

component: aValue
	component := aValue! !

!StreamComponentParseNode publicMethodsFor: 'initialize-release'!

on: aValue
	self component: aValue! !

!StreamCompositionParseNode class publicMethodsFor: 'instance creation'!

on: one with: two

	^(self on: one)
		add: two;
		yourself!

on: one with: two with: three

	^(self on: one)
		add: two;
		add: three;
		yourself!

on: one with: two with: three with: four

	^(self on: one)
		add: two;
		add: three;
		add: four;
		yourself! !

!StreamCompositionParseNode publicMethodsFor: 'accessing'!

add: aComponent 
	"Add a component node to my collection."

	^self components add: aComponent!

components
	^components!

components: aCollection
	components := aCollection!

value
	"Answer the value of my valueBlock evaluated with 
	each of my components' values."

	| str |
	str := (Array new: (self components size + 1)) writeStream.
	str nextPut: self component value.
	self components do: [:each | str nextPut: each value].
	^self valueBlock valueWithArguments: str contents!

valueBlock
	^valueBlock!

valueBlock: aBlock
	valueBlock := aBlock! !

!StreamCompositionParseNode publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	components := OrderedCollection new.
	valueBlock := []! !

!StreamDescriptionParseNode publicMethodsFor: 'accessing'!

attributes
	^attributes!

attributes: aValue
	attributes := aValue!

components
	^super components 
		addAll: self attributes;
		yourself!

value
	^self processNewComponent: super value! !

!StreamDescriptionParseNode publicMethodsFor: 'actions'!

processNewComponent: aValue 
	^self initializeNewComponent: aValue! !

!StreamDescriptionParseNode publicMethodsFor: 'addition/removal'!

add: aValue 
	^self attributes add: aValue!

at: aKey put: aValue 

	^self add: (aValue asAttributeNamed: aKey)!

remove: aValue
	^self attributes remove: aValue! !

!StreamDescriptionParseNode publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	attributes := OrderedCollection new!

initializeNewComponent: aComponent 
	self attributes do: [:attrib | attrib initializeClient: aComponent with: attrib value].
	^aComponent! !

!StreamDescriptionParseNode publicMethodsFor: 'printing'!

printOn: aStream level: aLevel

	"do nothing"! !

!StreamEnumeratedTypeParseNode class publicMethodsFor: 'instance creation'!

on: aNode enumeration: aCollection

	^(self on: aNode)
		conditions: aCollection!

on: aNode enumeration: aCollection ifNone: block1 
	^(self on: aNode enumeration: aCollection)
		ifNone: block1! !

!StreamEnumeratedTypeParseNode publicMethodsFor: 'accessing'!

conditions
	^conditions!

conditions: aValue 
	conditions := aValue!

ifNone
	^self translator!

ifNone: aBlock
	self translator: aBlock!

translationOf: aValue ifNone: block0

	^(self conditions includes: aValue)
		ifTrue: [aValue]
		ifFalse: [block0 value]! !

!StreamEnumeratedTypeParseNode publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	self ifNone: [:key | self unexpectedValueSignal raiseWith: key]! !

!StreamEnumeratedTypeParseNode publicMethodsFor: 'printing'!

printOn: aStream level: aLevel

	"do nothing"!

printSubcomponentsOn: aStream level: aLevel

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
		level: aLevel! !

!StreamLoopFeedbackParseNode class publicMethodsFor: 'documentation'!

testNode
	"self testNode"
	"| node oc |
	node := self testNode.
	oc := OrderedCollection new.
	5 timesRepeat: [oc add: node value].
	oc"

	^self on: [ [7 asValue] asValue ]! !

!StreamLoopFeedbackParseNode publicMethodsFor: 'accessing'!

value
	"Answer the value that I represent, and recycle that value."

	| myValue |
	myValue := super value.
	self component: myValue.
	^myValue! !

!StreamParseNode class publicMethodsFor: 'accessing'!

dataType
	^#Object!

unexpectedValueSignal
	"Answer the signal used when the value found in my
	text field is somehow unexpected."

	UnexpectedValueSignal == nil
		ifTrue: [self initializeSignals].
	^UnexpectedValueSignal! !

!StreamParseNode class publicMethodsFor: 'class initialization'!

initialize
	"self initialize"

	self initializeSignals!

initializeSignals
	UnexpectedValueSignal := (Object errorSignal newSignalMayProceed: true)
		notifierString: 'An unexpected value has been found in this field';
		nameClass: self message: #unexpectedValueErrorSignal! !

!StreamParseNode class publicMethodsFor: 'instance creation'!

new
	^self basicNew initialize! !

!StreamParseNode publicMethodsFor: 'accessing'!

attributeClass
	"Answer the class of the attribute wrapper that I require."

	^self component attributeClass!

component
	"Answer the node that provides my input."

	self subclassResponsibility!

components
	^OrderedCollection with: self component!

context
	"Answer the context in which my calculations should occur."

	^FileImportParserContext current!

unexpectedValueSignal
	^self class unexpectedValueSignal!

value
	"Answer the result of 1 computational cycle of my machinery."

	^self component value! !

!StreamParseNode publicMethodsFor: 'converting'!

asAttributeNamed: aName 
	"Answer myself wrapped with an attribute node."

	^self attributeClass 
		on: self 
		slotName: aName asSymbol! !

!StreamParseNode publicMethodsFor: 'debugging'!

asPostValueHaltNode
	"Answer myself wrapped with a node that will halt after I am evaluated."

	^StreamComponentParseNode on: [self value; halt; yourself]!

asPostValueHaltNode: test1 
	"Answer myself wrapped with a node that will halt after I am evaluated 
	if test1 on myself is true."

	^StreamComponentParseNode
		on: 
			[| myValue |
			myValue := self value.
			(test1 value: myValue)
				ifTrue: [self halt].
			myValue]!

asPreValueHaltNode
	"Answer myself wrapped by a node that will halt before I am evaluated."

	^StreamComponentParseNode on: [self halt value]!

asPreValueHaltNode: block0 
	"Answer myself wrapped by a node that will halt 
	before I am evaluated if the value of block0 is true."

	^StreamComponentParseNode
		on: 
			[block0 value ifTrue: [nil halt].
			self value]! !

!StreamParseNode publicMethodsFor: 'enumerating'!

allComponents

	| oc |
	oc := OrderedCollection new.
	self allComponentsDo: [:each |
		oc add: each
	].
	^oc!

allComponentsDo: block1

	self components do: [:each |
		block1 value: each.
		each isParseNode
			ifTrue: [each allComponentsDo: block1]
	]! !

!StreamParseNode publicMethodsFor: 'initialize-release'!

initialize
	"do nothing"! !

!StreamParseNode publicMethodsFor: 'inquiries'!

attributeCount
	"Answer the numer of attributes of which I am composed."

	^self components 
		inject: 0 
		into: 
			[:count :each | each species == StreamParseNode
				ifTrue: [count + each attributeCount]
				ifFalse: [count]]!

isParseNode
	^true!

mapsDirectlyToFile
	^false!

species
	^StreamParseNode! !

!StreamParseNode publicMethodsFor: 'printing'!

printOn: aStream level: aLevel

	aStream
		crtab: aLevel;
		print: self!

printSubcomponents: subcomponents on: aStream level: aLevel

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
	aStream nextPut: $)!

printSubcomponentsOn: aStream level: aLevel

	| mySubcomponents |
	mySubcomponents := self components.
	mySubcomponents isEmpty
		ifFalse: [
			self 
				printSubcomponents: mySubcomponents
				on: aStream 
				level: aLevel
		]!

printTree

	| str |
	str := WriteStream on: String new.
	self printTreeOn: str.
	^str contents!

printTreeOn: aStream 

	self 
		printTreeOn: aStream 
		level: 0!

printTreeOn: aStream level: aLevel

	self 
		printOn: aStream 
			level: aLevel;
		printSubcomponentsOn: aStream
			level: aLevel + 1!

printValue: aValue on: aStream level: aLevel 

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
		]!

traceTree
	self printTreeOn: Transcript.
	Transcript endEntry! !

!StreamPersistentObjectCachedDescriptionNode publicMethodsFor: 'accessing'!

add: aValue
	^self getClientCache add: aValue!

value
	"Add the newly retrieved element to the cache, and answer it."
	"Since the cache is an IdentitySet it does not matter if we add 
	an element that is already in the cache."

	^self add: super value! !

!StreamPersistentObjectCachedDescriptionNode publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	self initializeClientCache!

initializeClientCache
	clientCache := IdentitySet new! !

!StreamPersistentObjectCachedDescriptionNode publicMethodsFor: 'private'!

cachedInstancesOf: newClient
	"Answer the elements in the cache completely described 
	by my attributes"

	^self getClientCache reject: 
		[:aClient | 
		self attributes contains: 
			[:attrib | (attrib stReader value: aClient) ~= (attrib lastValue)]]!

cachedInstancesOf: newClient ifNone: block0 
	"Answer the elements in the cache completely described 
	by my attributes, or if there are none, the value of the ifNone block."

	| instances |
	^(instances := self cachedInstancesOf: newClient) isEmpty
		ifTrue: [block0 value]
		ifFalse: [instances]!

getClientCache
	^clientCache!

persistentInstancesOf: newClient 
	"Answer the elements in the cache completely described 
	by my attributes, or if there are none, try in the database."

	^self 
		cachedInstancesOf: newClient 
		ifNone: [super persistentInstancesOf: newClient]! !

!StreamPersistentObjectDescriptionNode class publicMethodsFor: 'instance creation'!

on: aNode database: aDatabase
	^(self on: aNode)
		database: aDatabase!

on: aNode database: aDatabase ifFound: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifFound: block!

on: aNode database: aDatabase ifFound: block1 ifNone: block2
	^(self 
		on: aNode
		database: aDatabase
	)
		ifFound: block1;
		ifNone: block2!

on: aNode database: aDatabase ifMany: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifMany: block!

on: aNode database: aDatabase ifMany: manyBlock2 ifFound: foundBlock1 ifNone: block2
	^(self 
		on: aNode
		database: aDatabase
	)
		ifMany: manyBlock2;
		ifFound: foundBlock1;
		ifNone: block2!

on: aNode database: aDatabase ifNone: block
	^(self 
		on: aNode
		database: aDatabase
	)
		ifNone: block!

on: aNode ifFound: block
	^(self on: aNode)
		ifFound: block!

on: aNode ifNone: block
	^(self on: aNode)
		ifNone: block! !

!StreamPersistentObjectDescriptionNode publicMethodsFor: 'accessing'!

attributeClass
	"Answer the class of the attribute wrapper that I require."

	^StreamAttributeReferenceParseNode!

database
	^database!

database: aDatabase
	database := aDatabase!

ifFound
	^ifFoundBlock!

ifFound: aValue
	ifFoundBlock := aValue!

ifMany
	^ifManyBlock!

ifMany: aBlock 
	ifManyBlock := aBlock!

ifNone
	^ifNoneBlock!

ifNone: aValue
	ifNoneBlock := aValue!

newComponentInitializer
	^[:aClient | self initializeNewComponent: aClient]!

predicate
	^predicate!

predicate: aValue
	predicate := aValue!

shouldUseSubclasses
	^shouldUseSubclasses!

shouldUseSubclasses: aValue
	shouldUseSubclasses := aValue!

useIfNoneThenNilBlock
	self ifNone: self ifNoneThenNilBlock!

useIfNoneThenPrototypeBlock
	self ifNone: self ifNoneThenPrototypeBlock! !

!StreamPersistentObjectDescriptionNode publicMethodsFor: 'actions'!

processNewComponent: aValue 
	"Answer the instance of aValue that is uniquely 
	described by my attributes."

	"evaluate each of the attributes first for consistency"
	self attributes do: [:each | each value].
	^self singleInstanceOf: aValue! !

!StreamPersistentObjectDescriptionNode publicMethodsFor: 'initialize-release'!

ifNoneThenNilBlock
	^[:client :initializer | 
	self persistentObjectNotFoundBlock 
		value: client 
		value: initializer.
	self context traceWith: '  Proceeding with ''nil''.'.
	nil]!

ifNoneThenPrototypeBlock
	^[:client :initializer | 
	self persistentObjectNotFoundBlock 
		value: client 
		value: initializer.
	self context traceWith: '  Proceeding with prototype.'.
	initializer value: client]!

initialize
	super initialize.
	database := self context database.
	ifManyBlock := [:client :collection | collection].
	ifFoundBlock := #yourself.
	ifNoneBlock := self ifNoneThenPrototypeBlock.
	shouldUseSubclasses := true!

initializeNewComponent: aClient 
	self attributes do: 
		[:each | 
		each 
			initializeClient: aClient 
			with: each lastValue].
	aClient becomePersistentIn: aClient class specificDatabase.
	^aClient!

persistentObjectNotFoundBlock
	^[:client :initializer | 
	self context persistentObjectNotFoundSignal 
		raiseRequestWith: 
			(PersistentObjectNotFoundSummary
				client: client
				initializer: initializer
				predicate: self predicate)]! !

!StreamPersistentObjectDescriptionNode publicMethodsFor: 'utility'!

handleUndefinedClassWith: handleBlock do: doBlock 
	^VError errorSignal 
		handle: 
			[:ex | 
			(VError errorSymbol == #VSI_WARNING and: [VError description = 'Class not found'])
				ifTrue: [handleBlock value: ex]
				ifFalse: [ex reject]]
		do: doBlock!

persistentInstancesOf: newClient 
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
			newClient class selectFrom: self database where: myPredicate]!

searchStringFrom: aQueryList

	| str |
	str := (String new: 100) writeStream.
	aQueryList 
		do: [:each | str nextPutAll: each]
		separatedBy: [str nextPutAll: ' & '].
	^str contents!

singleElementFrom: aCollection ifMany: manyBlock ifFound: foundBlock ifNone: noneBlock
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
					hits someElement]]!

singleInstanceOf: newClient 

	| persistentInstances initializer |
	persistentInstances := self persistentInstancesOf: newClient.
	initializer := self newComponentInitializer.
	^self 
		singleElementFrom: persistentInstances
		ifMany: [self ifMany value: (initializer value: newClient) value: persistentInstances]
		ifFound: self ifFound
		ifNone: [self ifNone value: newClient value: initializer]! !

!StreamPersistentObjectLimitedCachedDescriptionNode publicMethodsFor: 'accessing'!

add: aValue 
	
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
	^aValue!

maxSize
	^maxSize!

maxSize: aValue
	maxSize := aValue.
	self recalibrate! !

!StreamPersistentObjectLimitedCachedDescriptionNode publicMethodsFor: 'actions'!

recalibrate
	"If necessary, reduce the cache size to maxSize by 
	removing the oldest elements."

	(queryHistory size - self maxSize) 
		timesRepeat: [clientCache remove: (queryHistory removeLast)]! !

!StreamPersistentObjectLimitedCachedDescriptionNode publicMethodsFor: 'initialize-release'!

initializeClientCache
	super initializeClientCache.
	queryHistory := OrderedCollection new! !

!StreamPersistentObjectLimitedCachedDescriptionNode publicMethodsFor: 'private'!

getQueryHistory
	^queryHistory! !

!StreamTranslationParseNode class publicMethodsFor: 'instance creation'!

on: aGenerator translator: block1 
	^(self on: aGenerator)
		translator: block1! !

!StreamTranslationParseNode publicMethodsFor: 'accessing'!

translationOf: aValue ifNone: block0
	^block0 value!

translator
	^translator!

translator: block1
	translator := block1!

value
	"Answer the translation of my super value."

	| myValue |
	myValue := super value.
	^self 
		translationOf: myValue 
		ifNone: [self translator value: myValue]! !

!StreamTranslationParseNode publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	translator := [:client | client]! !

!StringParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process.
	Subclasses should override this."

	^#String! !

!StringParseNode publicMethodsFor: 'accessing'!

changeWidthTo: aWidth while: block0 
	| value oldWidth |
	
	[oldWidth := self width.
	self width: aWidth.
	value := block0 value]
		valueNowOrOnUnwindDo: [self width: oldWidth].
	^value!

value
	"Answer the string value that I represent."

	^super value trimBlanks!

widthValue
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
		while: [super widthValue]! !

!Symbol publicMethodsFor: 'evaluating'!

value: aValue
	"Evaluate the block represented by the receiver.  Fail if the block
	expects other than one argument."

	^aValue perform: self!

value: arg1 value: arg2 
	"Evaluate the block represented by the receiver. Fail if the block 
	expects other than two arguments."

	^arg1 
		perform: self 
		with: arg2!

value: arg1 value: arg2 value: arg3
	"Evaluate the block represented by the receiver.  Fail if the block
	expects other than three arguments."

	^arg1 
		perform: self 
		with: arg2
		with: arg3!

valueWithArguments: anArray
	"Evaluate the block represented by the receiver.  The argument is an Array
	whose elements are the arguments for the block.  Fail if the length of the
	Array is not the same as the the number of arguments that the block was
	expecting."

	^anArray size > 1
		ifTrue: [
			anArray first 
				perform: self
				withArguments: (anArray copyFrom: 2 to: anArray size)
		]
		ifFalse: [anArray first perform: self]! !

!Symbol publicMethodsFor: 'testing'!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true! !

!TextCollector publicMethodsFor: 'entry control'!

close
	"Be polymorphic with streams."

	^self endEntry; yourself! !

!TimestampParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Timestamp! !

!TimestampParseNode publicMethodsFor: 'accessing'!

value
	"Answer the timestamp value that I represent."

	| tsString |
	^(tsString := super value asUppercase) = 'MAXIMUM'
		ifTrue: [Timestamp openEnded]
		ifFalse: [tsString = 'MINIMUM'
					ifTrue: [Timestamp arbitraryBegin]
					ifFalse: [Timestamp readFrom: tsString readStream]]! !

!TitleParseNode class publicMethodsFor: 'accessing'!

dataType
	"Answer the data type that I am designed to process."

	^#Title! !

!TitleParseNode publicMethodsFor: 'accessing'!

value
	"Answer the title value that I represent."

	^super value capitalizeWords! !

!UndefinedObject publicMethodsFor: 'testing'!

ifDefinedDo: block1 
	"do nothing"

	^self!

isImmediateValue
	"Answer whether the receiver has an optimal 
	Smalltalk representation."

	^true! !

FileImportParserContext initializeAfterLoad!
FileParser initializeAfterLoad!
PersistentObjectNotFoundSummary initializeAfterLoad!
StreamParseNode initializeAfterLoad!
StreamBasicParseNode initializeAfterLoad!
CharacterParseNode initializeAfterLoad!
BooleanParseNode initializeAfterLoad!
DateParseNode initializeAfterLoad!
NullParseNode initializeAfterLoad!
NumberParseNode initializeAfterLoad!
IntegerParseNode initializeAfterLoad!
RealNumberParseNode initializeAfterLoad!
MoneyParseNode initializeAfterLoad!
StringParseNode initializeAfterLoad!
EndOfLineParseNode initializeAfterLoad!
TitleParseNode initializeAfterLoad!
TimestampParseNode initializeAfterLoad!
StreamComponentParseNode initializeAfterLoad!
StreamAttributeParseNode initializeAfterLoad!
StreamAttributeReferenceParseNode initializeAfterLoad!
StreamAttributeValueParseNode initializeAfterLoad!
StreamCompositionParseNode initializeAfterLoad!
StreamDescriptionParseNode initializeAfterLoad!
StreamPersistentObjectDescriptionNode initializeAfterLoad!
StreamPersistentObjectCachedDescriptionNode initializeAfterLoad!
StreamPersistentObjectLimitedCachedDescriptionNode initializeAfterLoad!
StreamLoopFeedbackParseNode initializeAfterLoad!
StreamTranslationParseNode initializeAfterLoad!
StreamEnumeratedTypeParseNode initializeAfterLoad!
StreamBranchParseNode initializeAfterLoad!
TextFileParser initializeAfterLoad!

TextFileParser loaded!
