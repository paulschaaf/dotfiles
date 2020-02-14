alias :Set :Array
alias :Dictionary :Hash
alias :IdentityDictionary :Hash
alias :OrderedCollection :Array
alias :Bag :Array

require 'extn/Enumerable'
require 'extn/Proc'

# From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 11, 2003 at 4:46:43 pm
  
# \FILED_OUT_USING: Harmony Source Code Management System-Release 1.0 beta 2
  
# \SUBJECT: + (1) <LIB: 1998_0127_1458_14> {Module} app.FileParser.contexts
# \COPYRIGHT: Copyright (C) 1998 Versant.  All Rights Reserved.
# \FILED_OUT_BY: P.G. Schaaf
# \ENVIRONMENT: c:\Smalltlk\Harmony\paul
# \COMMENT: NONE

# Category: TextFileParser
class FileImportParserContext < VersantDBApplicationContext
  # ClassVariables: PersistentObjectNotFoundSignal 
  attr_accessor :prePersistentObjects
=begin
I manage data that should be global to a File Import session, including a cache of objects that have not yet been marked persistent (this is done for efficiency reasons)
=end
  
  ##----------
  ## accessing
  
  def FileImportParserContext.defaultDatabase
    # self defaultDatabase
  
    mapping = Hash.new('pauldb@localhost')
    mapping[self.currentUserID]
  end
  
  ##----------
  ## accessing.signals
  
  def FileImportParserContext.persistentObjectNotFoundSignal
    @@PersistentObjectNotFoundSignal
  end
  
  ##----------
  ## class initialization
  
  def FileImportParserContext.initializeSignals
    @@PersistentObjectNotFoundSignal = Exception.new('The requested object was not found.')
  end
  
  ##----------
  ## utility
  
  def FileImportParserContext.rootClasses
    super << StreamParseNode << FileParser
  end

  ##----------
  ## accessing
  
  def addPrePersistentObject(anObject)
    set = (@prePersistentObjects.[anObject.class] ||= Set.new)
    set << anObject unless set.include?(anObject)
    anObject
  end
  
  def prePersistentObjects
    coll = Array.new
    self.prePersistentObjectsDo {|each| coll << each}
    coll
  end
  
  def prePersistentObjectsClassesDo
    @prePersistentObjects.each_key {|key| yield key}
  end
  
  def prePersistentObjectsDo
    @prePersistentObjects.each_value {|value| yield value}
  end
  
  def prePersistentObjectsOfClass(targetClass, useSubclasses=false)
    candidates = Array.new
    test=(useSubclasses ? :kind_of? :is_a?)
    @prePersistentObjects.each_pair {|aClass, aCollection|
      aCollection.concat(candidates) if aClass.send(test, targetClass)
    }
    return yield if candidates.empty? && block_given?
    candidates
  end
  
  def removePrePersistentObject(anObject, &ifNone)
    @prePersistentObjects[anObject.class].remove(anObject, &ifNone)
  end
  
  ##----------
  ## accessing.signals
  
  def persistentObjectNotFoundSignal
    # self current persistentObjectNotFoundSignal
    @@PersistentObjectNotFoundSignal
  end
  
  ##----------
  ## actions
  
  def createInDatabaseUsing
    self.addPrePersistentObject(yield)
  end
  
  def flushAllNonPersistentObjects
    # self current flushAllNonPersistentObjects
  
    flushCount = 0
    @prePersistentObjects.delete_if {|aClass, anInstanceCollection|
      anInstanceCollection.delete_if {|each|
        (each.isODBPersistent) && (flushCount += 1)
      }
      anInstanceCollection.empty?
    }
    self.newLineTraceWith(flushCount.to_s, ' objects flushed.')
  end
  
  def persistAllNonPersistentObjects
    # self.current persistAllNonPersistentObjects
    # todo show busy cursor
    self.prePersistentObjectsDo {|each|
      self.makePersistent(each) unless each.isODBPersistent
    }
  end

  ##----------
  ## actions.basic
  
  def basicBeginSessionOn(aDatabase)
    self.databaseInterface.beginOLSessionOn(self.database)
  end
  
  def doIfInProperSession(properSessionBlock0, improperSessionBlock0, noSessionBlock0)
    super
  ensure
    self.closeReferencedStreams
  end
  
  ##----------
  ## actions.db.session
  
  def commit
    super
    self.initializePrePersistentObjects
    self.traceTimeStamp
  end
  
  def commitAndClean
    super
    self.initializePrePersistentObjects
    self.traceTimeStamp
  end
  
  def endSession
    super
    self.initializePrePersistentObjects
    #todo Screen default ringBell
  end
  
  def rollback
    # We only wish to rollback if there is something to be rolled back.
    # self current rollback
  
    self.flushAllNonPersistentObjects  
    self.initializePrePersistentObjects if self.hasPrePersistentObjects
    super
  end
  
  ##----------
  ## debugging
  
  def closeReferencedStreams
    self.traceStreams.each {|each| each.close}
  end
  
  def nextPutAll(aString)
    self.traceStreams.each {|each|
      each.setToEnd unless each == Transcript
      each.nextPutAll(aString)
      flush
    }
  end
  
  ##----------
  ## initialize-release
  
  def defaultToDebugOn
    true
  end
  
  def initialize
    super
    self.initializePrePersistentObjects
    self.addTraceStream(('parse' , Time.now.to_s, '.txt').asFilename.writeStream)
  end
  
  def initializePrePersistentObjects
    # self current initializePrePersistentObjects
    prePersistentObjects = Hash.new
  end
  
  ##----------
  ## inquiries
  
  def hasPrePersistentObjects
    # self current hasPrePersistentObjects
    self.prePersistentObjects.each_value {|each|
      return true unless each.empty?
    }
    false
  end
  
  def prePersistentObjectCount
    # self.current prePersistentObjectCount
    sum=0
    self.prePersistentObjects.each_value {|each| sum += each.size}
    sum
  end
  
end

# Category: TextFileParser
class FileParser < Object
  attr_accessor :registry, :registrar, :recordsDoneCount, :startTimeSeconds, :recordCount
=begin
I construct a machine of parse nodes that can interpret the contents of the file I represent.  I manage the process of interpretation and the resultant objects, including persistence issues
  
  *************
  If the files are not self-referential (if they are completely normalized relational tables and the ordering of the records within the file cannot affect the processing time) they can be split into an arbitrary number of pieces and divided up between images on a different machine.  If these parsings are done concurrently the run times can be made to be as low as you like
  **************
=end
  
end

class FileParser
  ##----------
  ## accessing
  
  def clientClass
    # Answer the class that I should use to create instances.
    self.class.clientClass
  end
  
  def clientCreator
    self.class.clientCreator
  end
  
  def clientTypeDescriptor
    self.clientClass.printString
  end
  
  def context
    self.class.context
  end
  
  def debuggingBlock
    # Answer a block that is to be evaluated periodically for debugging
    # purposes.
  
    #| context sizeString |
    #context := self.context
    #^context debugIsOn not
    #  ifTrue{ {} }
    #  ifFalse:
    #    {self.recordsDoneCount(0)
    #    sizeString := String new writeStream
    #      nextPutAll: '/~';
    #      nextPutAll: self.recordCount.to_s;
    #      space;
    #      nextPutAll(self clientTypeDescriptor;)
    #      nextPutAll: 's completed.';
    #      contents
 
    #    {self.incrementRecordsDoneCountBy(self debugCycleClientCount)
    #    context newLineTraceWith: self recordsDoneCount.to_s , sizeString}}
    context = self.context
    return lambda {} unless context.debugIsOn
    lambda {
      self.recordsDoneCount(0)
      sizeString = "/~#{self.recordCount} #{self.clientTypeDescriptor}s completed."  
      self.incrementRecordsDoneCountBy(self.debugCycleClientCount)
      context.newLineTraceWith(self.recordsDoneCount.to_s, sizeString)
    }
  end
  
  def delimiter
    # Answer the delimiter to be used between attributes in a record.
    self.class.delimiter
  end
  
  def estimatedCompletionTime
    Time.fromSeconds((Time.now.tv_sec + self.estimatedTimeRemaining))
  end
  
  def estimatedTimeRemaining
    secondsPassed = Time.now.tv_sec - self.startTimeSeconds
    percentageComplete = self.recordsDoneCount / self.recordCount
    totalSeconds = secondsPassed / percentageComplete
    totalSeconds - secondsPassed
  rescue ArithmeticValue.divisionByZeroSignal
    0
  end
  
  def fileName
    self.class.fileName
  end
  
  def newClient
    self.class.clientCreator.value
  end
  
  def objectNotFoundRegistrarFor(aRegistry)
    # Answer an object that can track requests of the
    # persistent store that come up empty.
  
    lambda {|aSummary|
      client := aSummary client
      (aRegistry[client.class] ||= Set.new)[client]= aSummary.predicate
    }
  end
  
  ##----------
  ## accessing.nodes
  
  def endOfLineAttributeNodeOn(aStream)
    StreamAttributeParseNode
      on: (self.endOfLineNodeOn(aStream))
      stReader: {|client| }
      stWriter: {|client, value| }
      odbValueTest: ''
  end
  
  def endOfLineNodeOn(aStream)  
    self.stringNodeOn(aStream.withDelimiter($/))
  end
  
  def nilAttributeNodeOn(aStream)
    ^StreamAttributeParseNode
      on: (self.nilNodeOn(aStream))
      stReader: {|client| }
      stWriter: {|client, value| }
      odbValueTest: ''
  end
  
  ##----------
  ## accessing.nodes.delemeted
  
  def booleanNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Boolean, aDelimiter)
  end
  
  def characterNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Character, aDelimiter)
  end
  
  def dateNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Date, aDelimiter)
  end
  
  def integerNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Integer, aDelimiter)
  end
  
  def moneyNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Money, aDelimiter)
  end
  
  def nilNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, nil, aDelimiter)
  end
  
  def numberNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Number, aDelimiter)
  end
  
  def stringNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :String, aDelimiter)
  end
  
  def timeNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Time, aDelimiter)
  end
  
  def timestampNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Timestamp, aDelimiter)
  end
  
  def titleNodeOn(aStream, aDelimiter=self.delimiter)
    StreamBasicParseNode(aStream, :Title, aDelimiter)
  end
  
  ##----------
  ## accessing.nodes.fixedWidth
  
  def booleanNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Boolean, aWidth)
  end
  
  def characterNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Character, aWidth)
  end
  
  def dateNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Date, aWidth)
  end
  
  def integerNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Integer, aWidth)
  end
  
  def moneyNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Money, aWidth)
  end
  
  def nilNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, nil, aWidth)
  end
  
  def stringNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :String, aWidth)
  end
  
  def timestampNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Timestamp, aWidth)
  end
  
  def titleNodeOn(aStream, aWidth)
    StreamBasicParseNode(aStream, :Title, aWidth)
  end
  
  ##----------
  ## actions
  
  def asTransactionDo
    yield
  end
  
  def commit
    # do nothing
  end
  
  def incrementRecordsDoneCountBy(aValue)
    recordsDoneCount += aValue
  end
  
  def parseFile
    # Establish the transaction, parse the file, and answer the registry of
    # unsatisfied requests.
  
    #todo Cursor wait showWhile:
    self.preParseFile
    self.parseStreamOn(self.fileName)
    self.postParseFile

    self.displayParsingReport
    self
  end
  
  def parseStreamOn(aFileName)
    # Create a stream on the file and parse it.
  
    self.context.newLineTraceWith('Parsing file for ' , self clientTypeDescriptor;)
    self.context.newLineTraceWith: 'beginning...'
  
    stream = aFileName.readStream
    myClientCreator = self.postInitializedRootNodeOn(stream)
    begin
      self.preParseStream(stream)
      self.parseStream(stream, myClientCreator)
    ensure
      self.postParseStream(stream)
    end
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    @registry = Hash.new
    @registrar = self.objectNotFoundRegistrarFor(registry)
    @recordCount = nil
    @recordsDoneCount = nil
  end
  
  def postInitializeNewDomainObject(aDomainObject)
    # Do whatever is necessary to further prepare objects
    # that have been read from the file.  This is a good place
    # to set any constant values in the domain object.
    aDomainObject
  end
  
  def preInitializeNewDomainObject(aDomainObject)
    # Do whatever is necessary to prepare objects
    # whose attributes will be read from the file.
    aDomainObject
  end
  
  ##----------
  ## override.accessing
  
  def debugCycleClientCount
    # Answer the number of instances that should be created between
    # debug progress notifications.  This number should be proportional
    # to the time it takes to completely process a typical record in the file,
    # and **it should be an even factor of :maxInMemoryClientCount.**
    # As its purpose is to assure the user that progress is being made, the
    # higher it is set the longer an interested observer will be made to wait.
    # As screen writes are not cheap, if set too low the display update will
    # consume too much of the processing, and skew the performance
    # statistics.
    # i = self.basicNew
    # (i.maxInMemoryClientCount.remainder(i.debugCycleClientCount) = 0)
  
    self.maxInMemoryClientCount / 5.0
  end
  
  def debugTraceIncrement
    # Answer the number of instances that should be created between
    # debug progress notifications.  This number should be proportional
    # to the time it takes to completely process a typical record in the file,
    # and **it should be an even factor of :maxInMemoryClientCount.**
    # As its purpose is to assure the user that progress is being made, the
    # higher it is set the longer an interested observer will be made to wait
    # As screen writes are not cheap, if set too low the display update will
    # consume too much of the processing, and skew the performance
    # statistics.
    # i := self.basicNew
    # (i.maxInMemoryClientCount.remainder(i.debugTraceIncrement) = 0)
  
    self maxInMemoryClientCount / 5.0
  end
  
  def maxInMemoryClientCount
    # Answer the total number of clients permitted to exist in memory.  This
    # number should be proportional to the size of a typical client.  When this
    # boundary is reached, the clients are committed and removed from memory
    # If set too high there will not be enough memory to support all of the tasks
    # and garbage collection thrashing will occur, increasing the processing time
    # If set too low the commits will happen more frequently than they need to,
    # increasing the processing time.
  
    3000
  end
  
  def rootNodeType
    StreamDescriptionParseNode
  end
  
  ##----------
  ## override.actions
  
  def postParseFile
    # do nothing
  end
  
  def postParseStream(aStream)
    # do whatever work is postrequisite to parsing the file
    aStream close
  end
  
  def preParseFile
    # Do whatever work is prerequisite to parsing the file.
    self.context.initialize
    startTimeSeconds = Time.now.tv_sec
    recordCount = self.class.recordCount
  end
  
  def preParseStream(aStream)
    # do whatever work is prerequisite to parsing the file
    # do nothing
  end
  
  ##----------
  ## override.associations
  
  def selectorNodeFor(aClass, aDatabase)
    # Answer an object that can create new clients. Subclasses should override this to add attributes and
    # relations.
  
    StreamPersistentObjectDescriptionNode(aClass.new, aDatabase)
  end
  
  def selectorNodeOn(aStream, aClass, using)
    # Answer an object that can create new clients. Subclasses should override   this to add attributes and
    # relations.
  
    (self.selectorNodeFor(aClass in: self.database))[using]=
      (self.stringNodeOn(aStream))
    self
  end
  
  ##----------
  ## override.testing
  
  def shouldUseDelimitedFormat
    # Answer whether the records in my file are delimited.
    true
  end
  
  ##----------
  ## printing
  
  def displayParsingReport
    #todo lots here
    processingTime = Time.now.subtractTime: (Time.fromSeconds: self.startTimeSeconds)
    str = String.new.writeStream
    str.nextPutAll('Completed in ')
  
    (TimestampPrintPolicy defaultInstance.clone
      shortPolicyString: 'hh:mm')
      printShort: processingTime
      on: str
  
    str
      nextPutAll: ', throughput: ';
      print: (self.recordCount * 60 / (processingTime asSeconds max(1)) asFloat;)
      nextPutAll: ' records/minute.'
    self.context
      traceWith(str contents)
      isNewLine: true
  end
  
  def printTreeOn(aStream)
    # Print a textual description of the parse tree on aStream.
    (self.rootNodeOn(nil)).printTreeOn(aStream)
  end
  
  ##----------
  ## private
  
  def parseStream(aStream, clientCreator)  
    debugCycleClientCount = self.debugCycleClientCount
    debugCyclesPerCommit = (self.maxInMemoryClientCount / debugCycleClientCount).floor.max(1)
  
    #{debugCyclesPerCommit timesRepeat:
    #  {debugCycleClientCount timesRepeat:
    #    {aStream atEnd
    #      ifTrue: {^self.commit; self}
    #      ifFalse({clientCreator value}}})
    #  self commit} repeat

    while true 
      {debugCyclesPerCommit times 
	    {debugCycleClientCount times
	      {if aStream.atEnd
		 self.commit
		 return self
	       else
		 clientCreator.value
	       end}
	      self commit}
    end
  end
  
  ##----------
  ## private.accessing
  
  def postInitializedRootNodeOn(aStream)
    # Answer an object that can create new clients. Subclasses should
    # override this to add attributes and relations.
  
    debugBlock = self.debuggingBlock
    StreamTranslationParseNode(self.rootNodeOn(aStream)) {|client|
      self.postInitializeNewDomainObject(client)
      debugBlock.value
      client}
  end
  
  def preInitializedRootNodeOn(aStream)
    # Answer an object that can create new clients. Subclasses should
    # override this to add attributes and relations.
  
    StreamTranslationParseNode(self.clientCreator) {|client|
      self.preInitializeNewDomainObject(client)
      client}
  end
  
  def rootNodeOn(aStream)
    # Answer an object that can create new clients. Subclasses should override
    # this to add attributes and relations.
    # \OWNERSHIP:  Copyright (C) 1996, 1997 P.G. Schaaf. All Rights Reserved.
  
    self.rootNodeType.on(self preInitializedRootNodeOn: aStream)
  end
  
  ##----------
  ## accessing
  
  def FileParser.attributeCount
    # Answer the number of attributes contained in each record
    # of the file.  This is only used by the utilities and not for
    # parsing (where this value is defined implicitly).  Utilities
    # dependent upon file structure will not work properly on
    # files whose records have a variable number of attributes.
    # Check whether the expected attributeCount equals the
    # attributeCount described by the parse tree defined by an
    # instance of this class.
    # (self attributeCount) = (self new rootNodeOn(attributeCount))
  
    self.subclassResponsibility
  end
  
  def FileParser.clientClass
    # Answer the class that I should use to create instances.
    # Concrete subclasses should answer a class, Abstract
    # subclasses should answer nil.
    nil
  end
  
  def FileParser.clientClassToParserMapping
    # Answer a mapping from the client classes to their FileParsers.
    # self clientClassToParserMapping
  
    dict = Hash.new
    self.allSubclasses.each {|each|
      | key |
      dict[key = each.clientClass]=each if key
      dict}
  end
  
  def FileParser.clientCreator
    # Answer a block that can create new instances
    # of my client class.
  
    myClientClass := self clientClass
    lambda {myClientClass.new}
  end
  
  def FileParser.context
    # Answer the context in which my calculations should occur.
    FileImportParserContext.current
  end
  
  def FileParser.currentUserID
    # Answer the login ID of the current user.
    self.context.currentUserID
  end
  
  def FileParser.defaultFileDrive
    # Answer the default for the name of the drive
    # in which I should look for my file.  This method
    # should be patched to include drive letter info for
    # new users.
    # self defaultFileDrive
  
    userToDriveLetterMapping := Hash.new('.')
    userToDriveLetterMapping[self.currentUserID]
  end
  
  def FileParser.defaultFileName
    # Answer the default name of the file that I should parse
    # This should only include path information that distinguishes
    # it from the other files that are being processed.
    # self defaultFileSpecification
    # self editFile
  
    self.defaultFilePath
  end
  
  def FileParser.defaultFilePath
    # Answer the default for the name of the directory
    # in which I should look for my file.  This should
    # begin and end with a directory separator and
    # should not include the file name.
  
    self.defaultFileDrive, ''
  end
  
  def FileParser.defaultToTraceOn
    # Answer whether parses for my instances should trace their output.
    true
  end
  
  def FileParser.delimiter
    # Answer the character(s) used to delimit the
    # attributes used by my file.
    $/
  end
  
  def FileParser.fileName
    # Answer an instance of Filename that points to my file.
    # CIAccountParser fileName
  
    fileName = self.defaultFileName
  end
  
  def FileParser.fileNotFoundSignal
    OSErrorHolder.nonexistentSignal
  end
  
  def FileParser.requestFileSpecFromUser(aSpec)
    # Answer a string as supplied by the user.
    # self.requestFileSpecFromUser('c:\nowhere')
  
    ^puts "Where can I find the file representing:\n#{self.clientClass}?"
  end
  
  ##----------
  ## actions
  
  def FileParser.parseFile
    self.parseFileTracing(self.defaultToTraceOn)
  end
  
  def FileParser.parseFileTracing(aBoolean)
    aBoolean ? self.parseFileWithTracing : self.parseFileWithoutTracing
  end
  
  def FileParser.parseFileWithoutTracing
    self.context.doNotEvaluateDebugCodeWhile {self.new.parseFile}
  end
  
  def FileParser.parseFileWithTracing
    self.context.evaluateDebugCodeWhile {self.new.parseFile}
  end
  
  def FileParser.printTree
    str = String.new.writeStream
    self.printTreeOn(str)
    str.contents
  end
  
  def FileParser.printTreeOn(aStream)
    # Transcript clear
    # CIAddressParser printTreeOn: Transcript
    # Transcript endEntry
  
    self.context.doNotEvaluateDebugCodeWhile:
      {self.new.printTreeOn(aStream)}
  end
  
  def FileParser.traceTree
    self.printTreeOn(Transcript)
    Transcript.endEntry
  end
  
  ##----------
  ## utility
  
  def FileParser.fileSample
    # Answer a sample from my file.
  
    self.fileSampleFrom(self.fileName)
  end
  
  def FileParser.fileSampleFrom(aFile recordCount=50)
    # Answer a sample taken from aFile that includes recordCount complete records.
  
    delimiter = self.delimiter
    attributesRequestedCount = recordCount * self.attributeCount
    aStream = (String new(attributesRequestedCount)).clone
    file = File.open(aFile) {
      attributesRequestedCount.times {aStream << file.through(delimiter)}
    }
    aStream.reset
    self
  end
  
  def FileParser.recordCount
    # Answer the number of records in my file.
  
    self.recordCountIn(self.fileName)
  end
  
  def FileParser.recordCountIn(aFile)
    # Answer the number of records in aFile.  Note that for
    # an accurate number to be answered all records in the
    # file must have the same number of attributes.
    # CIAddressParser editFile
    # CIAddressParser recordCount
    # CIProductItemParser recordCount
  
    aFile.readlines($/).size
  end
  
  ##----------
  ## utility.batch processing
  def FileParser.batchProcessJobListFromUser
    # self batchProcessJobListFromUser
  
=begin
    | batchList start |
    batchList := self batchProcessJobList.clone
    start := Dialog
      choose: 'Begin processing at which level?'
      fromList: (batchList collect: {|each| each name})
      values: batchList
      lines: 8
      cancel: {^OrderedCollection new}
    {batchList first == start} whileFalse: {batchList removeFirst}
    ^batchList
=end
  end
  
  def FileParser.process(aJob)
    aJob || aJob.parseFileTracing(true)
  end
  
  def FileParser.processBatch
    # self.processBatch
  
    self.processBatch(self.batchProcessJobList)
  end
  
  def FileParser.processBatch(aList)
    aList.each {|each| each.parseFileTracing(true)}
  end
  
  def FileParser.processBatchFromUser
    # Allows the user to begin the batch processing at
    # any point in an ordered list of parsing jobs.
    # self processBatchFromUser
  
    self.processBatch(self.batchProcessJobListFromUser)
  end
  
  def FileParser.processBatchThenQuit
    # self processBatchThenQuit
  
    list = self.batchProcessJobList
    self.processBatchThenQuit(list) unless list.empty?
  end
  
  def FileParser.processBatchThenQuit(aBatch)
    # self processBatchThenQuit
  
    self.processBatch(aBatch)
  rescue => err
    ctx = FileParser context
    ctx.newLineTraceWith('EXCEPTION: ' , errorString)
    ctx.rollback
  ensure
      FileParser.context.endSession
  end
  
  def FileParser.processBatchThenQuitFromUser
    # Allows the user to begin the batch processing at
    # any point in an ordered list of parsing jobs. End the
    # session and quit the image (without saving) after all
    # jobs are completed. If at any point there is an un-
    # handled exception, end job processing, log the ex-
    # ception to the current log file, rollback, and then end
    # the session and quit without saving.
    # self processBatchThenQuitFromUser
  
    list = self.batchProcessJobListFromUser
    self.processBatchThenQuit(list) unless list.empty?
  end
  
  def FileParser.processFromUser
    # Allows the user to select from an ordered list of parsing jobs.
    # self.processBatchFromUser
  
    self.process(self.batchProcessJobListFromUser[0])
  end
  
  ##----------
  ## utility.session
  
  def FileParser.beginOLSession
    self.context.beginOLSession
  end
  
  def FileParser.beginSession
    self.context.beginSession
  end
  
  def FileParser.commit
    self.context.commit
  end
  
  def FileParser.connectToDatabaseFromUser
    # self.connectToDatabaseFromUser
  
    (list = self.favoriteDBList).delete(self.context.database)
=begin
    choice := Dialog
          choose: 'Connect to which database?'
          fromList: list
          values: list
          buttons: :('other...')
          values: (Array with: {self.databaseNameFromUserIfNone({}}))
          lines: 8
          cancel: {nil}
=end
    self.context.connectToDatabase(choice) if (choice = choice.value)
  end
  
  def FileParser.databaseNameFromUserIfNone
    # self.databaseNameFromUserIfNone({nil})
  
    choice := Dialog.request("Connect to what database (bprsvr4\nis the default server)?")
    if choice.include?('@')
      choice
    else
      if choice.empty?
         yield
      else
	choice = choice + '@bprsvr4'
      end
    end
  end
  
  def FileParser.disconnectFromDatabaseFromUser
    # self.disconnectFromDatabaseFromUser
  
    connectedDatabases = self.context.connectedDatabases.uniq!
    return nil if connectedDatabases.empty?
=begin
    choice := Dialog
      choose: 'Disconnect from which database?'
      fromList: connectedDatabases
      values: connectedDatabases
      lines: 8
      cancel: {nil}
=end
    choice || self.context.disconnectFromDatabase(choice)
  end
  
  def FileParser.endSession
    self.context.endSession
  end
  
  def FileParser.favoriteDBList
    ['chuckdb@bprsvr4', 'pauldb@bprsvr4', 'paulrefdb@bprsvr4', 'refdb1016@bprsvr4']
  end
  
  def FileParser.rollback
    self.context.rollback
  end
  
  ##----------
  ## utility.transaction management
  
  def FileParser.manageInSessionTransactionFromUser
    # self.manageInSessionTransactionFromUser
  
    options = [:connectToDatabaseFromUser, :rollback, :commit, :endSession]
    options += :disconnectFromDatabaseFromUser unless self.context.connectedDatabases.empty?
=begin
    choice = Dialog
          choose: 'Session database: ' , self.context database printString
          fromList((options collect: {|each| each.to_s}))
          values: options
          lines: 5
          cancel: {nil}
=end
    choice.value(self)
  end
  
  def FileParser.manageNotInSessionTransactionFromUser
    # self.manageNotInSessionTransactionFromUser
  
    options = [:beginSession, :beginOLSession, :cancel]
=begin
    choice = Dialog
          choose: 'Default database: ' , self.context database printString
          labels((options collect: {|each| each.to_s}))
          values: options
          default: :cancel
=end
    choice.value(self) unless choice == :cancel
  end
  
  def FileParser.manageTransactionFromUser
    # Allow the user to perform a variety of context-sensitive
    # session management options.
    # self.manageTransactionFromUser
  
    self.contextisInSession \
      ? self.manageInSessionTransactionFromUser \
      : self.manageNotInSessionTransactionFromUser
  end
  

end

# Category: TextFileParser
class VersantFileParser < FileParser
  attr_accessor :database

  ##----------
  ## accessing
  
  def persistentObjectNotFoundRegistrarFor(aRegistry)
    # Answer an object that can track requests of the
    # persistent store that come up empty.
  
    lambda {|aSummary|
      client = aSummary.client
      (aRegistry[client.class] ||= Hash.new)[client.id]= aSummary.predicate
    }
  end
  
  def referencedClasses
    # Answer all of the classes that are directly referenced during this parsing.
  
    self.classToRequiredDatabaseMapping.keys
  end
  
  def supplementaryDatabases
    # Answer all of the databases that are used during this parsing.
  
    self.classToRequiredDatabaseMapping.values
  end
  
  ##----------
  ## actions
  
  def asTransactionDo(&block0)
    self.context.asTransactionDo {
      self.context.connectToDatabases(self.supplementaryDatabases, &block0)
    }
  end
  
  def commit
    myContext = self.context
    myContext.persistAllNonPersistentObjects
    self.handleNonOLReadyClassWhile {myContext.commit}
    if self.context.debugIsOn
      self.displayDatabaseToSizeMap
      self.context.newLineTraceWith("Estimated completion time: #{self.estimatedCompletionTime}")
    end
  end
  
  def databaseToSizeMap
    myContext = self.context
    map = Dictionary.new
    (myContext.connectedDatabases.uniq + self.database).each {|each|
      map[each]= myContext.databaseSize(each)
     }
    map
  end
  
  def handleNonOLReadyClassWhile(&doBlock0)
    self.handleNonOLReadyClassWith(doBlock0) {|ex|
      aClass = ex.parameter.key
      aDatabase = ex.parameter.value
      if aClass.turnOnOLReadyIn(aDatabase)
	ex.restart
      else
	ex.reject
      end
    }			   
  end
  
  def handleNonOLReadyClassWith(doBlock0, &handleBlock)
     doBlock.value
  rescue VError => ex
    if VError.errorSymbol == :VSI_ERROR && 'has different OL-ready status' =~ VError.description
      # This is a wicked hack but I know no other way of getting
      # this information without changing the whole Versant error process.
      aStream = VError.description.readStream
      aStream.skip('Database class '.size)
      aClass = Smalltalk[aStream.upToAll(' in database ').asSymbol]
      aStream.skip(' in database '.size)
      aDatabase = aStream.upTo(' ')
      ex.parameter([aClass, aDatabase])
    else
      ex.reject
    end
  end
  
  def parseFile
    # \OWNERSHIP( Copyright (C) 1996 P.G. Schaaf. All Rights Reserved.)
  
    result = self.asTransactionDo {super.parseFile}
    ObjectSpace.garbage_collect
    result
  end
  
  def syncClassDefinitionsToDatabaseMappings
    # For each of the classes used in this parsing, set its specific
    # database attribute to that database, if necessary.
  
    commitAfterward = false
    self.classToRequiredDatabaseMapping.each_pair {|aClass, db|
      unless db.defines?(aClass)
        self.context.doNewLineTraceWith("Describing #{aClass.name} to #{db}") {
	  aClass.describeToOdb(db)
	}
	commitAfterward = true
      end
    }
    self.context.commit if commitAfterward
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    database = self.context.database
  end
  
  def postInitializeNewDomainObject(aDomainObject)
    # Do whatever is necessary to further prepare objects
    # that have been read from the file.  This is a good place
    # to set any constant values in the domain object.  If you
    # override this in the subclass, make sure to evaluate this
    # method after you evaluate the new one.
  
    self.context.makePersistent(aDomainObject, self.database)
  end
  
  def preInitializeNewDomainObject(aDomainObject)
    # Do whatever is necessary to prepare objects
    # whose attributes will be read from the file.
  
    super
    self.context.addPrePersistentObject(aDomainObject)
  end
  
  ##----------
  ## override.accessing
  
  def classToRequiredDatabaseMapping
    # Answer a mapping from each of the classes referred to by this node
    # to the databases in which they can be found.
    # Subclasses should extend this.
  
    dict = Hash new
    dict[self.clientClass]=self.database if self.clientClass
    dict
  end
  
  def database
    # Answer the database to be used for read and write operations on
    # instances of my client class.
  
    self.context.database
  end
  
  def rootNodeOn(aStream, aFileParser)
    # Answer an object that can create new clients.
  
    rootNode = super.rootNodeOn(aStream)
    aFileParser.aspects.each {|each|
      rootNode[each.ident.asSymbol]= 
	(each.parserNode(aStream, each.type, each,delimiter)
      }
    rootNode
  end
  
  ##----------
  ## override.actions
  
  def preParseStream(aStream)
    # do whatever work is prerequisite to parsing the file
  
    super
    self.syncClassDefinitionsToDatabaseMappings
  end
  
  ##----------
  ## printing
  
  def displayDatabaseToSizeMap
    myContext = self.context
    self.databaseToSizeMap.each_pair {|name, size|
      myContext.newLineTraceWith("#{name} size is #{size}")
    }
  end
  
  ##----------
  ## private
  
  def parseStream(aStream, aCreator)
    # \OWNERSHIP:  Copyright (C) 1996 P.G. Schaaf. All Rights Reserved.
  
    self.displayDatabaseToSizeMap
    super
  rescue self.context.persistentObjectNotFoundSignal => ex
    self.registrar.value(ex.parameter)
    self.context.newLineTraceWith('Persistent object not found.')
    ex.proceed
  end
  
end

# Category: TextFileParser
class PersistentObjectNotFoundSummary < Object
  attr_accessor :client, :initializer, :predicate
  
  ##----------
  ## accessing
  
  def initialize(aClient, anInitializer, aPredicate)
    super
    @client = aClient
    @initializer = anInitializer
    @predicate = aPredicate
  end
  end

=begin
A StreamParseNode is a machine that knows how to read and interpret data from the current position in its stream.  This process is initiated by asking the node for its :value.  Subclasses specialize this behavior to either apply a transformation to the data (such as mapping, interpretation, etc.), or to coordinate the component nodes that do the actual parsing
=end
  
class StreamParseNode
  @@UnexpectedValueSignal
  ##----------
  ## accessing
  
  def attributeClass
    # Answer the class of the attribute wrapper that I require.
  
    ^self.component.attributeClass
  end
  
  def component
    # Answer the node that provides my input.
  
    self.subclassResponsibility
  end
  
  def components
    ^[self.component]
  end
  
  def context
    # Answer the context in which my calculations should occur.
  
    ^ApplicationContext.current
  end
  
  def unexpectedValueSignal
    ^self.class.unexpectedValueSignal
  end
  
  def value
    # Answer the result of 1 computational cycle of my machinery.
  
    ^self.component.value
  end
  
  ##----------
  ## converting
  
  def asAttributeNamed(aName)
    # Answer myself.wrapped with an attribute node.
  
    self.attributeClass.on_slotName(self, aName.asSymbol)
  end
  
  ##----------
  ## debugging
  
  def asPostValueHaltNode
    # Answer myself.wrapped with a node that will halt after I am evaluated.
  
    StreamComponentParseNode.on {
      self.value
      self}
  end
  
  def asPostValueHaltNode(test1)
    # Answer myself.wrapped with a node that will halt after I am evaluated
    # if test1 on myself.is true.
  
    StreamComponentParseNode.on {
      myValue = self.value
      test1.value(myValue) && self.halt
      myValue}
  end
  
  def asPreValueHaltNode
    # Answer myself.wrapped by a node that will halt before I am evaluated.
  
    ^StreamComponentParseNode.on {
      self.halt
      value}
  end
  
  def asPreValueHaltNode(block0)
    # Answer myself.wrapped by a node that will halt
    # before I am evaluated if the value of block0 is true.
  
    ^StreamComponentParseNode.on {
      block0.value && nil.halt
      self.value}
  end
  
  ##----------
  ## inquiries
  
  def attributeCount
    # Answer the numer of attributes of which I am composed.
  
    self.components.inject(0) {|count, each|
      if each.species == StreamParseNode
	count + each attributeCount
      else
        count
      end}
  end
  
  def mapsDirectlyToFile
    false
  end
  
  def species
    StreamParseNode
  end
  
  ##----------
  ## printing
  
  def printOn(aStream level: aLevel)
    aStream.crtab(level + 1) << self.to_s
  end
  
  def printSubcomponents(subcomponents, aStream, aLevel)
    aStream << ' ('
    subcomponents .each {|each|
      self.printValue(each, aStream, aLevel)}
    aStream.crtab(aLevel - 1) unless
      (subcomponents.size == 1) &&
      (subcomponents.first.species == self.species) &&
      subcomponents.first.mapsDirectlyToFile
    aStream << ')'
  end
  
  def printSubcomponentsOn(aStream, aLevel)
    mySubcomponents = self.components
    self.printSubcomponents(mySubcomponents, aStream, aLevel) unless mySubcomponents.empty?
  end
  
  def printTree
    str = Array.new
    self.printTreeOn(str)
    str.join
  end
  
  def printTreeOn(aStream, aLevel=0)
    self.printOn(aStream, aLevel)
    self.printSubcomponentsOn(aStream, aLevel + 1)
  end
  
  def printValue(aValue on: aStream level: aLevel)
    if aValue.species == self.species
        aValue.printTreeOn(aStream, level)
    else
        aStream.crtab(aLevel)
      aStream << aValue.to_s
    end
  end
  
  def traceTree
    self.printTreeOn(Transcript)
    Transcript.endEntry
  end
  
  ##----------
  ## accessing
  
  def StreamParseNode.dataType
    :Object
  end
  
  def StreamParseNode.unexpectedValueSignal
    # Answer the signal used when the value found in my
    # text field is somehow unexpected.
  
    self.initializeSignals unless UnexpectedValueSignal
    UnexpectedValueSignal
  end
  
  ##----------
  ## class initialization
  
  def StreamParseNode.initialize
    # self.initialize
  
    self.initializeSignals
  end
  
  def StreamParseNode.initializeSignals
    UnexpectedValueSignal := Exception.new('An unexpected value has been found in this field')
  end
  
end

# Category(TextFileParser)
class StreamComponentParseNode < StreamParseNode
  attr_accessor :component
=begin
Instances of me are used to make a non-node object appear to be a node (a la the Decorator pattern).  If you implement
  
    Object>>value
      ^self
  
  then you can use the number 1234 as if it were a parse node by wrapping it with a value node:
  
    a := StreamComponentParseNode
      on: 1234
  
  Notice that ''a'' is a parse node whose value is constant
  
  If you do not implement Object>>value, then the implementation of ''a'' would be:
  
    a := StreamComponentParseNode
      on: {1234}
=end
  
  alias :on :component=
  
end

class StreamComponentParseNode class
  ##----------
  ## instance creation
  
  def on(aValue)
    ^self.new on(aValue)
  end
  
end
  
=begin
This node replaces its component with the value of its component whenever it is asked for its value (i.e. it uses its own output from invocation n as its input on invocation n+1).  For example:
    If we evaluate the following:
      node := StreamLoopFeedbackParseNode on: { { ''hello'' } }
      component0 := node component.    # = { { ''hello'' } }, no change yet
  
      value1 := node value
      # = { ''hello'' }, the outermost block has been evaluated and the result
    # placed in component and answered
      component1 := node component.      # value1 == component1
  
      value2 := node value
      # = ''hello'', the outermost block has been evaluated and the result
    # placed in component and answered
      component2 := node component.      # value2 == component2
  
      value3 := node value
      # = ''hello'', the string has been evaluated (it answers itself) and the result
    # placed in component and answered
      component3 := node component.      # value3 == component3
  
      # We could continue this forever, but `node value` would always equal ''hello'',
    # because `''hello'' value` equals ''hello''. 
  
  A more typical use of this node would be for deferred computation of a constant value as such:
  
    node := StreamLoopFeedbackParseNode on: (self.computePi)
  
  The first time this is evaluated, pi is computed and then answered.  All subsequent times it is simply answered directly without being re-computed
=end

class StreamLoopFeedbackParseNode < StreamComponentParseNode
  ##----------
  ## accessing
  
  def value
    # Answer the value that I represent, and recycle that value.
  
    self.component= super
  end
  
  ##----------
  ## documentation
  
  def StreamLoopFeedbackParseNode.testNode
    # self.testNode
    # node = self.testNode
    # oc = Array.new
    # 5 times {oc << node.value}
    # oc
  
    self.on(lambda { lambda {7} })
  end
  
end

# Category: TextFileParser
class StreamTranslationParseNode < StreamComponentParseNode
  attr_accessor :translator
=begin
Instances of me answer the value of their component after having applied a transformation function.  For instance, my owner wishes to know the name of the person answered by another node.  Assuming that personNode is a node whose value is a person, create the translator as follows:
  
    StreamTranslationParseNode
      on: (personNode)
      translator: {|aPerson| aPerson name}
  
  This translator is passed the output of the component as a parameter, but it need not do anything with that value.  The following node evaluates personNode but discards the result.  When the node is evaluated, it always answers ''John''
  
    StreamTranslationParseNode
      on: (personNode)
      translator: {|aPerson| ''John''}
  
  Although this is permitted (mainly because I have no means of detecting it) it is not recommended style.  Why do the work that personNode represents if it is simply to be thrown away?
  
  It is possible to use Translation nodes to simulate Branch and Enumeration nodes--just as it is possible to use a screwdriver as a hammer.  In both situations you are far better off to use the more specialized tool.
=end
  
  ##----------
  ## accessing
  
  def translationOf(aValue)
    yield
  end
  
  def value
    # Answer the translation of my super value.
  
    myValue := super
    self.translationOf(myValue) {
      self.translator.value(myValue)}
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    self.translator= {|client| client}
  end
  
  ##----------
  ## instance creation
  
  def StreamTranslationParseNode.on(aGenerator, &translator)
    answer = self.on(aGenerator)
    answer.translator= &translator
    answer
  end

end

# Category: TextFileParser
class StreamEnumeratedTypeParseNode < StreamTranslationParseNode
  attr_accessor :conditions
=begin
Instances of me maintain an enumeration of permissible values.  When an instance is evaluated, it evaluates its component and tests to see if that value is in the enumeration.  If it is, answer it.  If it is not, answer the value of ifNoneBlock
  
    StreamEnumeratedTypeParseNode
      on: (integerNode)
      enumeration: (1 to: 75)
      ifNone: {|value| Dialog warn: value.to_s, '' is not in the permissible range''}
  
  If the ifNone block is not provided, the default one is used, which raises the unexpectedValueSignal.  You will usually not want to override this.  Instead, handle the signal and process it appropriately.  If you just open a Dialog (as above) the whole parsing process will be suspended until the user acknowledges the dialog
=end
  
  ##----------
  ## accessing
  
  alias :ifNone :translator
  alias :ifNone= :translator=
  
  def translationOf(aValue, &ifNone)
    if self.conditions.include?(aValue)
      aValue
    else
      ifNone.value
    end
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    self.ifNone= lambda {|key| self.unexpectedValueSignal.raiseWith(key)}
  end
  
  ##----------
  ## printing
  
  def printOn(aStream, aLevel)
    # do nothing
  end
  
  def printSubcomponentsOn(aStream, aLevel)
    wstr = ['{']
    self.conditions.each_separatedBy(lambda {wstr << ' '}) {|each| wstr << each.to_s}
    wstr << '}'
    self.printValue(wstr.contents, aStream, aLevel)
  end
  
  ##----------
  ## instance creation
  
  def StreamEnumeratedTypeParseNode.on(aNode, enumeration, &ifNone)
    (answer = self.on(aNode)).enumeration= aCollection
    answer.ifNone(&ifNone)
  end
  
end

=begin
My instances serve as a case statement.  They first evaluate their keyNode, search their testConditions for the *first* one that that value satisfies, and answer the value of the corresponding branchNode.  If the value fails every testCondition, evaluate ifNoneBlock with the value as an argument
  
  Do not use an instance of me to constrain the permissible input values to an enumerated set, use a StreamEnumeratedTypeNode instead
=end

class StreamBranchParseNode < StreamEnumeratedTypeParseNode
  ##----------
  ## accessing
  
  def components
    super + self.values
  end
  
  def keys
    self.conditions.collect {|each| each.key}
  end
  
  def translationOf(aValue, &ifNone)
    # Answer the parsedValue of the node whose test condition
    # is satisfied by my parsedValue.
  
    (self.conditions.detect_if_none(ifNone) {
       |cond| cond key value: aValue}
     ).value.value
  end
  
  def values
    self.conditions.collect {|each| each.value}
  end
  
  ##----------
  ## actions
  
  def canUseEquivalenceTestOn(aValue)
    [Fixnum, TrueClass, FalseClass, NilClass, Symbol] include?(aValue.class)
  end
  
  def mapIfEquals(aValue, aNode)
    self.mapIfSatisfies(aNode) {|parsedValue| aValue == parsedValue}
  end
  
  def mapIfSatisfies(aNode, &block1)
    conditions<< [block1, aNode]
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    conditions = Array.new
  end
  
  ##----------
  ## inquiries
  
  def attributeCount
    # Answer the count of attributes of which I am composed.  This
    # number will be incorrect if the number of nodes in each branch
    # differs.
  
    [self.component, self.conditions someElement key].inject(0) {|count, each|
        if each.species == StreamParseNode
          count + each.attributeCount
	else
          count
	end
      }
  end
  
  def expectedValues
    self.conditions.collect {|each| each.copiedValues}
  end
  
  ##----------
  ## printing
  
  def printOn(aStream, aLevel)
    self.printValue('Branch: ', aStream, aLevel)
    self.printSubcomponents([self.component], aStreamaLevel + 1)
    self.printValue('Values: ', aStream, aLevel)
  end
  
  def printSubcomponentsOn(aStream level: aLevel)
    self.printSubcomponents(self.values, aStream, aLevel)
  end
  
  ##----------
  ## instance creation
  
  def StreamBranchParseNode.ifNone(&block1)
    ^self.new.ifNone(&block1)
  end
  
  def StreamBranchParseNode.on(aValue, &ifNone)
    self.on(aValue).ifNone(&block1)
  end
  
end

# Category: TextFileParser
class StreamCompositionParseNode < StreamComponentParseNode
  attr_accessor :components, :valueBlock
=begin
Do not use this in place of a StreamDescriptionParseNode, StreamTranslationParseNode, or a StreamBranchParseNode
  
  Instances of me contain a collection of component nodes and a valueBlock whose argument count is the number of nodes.  When an instance is evaluated it evaluates each of the components and then evaluates the block using the component values as parameters.  The value answered by my instance is the value answered by its block
  
  This node is polymorphic with the others in the sense that it has a component node (established using :on:).  The only distinction between the *component* and the *components* is that the component is passed into the valueBlock as the first argument.  You may choose to distinguish the component in the valueBlock, but this node does not care
=end
  
  ##----------
  ## accessing
  
  def add(aComponent)
    # Add a component node to my collection.
    self.components << aComponent
  end
  
  def value
    # Answer the value of my valueBlock evaluated with
    # each of my components' values.
  
    str = [self.component.value] #size = (self.components size + 1)
    self.components.each {|each| str << each.value}
    self.valueBlock.value(*str)
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    components = []
    valueBlock = nil
  end
  
  ##----------
  ## instance creation
  
  def StreamCompositionParseNode.on(*args)
    answer = self.on(one)
    args.each {|ea| answer << ea}
    answer
  end
  

end

# Category: TextFileParser
class StreamDescriptionParseNode < StreamComponentParseNode
  attr_accessor :attributes
=begin
A StreamDescriptionParseNode takes the object answered by the evaluation of its component, and makes that object conform to the description given by the collection of attribute nodes.  The initialized object is answered as a result
=end
  
  ##----------
  ## accessing
  
  def components
    super << self.attributes
  end
  
  def value
    self.processNewComponent(super)
  end
  
  ##----------
  ## actions
  
  def processNewComponent(aValue)
    self.initializeNewComponent(aValue)
  end
  
  ##----------
  ## addition/removal
  
  def <<(aValue)
    self.attributes << aValue
  end
  
  def []=(aKey, aValue)
    self << (aValue.asAttributeNamed(aKey))
  end
  
  def remove(aValue)
    self.attributes.delete(aValue)
  end
  alias :delete :remove
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    attributes = []
  end
  
  def initializeNewComponent(aComponent)
    self.attributes.each {|attrib|
      attrib.initializeClient(aComponent, attrib.value)
    }
    aComponent
  end
  
  ##----------
  ## printing
  
  def printOn(aStream, aLevel)
    # do nothing
  end
  
end

# Category: TextFileParser
class StreamPersistentObjectDescriptionNode < StreamDescriptionParseNode
  attr_accessor :database, :ifOneBlock, :ifManyBlock, :ifFoundBlock, :ifNoneBlock, :newClientTranslator, :predicate, :shouldUseSubclasses, :variableNames
=begin
An StreamPersistentObjectDescriptionNode takes the object answered by the evaluation of its component, and asks for a persistent object of the same class which conforms to the description given by the collection of attribute nodes.  If one is found, answer the value of the ifFound block (see below for a more thorough description of the blocks).  If many are found, use the ifMany block as a filter against the answer set.  If none is found, answer the value of the ifNone block
  
  IfMany
    This block takes the new client and the answer set as its parameter.  It should answer as small a subset of that collection as it can.  The default is to answer the supplied collection:
    {|client, collection| collection}
  If it answers an empty or a one-element collection, proceed as if that had been the databases'' response (evaluate the ifNone or ifFound block, respectively).  If it answers a collection of any other size, log the error, select one arbitrarily, and proceed (without evaluating either the ifFound or ifNone blocks)
  
  ifFound
    This block takes the persistent object as a parameter.  The default is to just answer that parameter directly:
      {|value| value}
    Although some translation could be performed within the block:
      {|value| value isOutOfDate
        ifTrue: {self.defaultValue}
        ifFalse({value}})
    for nontrivial translations it is better to just use the default ifFound block, and do the translation in a
    StreamTranslationParseNode that contains the StreamPersistentObjectDescriptionNode
  
  ifNone
    This block takes two parameters.  The first parameter is the value of my component (the template object).  The second parameter is a one-argument block that can be used to initialize an object of the same type such that it satisfies the descriptions given by my attributes.  For example, the ifNone block:
      {|component, initializer| initializer value: component}
  would initialize the component and answer it as my value.  The default ifNone block raises an exception providing the two block parameters as exception parameters
=end

  ##----------
  ## accessing
  
  def attributeClass
    # Answer the class of the attribute wrapper that I require.
    StreamAttributeReferenceParseNode
  end
  
  def defaultDatabase
    self.database
  end
  
  alias :ifFound :ifFoundBlock
  alias :ifMany  :ifManyBlock
  alias :ifNone  :ifNoneBlock
  
  def newComponentInitializer
    {|aClient| self.initializeNewComponent(aClient)}
  end
  
  ##----------
  ## actions
  
  def processNewComponent(aValue)
    # Answer the instance of aValue that is uniquely
    # described by my attributes.
  
    # evaluate each of the attributes first for consistency
    self.attributes.each {|each| each.value}
    self.singleInstanceOf(aValue)
  end
  
  def useIfNoneThenNilBlock
    self.ifNone(self.ifNoneThenNilBlock)
  end
  
  def useIfNoneThenPrototypeWithErrorBlock
    self.ifNone(self.ifNoneThenPrototypeWithErrorBlock)
  end
  
  def useIfNoneThenPrototypeWithoutErrorBlock
    self.ifNone(self.ifNoneThenPrototypeWithoutErrorBlock)
  end
  
  ##----------
  ## initialize-release
  
  def ifNoneThenNilBlock
    {|client, initializer|
    self.persistentObjectNotFoundBlock.value(client, initializer)
    self.context.traceWith('  Proceeding with \'nil\'.')
    nil}
  end
  
  def ifNoneThenPrototypeWithErrorBlock
    {|client, initializer|
    self.persistentObjectNotFoundBlock.value(client, initializer)
    self.context.traceWith('  Proceeding with prototype.')
    self.ifNoneThenPrototypeWithoutErrorBlock.value(client, initializer)
  end
  
  def ifNoneThenPrototypeWithoutErrorBlock
    {|client, initializer| initializer value: client}
  end
  
  def initialize
    super
    database = self.defaultDatabase
    ifManyBlock = {|client, collection| collection}
    ifFoundBlock = :yourself
    ifNoneBlock = self.ifNoneThenPrototypeWithErrorBlock
    shouldUseSubclasses = true
  end
  
  def initializeNewComponent(aClient)
    self.attributes.each {|each|
      each.initializeClient(aClient, each.lastValue)
    }
    aClient.becomePersistentIn(self.database)
    aClient
  end
  
  def persistentObjectNotFoundBlock
    {|client, initializer|
    self.context.persistentObjectNotFoundSignal.
      raise(PersistentObjectNotFoundSummary.new(client, initializer, self.predicate))}
  end
  
  ##----------
  ## utility
  
  def handleUndefinedClassWith(&handleBlock)
    yield self
  rescue VError => ex
    if VError.errorSymbol == :VSI_WARNING && VError.description =~ 'Class not found'
      handleBlock.value(ex)
    else
      ex.reject
    end
  end
  
  def persistentInstancesOf(newClient)
    attributeCount = self.attributes.size
    query, keys, values = [], [], []
    self.attributes.each {|attrib| 
      attrib.addQueryTo(query, keys, values)}
    self.handleUndefinedClassWith: {|ex| []} {| myPredicate |
      myPredicate = VPredicate.from(self.searchStringFrom(query))
      myPredicate[keys]=values
      self.predicate(myPredicate)
      newClient.class.selectFrom(self.database, myPredicate)}
  end
  
  def searchStringFrom(aQueryList)
    str = []
    aQueryList.each_separatedBy(lambda {str << ' & '}) {|each|
      str << each}
    str.join
  end
  
  def singleElementFrom(aCollection, manyBlock, foundBlock, noneBlock)
    hits = (aCollection.size > 1) ? manyBlock.value : aCollection
  
    if hits.size == 1
      foundBlock.value(hits.someElement)
    elsif hits.size == 0
      noneBlock.value
    else
      self.context.newLineTraceWith('Unable to resolve instance.  Choosing one arbitrarily.')
      hits.someElement
    end
  end
  
  def singleInstanceOf(newClient)
    persistentInstances = self.persistentInstancesOf(newClient)
    initializer = self.newComponentInitializer
    self.singleElementFrom(persistentInstances,
	:ifMany => lambda {self.ifMany.value(initializer.value(newClient), persistentInstances)},
	:ifFound => lambda {self.ifFound},
	:ifNone => lambda {self.ifNone value: newClient value: initializer})
  end
  
  ##----------
  ## instance creation
  
  def StreamPersistentObjectDescriptionNode.on(aNode, aDatabase, args)
    answer = self.on(aNode, aDatabase)
    answer.ifMany=  args[:ifMany]
    answer.ifFound= args[:ifFound]
    answer.ifNone=  args[:ifNone]
    answer
  end

end

# Category: TextFileParser
class StreamPersistentObjectCachedDescriptionNode < StreamPersistentObjectDescriptionNode
  attr_accessor :clientCache
=begin
Instances of me specialize the behavior of my superclass by providing a cache of past answers.  All previous matches are cached, and the cache is checked before the database.  Since nothing in the cache can be garbage collected (until the instance holding the cache is), this has the added benefit of maintaining all complete dereferenced paths from those elements from being garbage collected, thereby assuring that the attribute data need not be fetched multiple times
  
  Instances of this class should be used sparingly.  As with all caching strategies, the decision of whether and what to cache can be difficult.  Considerations include:
    the larger the cache
      the less application memory available
      the more overhead it requires (storage of indexes, empty slots for growth, etc.)
      the more expensive it is to search (therefore the more expensive a miss)
    the smaller the cache
      the larger the percentage of cache resources consumed by overhead
      the more frequent the miss
  
  Success involves identifying a small subset of the possible data that is most frequently requested by the client, and caching only that
=end
  
  ##----------
  ## accessing
  
  def value
    # Add the newly retrieved element to the cache, and answer it.
  
    (self.getClientCache << super).uniq
  end
  
  ##----------
  ## initialize-release
  
  def initialize
    super
    self.initializeClientCache
  end
  
  def initializeClientCache
    clientCache = []
  end
  
  ##----------
  ## private
  
  def cachedInstancesOf(newClient)
    # Answer the elements in the cache completely described
    # by my attributes
  
    self.getClientCache.reject {|aClient|
      self.attributes.include? {|attrib|
	(attrib.stReader.value(aClient)) != (attrib.lastValue)}}
  end
  
  def cachedInstancesOf(newClient &ifNone)
    # Answer the elements in the cache completely described
    # by my attributes, or if there are none, the value of the ifNone block.
  
    instances = self.cachedInstancesOf(newClient)
    instances.empty? ? yield : instances
  end
  
  def getClientCache
    @clientCache
  end
  
  def persistentInstancesOf(newClient)
    # Answer the elements in the cache completely described
    # by my attributes, or if there are none, try in the database.
  
    self.cachedInstancesOf(newClient).or_if_empty {super}
  end
  
end

class StreamPersistentObjectLimitedCachedDescriptionNode < StreamPersistentObjectCachedDescriptionNode
  attr_reader :maxSize
  attr_accessor :maxSize, :queryHistory
=begin
Just like instances of my superclass, instances of me maintain a cache.  My cache, however, is of a fixed size.  Whenever the addition of a new element causes the cache to grow beyond that size I remove from my cache the oldest element
=end
  
  ##----------
  ## accessing
  
  def add(aValue)
  
    # is it already in the cache?
    if self.getClientCache.include?(aValue)
      # move it to the front of the history list
      queryHistory.delete(aValue)
      queryHistory.shift(aValue)
    else
      super
      queryHistory.shift(aValue)
      self.recalibrate
    end
    aValue
  end
  
  def maxSize(aValue)
    maxSize = aValue
    self.recalibrate
  end
  
  ##----------
  ## actions
  
  def recalibrate
    # If necessary, reduce the cache size to maxSize by
    # removing the oldest elements.
  
    (queryHistory size - self.maxSize).times {
      clientCache.delete(queryHistory.pop)
    }
  end
  
  ##----------
  ## initialize-release
  
  def initializeClientCache
    super
    queryHistory = Array.new
  end
  
  ##----------
  ## private
  
  def getQueryHistory
    queryHistory
  end
end

# Category: TextFileParser
class StreamAttributeParseNode < StreamComponentParseNode
  attr_accessor :lastValue, :odbReader, :stReader, :stWriter
=begin
Instances of me have a component that answers an object, and a set of functions that map that value to a slot.  I do not perform the actual mapping, I simply store the information on how to do it.  The different read and write syntaxes for the database and Smalltalk collections require four separate functions: :stReader, stWriter, odbReader, and odbWriter.  My two subclasses are specialized to handle the two different types of database read queries: value and reference
=end
  
  ##----------
  ## accessing
  
  def addQueryTo(aCollection keyTo: aKeyCollection valueTo: aValueCollection)
    ^aCollection add: self.odbReader
  end
  
  def odbVariableName
    ^nil
  end
  
  def slotName
    ^self.stReader isSymbol
      ifTrue({self.stReader})
      ifFalse({nil})
  end
  
  def stReaderWriterOdbReader(stReader, stWriter, odbReader)
    self.stReader(stReader)
    self.stWriter(stWriter)
    self.odbReader(odbReader)
  end
   
  def value
    # Store then answer the result of 1 computational cycle of my machinery.
  
    self.lastValue=super
  end
  
  ##----------
  ## converting
  
  def asAttributeNamed(aName)
    self.error('A ', self.class.to_s, ' cannot be converted to an attribute node.')
  end
  
  ##----------
  ## initialize-release
  
  def initializeClient(aClient, withValue)
    self.stWriter(aClient, withValue)
  end
  
  ##----------
  ## printing
  
  def printOdbReaderOn(aStream)
    aStream << self.odbReader
    self
  end
  
  def printOn(aStream)
    aStream << nextPut($?)
  end
  
  ##----------
  ## testing
  
  def needsOdbParameter
    # Answer whether the query that I represent has a
    # replaceable parameter.
  
    false
  end
  
end

class StreamAttributeParseNode class
  ##----------
  ## instance creation
  
  def StreamAttributeParseNode.on(aValue, slotName)
    self.subclassResponsibility
  end
  
  def StreamAttributeParseNode.onReferenceTest(aValue, stReader, stWriter, odbReferenceTest, odbVariableName)
    node=StreamAttributeReferenceParseNode.on(aValue)
    node.stReader(stReader)
    node.stWriter(stWriter)
    node.odbReferenceTest(odbReferenceTest)
    node.odbVariableName(odbVariableName)
  end
  
  def StreamAttributeParseNode.onValueTest(aValue, stReader, stWriter, odbValueTest)
    node=StreamAttributeValueParseNode.on(aValue)
    node.stReader(stReader)
    node.stWriter(stWriter)
    node.odbEquivalenceTest(odbQuery)
  end
  
  ##----------
  ## private
  
  def StreamAttributeParseNode.stAccessorForSlotNamed(aName)
    aName.to_sym
  end
  
  def StreamAttributeParseNode.stMutatorForSlotNamed(aName)
    (aName + '=').to_sym
  end
  
end

# Category: TextFileParser
class StreamAttributeValueParseNode < StreamAttributeParseNode
  attr_accessor :genericOdbReader
=begin
Instances of me are specialized to perform database searches for object whose slot (which I identify) contains a value that is similar to the value of my component.  This is typically a string, or a number
=end
  
  ##----------
  ## accessing
  
# todo start here ###########################################################

  def odbReader
    # Since the syntax of my odb query depends upon my value,
    # I make that adjustment now.
  
    @odbReader || self.printOdbReaderOn(String.new.writeStream).contents
  end
  
  def stReader(stReader, stWriter, odbEquivalenceTest)
    self.stReader(stReader, stWriter, odbReader(''))
    self.genericOdbReader(odbEquivalenceTest)
  end
  
  def value
    # Answer the result of 1 computational cycle of my machinery.
  
    self.odbReader(nil.  # invalidate my cached odbReader)
    ^super value
  end
  
  ##----------
  ## printing
  
  def printOdbReaderOn(aStream)
    ^aStream
      nextPutAll(self.genericOdbReader;)
      print: self.lastValue;
      self
  end
  
  def printOn(aStream)
  
    super printOn(aStream)
    self.genericOdbReader == nil
      ifTrue({aStream print: self.stReader})
      ifFalse: {aStream print: self.genericOdbReader}
  end
  
  StreamAttributeValueParseNode class instanceVariableNames('')
  
end

class StreamAttributeValueParseNode class
  ##----------
  ## instance creation
  
  def on(aValue slotName: aName)
  
    ^self
      on: aValue
      stReader: (self.stAccessorForSlotNamed(aName))
      stWriter: (self.stMutatorForSlotNamed(aName))
      odbValueTest: aName.clone, ' = '
  end
  

end

# Category: TextFileParser
class StreamAttributeReferenceParseNode < StreamAttributeParseNode
  attr_accessor :odbVariableName
=begin
Instances of me are specialized to perform database searches for object whose slot (which I identiry) contains a reference to the value of my component (which is expected to be a persistent object)
=end
  
end

class StreamAttributeReferenceParseNode
  ##----------
  ## accessing
  
  def addQueryTo(aCollection keyTo: aKeyCollection valueTo: aValueCollection)
    aKeyCollection add: self.odbVariableName
    aValueCollection add(lastValue)
    ^super
      addQueryTo: aCollection
      keyTo: aKeyCollection
      valueTo: aValueCollection
  end
  
  def odbVariableName
    ^odbVariableName
  end
  
  def stReader(block1 stWriter: block2 odbReferenceTest: odbQuery odbVariableName: aVariableName)
    self
      stReader: block1
      stWriter: block2
      odbReader: odbQuery
    odbVariableName := aVariableName
  end
  
  ##----------
  ## printing
  
  def printOn(aStream)
    super printOn: aStream
    aStream
      print: self.odbReader
  end
  
  ##----------
  ## testing
  
  def needsOdbParameter
    # Answer whether the query that I represent has a
    # replaceable parameter.
  
    ^true
  end
  
  StreamAttributeReferenceParseNode class instanceVariableNames('')
  
end

class StreamAttributeReferenceParseNode class
  ##----------
  ## instance creation
  
  def on(aValue slotName(aName))
  
    | aNameString |
    aNameString := aName.to_s.clone
    ^self
      on: aValue
      stReader: (self.stAccessorForSlotNamed(aName))
      stWriter: (self.stMutatorForSlotNamed(aName))
      odbReferenceTest: aNameString, ' = :', aNameString
      odbVariableName: aName
  end
  

end

# Category: TextFileParser
class StreamBasicParseNode < StreamParseNode
  attr_accessor :delimiter, :sourceStream, :width
=begin
Instances of me know how to read # terminal values from the database.  By # terminal, I mean objects that are not normally considered to have links to other objects, for instance integers, strings, dates, etc.  This is of course independent upon whether they are implemented that way.  (For instance, a String is actually a collection of pointers to characters, but that is ignored in most problem domains.)  I am prepared to deal with files containing either fixed-width or delimited columns
=end
  
end

class StreamBasicParseNode
  ##----------
  ## accessing
  
  def attributeClass
    # Answer the class of the attribute wrapper that I require.
  
    ^StreamAttributeValueParseNode
  end
  
  def component
    # Answer the node that provides my input.
    # For database storage reasons the string needs to
    # be a ByteString
  
    ^ByteString new(self.width)
  end
  
  def components
    ^OrderedCollection new
  end
  
  def dataType
    ^self.class dataType
  end
  
  def delimitedValue
  
    ^self.delimiter size > 1
      ifTrue({self.vectorDelimitedValue})
      ifFalse({self.scalarDelimitedValue})
  end
  
  def delimiter
    ^delimiter
  end
  
  def delimiter(aValue)
  
    aValue == nil
      ifTrue: {delimiter := nil}
      ifFalse: {
        delimiter := aValue
        self.width(nil)
      }
  end
  
  def scalarDelimitedValue
    ^self.sourceStream upTo(self.delimiter)
  end
  
  def sourceStream
    ^sourceStream
  end
  
  def sourceStream(aReadStream)
    sourceStream := aReadStream
  end
  
  def value
    # Answer the string value that I represent.
  
    ^self.shouldUseDelimitedFormat
      ifTrue({self.delimitedValue})
      ifFalse({self.widthValue})
  end
  
  def vectorDelimitedValue
  
    | myDelimiter myStream value |
    myStream := self.sourceStream
    myDelimiter := self.delimiter
    value := myStream upToAll: myDelimiter
    myStream skip: myDelimiter size
    ^value
  end
  
  def width
  
    ^width
  end
  
  def width(aValue)
  
    aValue == nil
      ifTrue: {width := nil}
      ifFalse: {
        width := aValue
        self.delimiter(nil)
      }
  end
  
  def widthValue
    # Answer the value I represent, constraining
    # the number of characters.
  
    ^self.sourceStream
      next(self.width)
      into(super value)
      startingAt: 1
  end
  
  ##----------
  ## inquiries
  
  def attributeCount
    # Answer the count of attributes of which I am composed.  I am
    # exactly 1 attribute.
  
    1
  end
  
  def mapsDirectlyToFile
    true
  end
  
  def shouldUseDelimitedFormat
    self.delimiter != nil
  end
  
  def shouldUseFixedWidthFormat
    ^self.shouldUseDelimitedFormat not
  end
  
  ##----------
  ## printing
  
  def printOn(aStream)
  
    aStream print(self.dataType)
    self.shouldUseFixedWidthFormat
      ifTrue({)
        aStream
          nextPut: ${;
          print: self.width;
          nextPut($})
      }
      ifFalse: {
        aStream nextPutAll: ', <'
        self.delimiter == $/
          ifTrue({aStream nextPutAll: 'CR'})
          ifFalse: {
            self.delimiter == Character tab
              ifTrue({aStream nextPutAll: 'TAB'})
              ifFalse: {aStream print: self.delimiter}
          }
        aStream nextPut($>)
      }
  end
  
  def printOn(aStream level: aLevel)
  
    aStream
      print: self
  end
  
  StreamBasicParseNode class instanceVariableNames: ''
  
end

class StreamBasicParseNode class
  ##----------
  ## accessing
  
  def defaultDelimiter
    # Answer the delimiter I should assume when I
    # am not provided one.
  
    ^$/
  end
  
  ##----------
  ## instance creation
  
  def on(aReadStream forType: aDataType)
    # Answer a delimited node specialized for dealing
    # with aDataType.
  
    | aClassOrNil |
    aClassOrNil := self.classForType(aDataType)
    ^aClassOrNil == nil
      ifTrue: {nil}
      ifFalse: {
        aClassOrNil new
          sourceStream: aReadStream;
          delimiter: self.defaultDelimiter
      }
  end
  
  def on(aReadStream forType(aDataType delimiter: aDelimiter))
    # Answer a a delimited node specialized for dealing
    # with aDataType.
  
    ^(self.on(aReadStream forType: aDataType))
      delimiter: aDelimiter
  end
  
  def on(aReadStream forType: aDataType width: anInteger)
    # Answer a fixed-width node specialized for dealing
    # with aDataType.
  
    ^(self.on(aReadStream forType: aDataType))
      width: anInteger
  end
  
  ##----------
  ## utility
  
  def classForType(aDataType)
    # Answer the subclass that is designed to handle data of type 'aDataType'.
  
    ^self.allSubclasses
      detect({|each| each dataType == aDataType})
      ifNone: {}
  end
  
  StreamBasicParseNode subclass: :CharacterParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class CharacterParseNode
  ##----------
  ## accessing
  
  def scalarDelimitedValue
    # Answer the first character contained in
    # the string that I represent.
  
    | stream value |
    stream := self.sourceStream
    ^(value := stream next) = self.delimiter
      ifTrue({nil})
      ifFalse: {
        stream skipThrough: self.delimiter
        value
      }
  end
  
  def vectorDelimitedValue
    # Answer the first character contained in
    # the string that I represent.
  
    | stream value |
    stream := self.sourceStream
    value := stream upToAll: self.delimiter
    stream skip(self.delimiter size)
    nil halt: 'I should check if it is scalar, not if it is size == 1'
    ^value size > 1
      ifTrue: {value first}
      ifFalse: {value}
  end
  
  def widthValue
    # Answer the first character contained in
    # the string that I represent.
  
    | stream value |
    stream := self.sourceStream
    value := stream next
    stream skip: self.width - 1
    ^value
  end
  
  CharacterParseNode class instanceVariableNames('')
  
end

class CharacterParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Character
  end
  
  CharacterParseNode subclass: :BooleanParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class BooleanParseNode
  ##----------
  ## accessing
  
  def value
    # Answer the boolean value that I represent.
  
    | booleanChar |
    booleanChar := super value asUppercase
    ^(booleanChar == $T or: {booleanChar == $Y})
      ifTrue: {true}
      ifFalse: {
        (booleanChar == $F or: {booleanChar == $N})
          ifTrue: {false}
          ifFalse: {
            self.unexpectedValueSignal
              raiseRequestErrorString(' ',)
              booleanChar printString,
              ' <- One of {YyTtFfNn} expected.'
        }
      }
  end
  
  BooleanParseNode class instanceVariableNames: ''
  
end

class BooleanParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Boolean
  end
  
  StreamBasicParseNode subclass: :StringParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class StringParseNode
  ##----------
  ## accessing
  
  def changeWidthTo(aWidth while: block0)
    | value oldWidth |
  
    {oldWidth := self.width
    self.width(aWidth)
    value := block0 value}
      valueNowOrOnUnwindDo: {self.width(oldWidth})
    ^value
  end
  
  def value
    # Answer the string value that I represent.
  
    ^super value withoutLeadingOrTrailingWhitespace
  end
  
  def widthValue
    # Answer the string contained in the next fieldWidth characters on
    # my sourceStream.
  
    | aWidth space |
    # do not read leading spaces
    aWidth := self.width
    space := Character space
    {aWidth > 0 and: {sourceStream next == space}}
      whileTrue: {aWidth := aWidth - 1}
    aWidth == 0
      ifFalse: {self.sourceStream skip(-1})
  
    # The template we create for this client must be smaller than the default
    # width.  We return it to the original state to prepare for the next client.
    ^self
      changeWidthTo: aWidth
      while: {super widthValue}
  end
  
  StringParseNode class instanceVariableNames: ''
  
end

class StringParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process
    # Subclasses should override this.
  
    ^:String
  end
  
  StringParseNode subclass: :TitleParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class TitleParseNode
  ##----------
  ## accessing
  
  def value
    # Answer the title value that I represent.
  
    ^super value capitalizeWords
  end
  
  TitleParseNode class instanceVariableNames('')
  
end

class TitleParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Title
  end
  
  StreamBasicParseNode subclass: :TimeParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class TimeParseNode
  ##----------
  ## accessing
  
  def value
    ^Time readFrom((super value) readStream)
  end
  
  TimeParseNode class instanceVariableNames: ''
  
end

class TimeParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Time
  end
  
  StreamBasicParseNode subclass: :NumberParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class NumberParseNode
  ##----------
  ## accessing
  
  def clientClass
    ^Number
  end
  
  def value
    # Answer the number value that I represent.
  
    ^self.clientClass readFrom(super value readStream)
  end
  
  NumberParseNode class instanceVariableNames('')
  
end

class NumberParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Number
  end
  
  NumberParseNode subclass: :MoneyParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class MoneyParseNode
  ##----------
  ## accessing
  
  def value
    # Answer the money value that I represent
    # To avoid rounding errors I answer money as
    # number of cents.
  
    ^(super value * 10) asInteger
  end
  
  MoneyParseNode class instanceVariableNames('')
  
end

class MoneyParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Money
  end
  
  NumberParseNode subclass: :IntegerParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class IntegerParseNode
  ##----------
  ## accessing
  
  def clientClass
  
    ^Integer
  end
  
  IntegerParseNode class instanceVariableNames('')
  
end

class IntegerParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Integer
  end
  
  StreamBasicParseNode subclass: :NullParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class NullParseNode
  ##----------
  ## accessing
  
  def dataType
    ^:nil
  end
  
  def delimitedValue
    super delimitedValue
    ^nil
  end
  
  def scalarDelimitedValue
  
    self.sourceStream skipThrough(self.delimiter)
    ^nil
  end
  
  def vectorDelimitedValue
  
    self.sourceStream
      skipToAll(self.delimiter;)
      skip(self.delimiter size)
    ^nil
  end
  
  def widthValue
    # Ignore the information stored in the next field.
  
    self.sourceStream skip(self.width)
    ^nil
  end
  
  NullParseNode class instanceVariableNames('')
  
end

class NullParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^nil
  end
  
  StreamBasicParseNode subclass(:TimestampParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser')
  
end

class TimestampParseNode
  ##----------
  ## accessing
  
  def value
    # Answer the timestamp value that I represent.
  
    | tsString |
    ^(tsString := super value asUppercase) = 'MAXIMUM'
      ifTrue: {Timestamp openEnded}
      ifFalse: {tsString = 'MINIMUM'
            ifTrue: {Timestamp arbitraryBegin}
            ifFalse: {Timestamp readFrom: tsString readStream}}
  end
  
  TimestampParseNode class instanceVariableNames: ''
  
end

class TimestampParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Timestamp
  end
  
  StreamBasicParseNode subclass: :DateParseNode instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'TextFileParser'
  
end

class DateParseNode
  ##----------
  ## accessing
  
  def specialMappings
    # Answer mappings from the special date forms to the dates
    # they represent.
  
    ^OrderedCollection
      add('MAXIMUM' -> Date openEnded;)
      add: 'MINIMUM' -> Date arbitraryBegin;
      add: nil -> nil;
      self
  end
  
  def value
    # Answer the date value that I represent.
  
    | dateString |
    dateString := super value asUppercase
    ^self.specialMappings
      detect({|each|)
        dateString = each key
          ifTrue: {each value value}}
      ifNone: {Date readFrom: dateString readStream}
  end
  
  DateParseNode class instanceVariableNames: ''
  
end

class DateParseNode class
  ##----------
  ## accessing
  
  def dataType
    # Answer the data type that I am designed to process.
  
    ^:Date
  end
  
end

class Object
  ##----------
  ## accessing
  
  def attributeClass
    # Answer the class of the attribute wrapper that I require.
  
    ^StreamAttributeValueParseNode
  end
  
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^false
  end
  
end

class Boolean
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^true
  end
  
end

class UndefinedObject
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^true
  end
  
end

class Character
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^true
  end
  
end

class SmallInteger
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^true
  end
  
end

class CharacterArray
  ##----------
  ## converting
  
  def capitalizeWords
    # 'paul SCHAAF' capitalizeWords
    # P.G. Schaaf, 06/09/95
  
    | capitalize |
    capitalize := true
    ^self.collect({|each|)
      (each isWhite or: {each isDigit or: {each isLeftParenthetical}})
        ifTrue:
          {capitalize := true
          each}
        ifFalse:
          {(capitalize and: {each isAlphabetic})
            ifTrue:
              {capitalize := false
              each asUppercase}
            ifFalse: {each asLowercase}}}
  end
  
end

class Symbol
  ##----------
  ## testing
  
  def isImmediateValue
    # Answer whether the receiver has an optimal
    # Smalltalk representation.
  
    ^true
  end
  StreamParseNode initialize
  
  
end
