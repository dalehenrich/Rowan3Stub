! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'JadeServer'
	instVarNames: #(classList classOrganizers readStream writeStream selectedClass methodFilterType methodFilters selections methodCommandResult)
	classVars: #(ExternalInteger GciError GsObject OopType32 OopType64)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer
	subclass: 'JadeServer64bit'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit
	subclass: 'JadeServer64bit24'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit24
	subclass: 'JadeServer64bit3x'
	instVarNames: #(environment)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit3x
	subclass: 'JadeServer64bit32'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit32
	subclass: 'JadeServer64bit35'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RowanCommandResult'
	instVarNames: #(executionTime)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Simple object with the results of the Jadeite command.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RowanService'
	instVarNames: #(definition updates command commandArgs updateType organizer)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Rowan Service is the abstract service class for classes that represent
first class entities in Rowan. They are transported to the client via 
ston. 

On the client, set the command & commandArgs inst vars, then tell
the browser to #issueCommand: with an array of services. A service
received without the the command inst var set will send #update to
the service. #issueCommand: should call JadeServer>>updateFromSton:
which will run the command and return a result. 

Any service that sends updates back to the client willl propogates to 
registered windows. Add a service for return to the client with:
RowanCommandResult addResult: <self>';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanAnsweringService'
	instVarNames: #(answer)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'A place to put miscellaneous commands that don''t fit 
well in other services. 

Also good asking the server questions as it gives back 
an answer whereas other services simply return updated
services.';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanAutoCommitService'
	instVarNames: #(autoCommit)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Simple service that provides updates to the client related to auto commit.';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanBrowserService'
	instVarNames: #(projects removedMethods allClasses hierarchyServices testPackages testCount dictionaries selectedClass newCachedSelectors newCachedClasses)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'RowanBrowserService handles services that fall outside
the scope of other services.';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanClassService'
	instVarNames: #(name comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions oop template filters filterType methods selectedPackageServices packageName definedPackageName selectedMethods projectName hierarchyServices variables categories isTestCase expand visibleTests isNewClass updateAfterCommand isInSymbolList dictionaryName wasRemoved renamedName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Most class operations done here. 

selectedMethods - client side selection. Used after a method compile.';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanDebuggerService'
	instVarNames: #(initialProcessOop processes)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanDictionaryService'
	instVarNames: #(name classes hierarchyServices globals defaultTemplate)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanFrameService'
	instVarNames: #(label method stepPoint vars oop homeMethodSelector homeMethodClassName classIsResolvable)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanInspectorService'
	instVarNames: #(oop objects myself className indexedSize visibleIndices nextIndices maxIndexedVars compileErrorArray isOop instVarNames instVarsAreRemovable isDictionary isVariable selectionOop isUnordered statusText)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanLoggingService'
	instVarNames: #(fileName id groupId date time comment services mode location isLogging)
	classVars: #(Current)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanMethodService'
	instVarNames: #(oop source selector methodDefinitions classService category packageName projectName className meta hasSupers hasSubs compilationWarnings isExtension inSelectedPackage references stepPoints selectedPackageServices superDisplayString accessedInstVars breakPoints testResult definedPackage isTestMethod testRunClassName failedCompile comparisonSource firstReference renamedName isMethodForBlock homeMethodOop hasMethodHistory searchString definedClassName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanPackageService'
	instVarNames: #(projectDefinition packageName name isDirty classes defaultTemplate projectName testClasses hierarchyServices selectedClass)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanProcessService'
	instVarNames: #(frames oop status)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanProjectService'
	instVarNames: #(rwProject name sha branch isSkew isDirty packages changes existsOnDisk isLoaded projectUrl rowanProjectsHome isDiskDirty)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanQueryService'
	instVarNames: #(queryResults)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanTestService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanVariableService'
	instVarNames: #(oop key value className)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

! Class implementation for 'JadeServer'

!		Class methods for 'JadeServer'

category: 'jadeite server'
classmethod: JadeServer
theJadeiteServer
  ^ SessionTemps current
    at: #'jadeiteServer'
    ifAbsentPut: [ 
      | jadeServerClass |
      jadeServerClass := (System _gemVersion beginsWith: '3.2')
        ifTrue: [ Rowan jadeServerClassNamed: #'JadeServer64bit32' ]
        ifFalse: [ Rowan jadeServerClassNamed: #'JadeServer64bit35' ].
      jadeServerClass new
        initialize;
        yourself ]
%

!		Instance methods for 'JadeServer'

category: 'category'
method: JadeServer
abort

	classOrganizers := Array new: 4.
	System abortTransaction.
%

category: 'category'
method: JadeServer
addAccessorsFor: aString inBehavior: aBehavior

	aBehavior compileAccessingMethodsFor: (Array with: aString asSymbol).

%

category: 'category'
method: JadeServer
addCategory: aString to: aClass 

	aClass addCategory: aString.

%

category: 'category'
method: JadeServer
addGroup: aString toUser: aUserProfile

	aUserProfile addGroup: aString.

%

category: 'category'
method: JadeServer
addMethodCategoryNamesToMethodFilters

	classList do: [:each | methodFilters addAll: each categoryNames].

%

category: 'category'
method: JadeServer
addMissingAccessorsFor: aClass

	aClass compileMissingAccessingMethods.

%

category: 'category'
method: JadeServer
addPrivilege: aString toUser: aUserProfile

	aUserProfile addPrivilege: aString.

%

category: 'category'
method: JadeServer
addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler

	| x |
	aStream lf
"1"	nextPutAll: aString; tab;
"2"	nextPutAll: aProcess asOop printString; tab;
"3"	nextPutAll: aProcess priority printString; tab;
"4"	nextPutAll: (aProcess createdByApplication ifTrue: ['Y'] ifFalse: ['']); tab; 
"5"	nextPutAll: ((x := aProcess stackId) == -1 	ifTrue: [''] ifFalse: [x printString]); tab;
"6"	nextPutAll: ((x := aProcess waitingOn) 	isNil ifTrue: [''] ifFalse: [x asOop printString]); tab;
"7"	nextPutAll: ((x := aProcess _signalTime) 	isNil ifTrue: [''] ifFalse: [(x - aScheduler _now) printString]); tab;
"8"	nextPutAll: (aProcess isPartialContinuation	ifTrue: ['partial'] ifFalse: [aProcess isContinuation ifTrue: ['full'] ifFalse: ['']]); tab;
"9"	"type: forked or main"
"10"	"live or terminated"
	yourself.

%

category: 'category'
method: JadeServer
addSessionWithId: anInteger toStream: aStream

	| array gsSession timeGmt x |
	array := System descriptionOfSession: anInteger.
	array size: 20.
	gsSession := GsSession sessionWithSerialNumber: (array at: 9).
	timeGmt := System timeGmt.
	aStream
		nextPutAll: '<session oop=';
		nextPutAll: (self oopOf: gsSession) printString printString;
		nextPutAll: ' name=';
		nextPutAll: (array at: 1) userId printString;
		nextPutAll: ' process=';
		nextPutAll: (array at: 2) printString printString;
		nextPutAll: ' host=';
		nextPutAll: (array at: 3) printString;
		nextPutAll: ' primitive=';
		nextPutAll: (array at: 4) printString printString;
		nextPutAll: ' viewAge=';
		nextPutAll: (timeGmt - (array at: 5)) printString printString;
		nextPutAll: ' state=';
		nextPutAll: (array at: 6) printString printString;
		nextPutAll: ' transaction=';
		nextPutAll: (array at: 7) printString printString;
		nextPutAll: ' hasOldestCR=';
		nextPutAll: (array at: 8) printString printString;
		nextPutAll: ' serial=';
		nextPutAll: (array at: 9) printString printString;
		nextPutAll: ' id=';
		nextPutAll: (array at: 10) printString printString;
		nextPutAll: ' ip=';
		nextPutAll: (array at: 11) printString;
		nextPutAll: ' priority=';
		nextPutAll: ((x := array at: 12) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' hostId=';
		nextPutAll: ((x := array at: 13)  isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' quietTime=';
		nextPutAll: ((x := array at: 14) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' lifeTime=';
		nextPutAll: ((x := array at: 15) isNil ifTrue: [''] ifFalse: [(timeGmt - x)  printString]) printString;
		nextPutAll: ' backlog=';
		nextPutAll: ((x := array at: 16) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' description=';
		nextPutAll: ((x := array at: 17) isNil ifTrue: [''] ifFalse: [x]) printString;
		nextPutAll: ' objects=';
		nextPutAll: ((x := array at: 18) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' pages=';
		nextPutAll: ((x := array at: 19) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' voteState=';
		nextPutAll: ((x := array at: 20) isNil ifTrue: [''] ifFalse: [x printString]) printString;
		nextPutAll: ' />';
		yourself.

%

category: 'category'
method: JadeServer
addUser: aUserProfile toStream: aStream

	(self oopOf: aUserProfile) printOn: aStream.
	aStream tab; nextPutAll: aUserProfile userId.
	aStream tab; nextPutAll: (aUserProfile lastLoginTime asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile loginsAllowedBeforeExpiration printOn: aStream.
	aStream tab. aUserProfile isDisabled printOn: aStream.
	aStream tab. aUserProfile activeUserIdLimit printOn: aStream.
	aStream tab.	"; nextPutAll: aUserProfile nativeLanguage asString."
	aStream tab. aUserProfile reasonForDisabledAccount printOn: aStream.
	aStream tab; nextPutAll: (aUserProfile lastPasswordChange asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile passwordNeverExpires printOn: aStream.
	aStream lf.

%

category: 'category'
method: JadeServer
allGroups

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := (AllUsers userWithId: 'GcUser') groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
allSessions

	| list stream |
	stream := WriteStream on: String new.
	stream nextPutAll: '<?xml version=''1.0'' ?><sessions>'.
	list := System currentSessionNames subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | (each subStrings at: 3) asNumber].
	list do: [:each | 
		self
			addSessionWithId: each
			toStream: stream.
	].
	^stream 
		nextPutAll: '</sessions>';
		contents.

%

category: 'category'
method: JadeServer
allUsersPasswordLimits

	| stream |
	stream := WriteStream on: String new.
	AllUsers disallowUsedPasswords printOn: stream. stream tab.
	AllUsers minPasswordSize printOn: stream. stream tab.
	AllUsers maxPasswordSize printOn: stream. stream tab.
	AllUsers maxRepeatingChars printOn: stream. stream tab.
	AllUsers maxConsecutiveChars printOn: stream. stream tab.
	AllUsers maxCharsOfSameType printOn: stream. stream tab.
	AllUsers staleAccountAgeLimit printOn: stream. stream tab.
	AllUsers passwordAgeLimit printOn: stream. stream lf.
	AllUsers disallowedPasswords do: [:each | 
		stream nextPutAll: each; tab.
	].
	stream lf. AllUsers passwordAgeWarning printOn: stream. stream lf.
	^stream contents.

%

category: 'category'
method: JadeServer
asAsciiString: aString

	^String withAll: (aString asArray collect: [:char | 
		((32 <= char asciiValue and: [char asciiValue <= 127]) or: [char isSeparator])
			ifTrue: [char]
			ifFalse: [$?].
	]).

%

category: 'category'
method: JadeServer
assignClass: aClass toCategory: aString

	aClass thisClass category: aString.

%

category: 'category'
method: JadeServer
asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | 
			^'<<printString error: ' , ex printString , '>>'.
		].
	^anObject printString.

%

category: 'category'
method: JadeServer
authorInitials: aString

	| packagePolicy |
	(packagePolicy := self gsPackagePolicy) isNil ifTrue: [^self].
	packagePolicy authorInitials: aString.

%

category: 'jadeite'
method: JadeServer
autoCommitIfRequired
	| commitResult |
	Rowan serviceClass autoCommit == true ifTrue:[
		commitResult := System commitTransaction.
		RowanAutoCommitService new autoCommit:  
			(commitResult 
				ifTrue:[true] 
				ifFalse:[#failed])].
%

category: 'category'
method: JadeServer
beginTransaction

	classOrganizers := Array new: 4.
	System beginTransaction.

%

category: 'category'
method: JadeServer
behaviorFor: selector in: aClass

	| behavior |
	behavior := aClass.
	[
		behavior notNil.
	] whileTrue: [
		(behavior includesSelector: selector) ifTrue: [^behavior].
		behavior := behavior superclass.
	].
	self error: 'Method not found in class or in any superclass'.

%

category: 'category'
method: JadeServer
categoryListFor: aSymbolDictionary

	| categories stream |
	categories := Set new.
	aSymbolDictionary do: [:each | 
		each isBehavior ifTrue: [
			categories add: each category.
		].
	].
	categories copy do: [:each | 
		1 to: each size do: [:i | 
			(each at: i) = $- ifTrue: [
				| string |
				string := each copyFrom: 1 to: i - 1.
				(categories includes: string) ifFalse: [
					categories add: string.
					self _addToPureExportSet: string.
				].
			].
		].
	].
	stream := WriteStream on: String new.
	categories asSortedCollection do: [:each | 
		(self oopOf: each) printOn: stream.
		stream tab; nextPutAll: each; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
categoryOfMethod: aMethod

	| selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	^self _behavior: aMethod inClass categoryOfSelector: selector.

%

category: 'category'
method: JadeServer
class: aClass includesSelector: aSelector

	^aClass includesSelector: aSelector asSymbol.

%

category: 'category'
method: JadeServer
classesForUser: aUserProfile

	| stream |
	stream := WriteStream on: String new.
	aUserProfile symbolList do: [:eachDict |
		eachDict keysAndValuesDo: [:key :value |
			value isBehavior ifTrue: [
				stream nextPutAll: key; space; nextPutAll: value category asString; tab.
			].
		].
		stream lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
classListFor: aDictionary category: aString

	| visibleClasses allClasses stream queue |
	visibleClasses := aDictionary asArray select: [:each | 
		each isBehavior and: [aString isNil or: [
			| category |
			(category := each category) notNil and: [
			category = aString or: [
			category matchPattern: (Array with: aString with: $*)]]]]].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: aDictionary.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
classOrganizer

	^ClassOrganizer new

%

category: 'category'
method: JadeServer
clearBreakAtStepPoint: anInteger inMethod: aGsMethod

	aGsMethod clearBreakAtStepPoint: anInteger.

%

category: 'category'
method: JadeServer
commentFor: aClass

	| description |
	(Class canUnderstand: #'classComment') ifTrue: [
		^aClass classComment.
	].
	(description := aClass description) isNil ifTrue: [^nil].
	(description class name = #'GsClassDocumentation') ifTrue: [^description detailsAboutClass].
	^description printString.

%

category: 'category'
method: JadeServer
commit

	classOrganizers := Array new: 4.
	^System commitTransaction.

%

category: 'category'
method: JadeServer
compile: aString frame: anInteger process: aGsProcess
	"Compile method from within debugger"

	| oldMethod aBehavior selector category result |
	oldMethod := aGsProcess localMethodAt: anInteger.
	result := self recompile: oldMethod withSource: aString.
	(result isKindOf: Boolean) ifTrue: [^result].
	aBehavior := oldMethod inClass.
	selector := oldMethod selector.
	selector isNil ifTrue: [^result].
	category := self _behavior: aBehavior categoryOfSelector: selector.
	result := [[ aBehavior rwCompileMethod: aString category: category ]
							on: RwExecuteClassInitializeMethodsAfterLoadNotification
							do: [:ex | ex resume: false ]] 
								on: RwPerformingUnpackagedEditNotification
								do: [:ex | ex resume ].

					
	^result
%

category: 'category'
method: JadeServer
compiledMethodAt: aSymbol inClass: aClass

	^aClass compiledMethodAt: aSymbol.

%

category: 'category'
method: JadeServer
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"Returns aGsNMethod (if successful) -> anArrayOfErrorsOrWarnings"

	| result |
	"Method is in GsFoundation, but not in GsBase"
	result := (aBehavior class canUnderstand: #'compileMethod:category:using:environmentId:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList
				environmentId: 0.
			nil.
		] on: (self objectInBaseNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [(aBehavior class canUnderstand: #'compileMethod:category:using:') ifTrue: [
		[
			aBehavior		"returns self or signals a CompileError"
				compileMethod: methodString
				category: categorySymbol
				using: aSymbolList.
			nil.
		] on: (self objectInBaseNamed: #'UndefinedSymbolNotification') do: [:ex | 
			ex resume: false.
		].
	] ifFalse: [
		aBehavior		"returns nil or an Array of error descriptions"
			compileMethod: methodString
			dictionaries: aSymbolList
			category: categorySymbol.
	]].
	result notNil ifTrue: [
		^nil -> result.
	].
	(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:oldLitVars:intoMethodDict:intoCategories:intoPragmas:) ifTrue: [
		result := aBehavior 
			_primitiveCompileMethod: methodString
			symbolList: aSymbolList
			category: categorySymbol
			oldLitVars: nil
			intoMethodDict: GsMethodDictionary new 
			intoCategories: GsMethodDictionary new
			intoPragmas: nil.
	] ifFalse: [
		(aBehavior class canUnderstand: #_primitiveCompileMethod:symbolList:category:obsoleteClassNames:oldLitVars:) ifTrue: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				obsoleteClassNames: nil
				oldLitVars: nil.
		] ifFalse: [
			result := aBehavior 
				_primitiveCompileMethod: methodString
				symbolList: aSymbolList
				category: categorySymbol
				oldLitVars: nil
				intoMethodDict: GsMethodDictionary new 
				intoCategories: GsMethodDictionary new.
		].
	].
	(result isKindOf: Array) ifTrue: [
		"in 2.3.x: (Array with: compiledMethod with: errors with: warnings)"
		(result at: 2) notNil ifTrue: [^nil -> (result at: 2)].
		^(result at: 1) -> (result at: 3)
	].
	^result -> nil.

%

category: 'category'
method: JadeServer
compileMethod: methodString behavior: aBehavior user: aUserProfileOrNil inCategory: categoryString
	"answers a String:
		OOP of new method <TAB> selector of new method (or empty if compile failed)
		ERROR: [details] (repeat line for each error)
		[warnings] (if no errors)"

	| userProfile result gsMethod stream errDict errorList warnings |

	userProfile := aUserProfileOrNil isNil
		ifTrue: [System myUserProfile]
		ifFalse: [aUserProfileOrNil].
	result := self 		"key: GsNMethod value: ((Array withAll: errors) or aStringOfWarnings)"
		compileMethod: methodString 
		behavior: aBehavior 
		symbolList: userProfile symbolList 
		inCategory: categoryString asSymbol.
	(gsMethod := result key) isNil ifTrue: [
		errorList := result value.
		warnings := ''.
	] ifFalse: [
		errorList := #().
		warnings := result value.
	].
	stream := WriteStream on: String new.
	gsMethod notNil ifTrue: [
		stream 
			nextPutAll: gsMethod asOop printString;
			tab;
			nextPutAll: gsMethod selector;
			yourself.
	].
	errDict := GemStoneError at: System myUserProfile nativeLanguage.
	errorList do: [:each |
		stream lf; 
			nextPutAll: 'ERROR:'; tab;
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			yourself.
		(each size >= 3 and: [(each at: 3) notNil]) ifTrue: [
			stream nextPutAll: (each at: 3); tab.
		] ifFalse: [
			(each at: 1) > errDict size ifTrue: [
				stream nextPutAll: '(unknown error number)'; tab.
			] ifFalse: [
				stream nextPutAll: (errDict at: (each at: 1)) asString; tab.
			].
		].
	].
	warnings isNil ifTrue: [warnings := ''].
	stream lf; nextPutAll: warnings.
	^stream contents.

%

category: 'category'
method: JadeServer
contents
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!"

	^''.

%

category: 'category'
method: JadeServer
cr

	self nextPut: Character cr.

%

category: 'category'
method: JadeServer
currentUserMayEditMethod: aMethod

	^true
%

category: 'category'
method: JadeServer
debugString: aString fromContext: anObject environment: anInteger
	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!'].
	^(RowanDebuggerService new debugStringFrom: aString)
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList.
%

category: 'category'
method: JadeServer
debugTestNamed: testName of: testCaseClassName
	"Open a Debugger for the TestCase class named <testCaseClassName> in the test named <testName>"

	^(self objectNamed: testCaseClassName) debug: testName asSymbol
%

category: 'category'
method: JadeServer
defectiveTestsIn: aClass

	| testClass results stream |
	testClass := aClass thisClass.
	results := testClass suite run.
	stream := WriteStream on: String new.
	stream nextPutAll: results printString; lf.
	results defects asSet do: [:each | 
		| selector class |
		selector := each selector asSymbol.
		class := each class whichClassIncludesSelector: selector.
		stream nextPutAll: class name , ' debug: ' , each selector printString; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
definitionOfClass: aClass
	^aClass definition.
%

category: 'category'
method: JadeServer
definitionOfClass: aClass forUser: aUserProfile

			| stream |
			stream := WriteStream on: String new.
"1"		aClass superclass printOn: stream.
			stream 
"2"			lf; nextPutAll: (self subclassSelectorForClass: aClass);
"3"			lf; nextPutAll: aClass name;
"4"			lf; nextPutAll: (self dictionaryForClass: aClass forUser: aUserProfile);
				yourself.
"5"		stream lf. aClass instancesInvariant printOn: stream.
"6"		stream lf. aClass isModifiable printOn: stream.
"7"		stream lf. ((aClass class canUnderstand: #'instancesDbTransient') and: [aClass instancesDbTransient]) printOn: stream.
"8"		stream lf. ((aClass class canUnderstand: #'instancesNonPersistent') and: [aClass instancesNonPersistent]) printOn: stream.
			stream lf.
		aClass instVarNames do: [:each |
				stream 
"9.*.1"		nextPutAll: each; 
					space;
"9.*.2"		nextPutAll: (aClass constraintOfInstVar: each) name;
					tab.
			].
			stream lf.
			aClass class instVarNames do: [:each | 
"10.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass classVarNames asSortedCollection do: [:each | 
"11.*"		stream nextPutAll: each; tab.
			].
			stream lf.
			aClass sharedPools asSortedCollection do: [:each | 
"12.*"		stream nextPutAll: (self nameForSharedPool: each forUser: aUserProfile); tab.
			].
			^stream 
"13"		lf; nextPutAll: aClass userId;
"14"		lf; nextPutAll: (aClass timeStamp asStringUsingFormat: #(3 2 1 $- 1 1 $: true true false));
				lf; 
				contents.

%

category: 'category'
method: JadeServer
delay

	(Delay forMilliseconds: 10) wait.

%

category: 'category'
method: JadeServer
describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	writeStream := WriteStream on: String new.
	self _describeMethod: (self homeMethodFor: aMethod).
	^writeStream contents

%

category: 'category'
method: JadeServer
descriptionOfConfigOption: aString

	| dict key string |
	dict := self systemConfigAsDictionary.
	(string := dict at: aString ifAbsent: [nil]) notNil ifTrue: [^string].	string := aString asUppercase.
	dict keys do: [:each1 | 
		key := (each1 reject: [:each2 | each2 = $_]) asUppercase.
		key = string ifTrue: [^dict at: each1].
	].
	^''
%

category: 'category'
method: JadeServer
descriptionOfErrorNumber: anInteger

	| array stream |
	array := GemStoneError at: #'English'.
	anInteger <= 0 ifTrue: [^'Invalid number!'].
	array size < anInteger ifTrue: [^'Invalid number!'].
	stream := WriteStream on: String new.
	array := array at: anInteger.
	array isNil ifTrue: [^'No entry in GemStoneError for #' , anInteger printString , '!'].
	(array isKindOf: String) ifTrue: [array := Array with: array].
	array do: [:each | 
		(each isKindOf: Integer) ifTrue: [
			stream space; nextPut: $%.
			each printOn: stream.
		] ifFalse: [
			stream nextPutAll: each.
		].
	].
	^stream contents.

%

category: 'category'
method: JadeServer
dictionaryAndSymbolOf: aClass

	^self symbolList dictionaryAndSymbolOf: aClass.

%

category: 'category'
method: JadeServer
dictionaryAndSymbolOf: aClass forUser: aUserProfile

	^aUserProfile symbolList dictionaryAndSymbolOf: aClass.

%

category: 'category'
method: JadeServer
dictionaryForClass: aClass forUser: aUserProfile

	| anArray |
	anArray := self dictionaryAndSymbolOf: aClass forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	anArray := self dictionaryAndSymbolOf: (anArray at: 1) forUser: aUserProfile.
	anArray isNil ifTrue: [^''].
	^(anArray at: 2)

%

category: 'category'
method: JadeServer
dictionaryListFor: aUserProfile

	| symbolList list stream |
	symbolList := aUserProfile symbolList.
	list := symbolList namesReport subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | each subStrings].
	stream := WriteStream on: String new.
	list do: [:each | 
		(self oopOf: (symbolList at: (each at: 1) asNumber)) printOn: stream.
		stream tab; nextPutAll: (each at: 2); lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
does: newClass replace: oldClass

	^newClass name = oldClass name.

%

category: 'category'
method: JadeServer
environment
	"Ignored prior to 3.x"

	^0
%

category: 'category'
method: JadeServer
environment: anInteger
	"Ignored prior to 3.x"
%

category: 'category'
method: JadeServer
environmentForMethod: aGsNMethod

	^0
%

category: 'category'
method: JadeServer
environmentSuperClassFor: aBehavior

	^aBehavior superclass
%

category: 'category'
method: JadeServer
errorListFor: aCollection

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := WriteStream on: String new.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
executeString: aString fromContext: anObject environment: anInteger

	anInteger == 0 ifFalse: [self error: 'Only environment 0 is supported in this version!'].
	^aString
		evaluateInContext: anObject 
		symbolList: GsSession currentSession symbolList. 
%

category: 'category'
method: JadeServer
fileInClass: aString

	| list className index dictionaryName dictionary oldClass oldString |
	list := aString subStrings.
	className := list at: 3.
	className first = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!'].
	className last = $' ifFalse: [self error: 'Class name ' , className printString , ' expected to begin and end with a quote!'].
	className := className copyFrom: 2 to: className size - 1.
	index := list indexOf: 'inDictionary:'.
	dictionaryName := list at: index + 1.
	dictionary := self objectNamed: dictionaryName.
	oldClass := dictionary at: className ifAbsent: [nil].
	oldClass notNil ifTrue: [
		oldString := (oldClass _modifiableDefinitionInDictionary: dictionary named: dictionaryName) , '.'.
	].
	oldString = aString ifFalse: [aString evaluate].

%

category: 'category'
method: JadeServer
fileOutForClass: aClass

	^aClass thisClass fileOutClass.
 
%

category: 'category'
method: JadeServer
gemLogPath

	^''

%

category: 'category'
method: JadeServer
getMethodsNamesOf: classNamed
	"NO SENDERS"
	"Answer a collection with selectors of GemStone/s class name <className>"

	^(self objectNamed: classNamed) selectors asSortedCollection

%

category: 'category'
method: JadeServer
getSubclassesNamesOf: classNamed 
	"NO SENDERS"
	"Answer a collection with all subclasses of GemStone/S class named <classNamed>"

	^((self objectNamed: classNamed) allSubclasses collect: [:each | each name]) asSortedCollection
%

category: 'category'
method: JadeServer
getTestCaseClassesNamesOf: packageName
	"Answer a collection with all test classes of the GemStone/S package named <packageNamed>"

	| classes comma stream |
	packageName isEmpty ifTrue: [^''].
	classes := (self objectInBaseNamed: #'TestCase') allSubclasses.
	packageName = '<All>' ifFalse: [
		| packageInfo packageOrganizer |
		packageOrganizer := (self objectInBaseNamed: #'PackageOrganizer') default.
		packageInfo := packageOrganizer packageNamed: packageName ifAbsent: [^''].
		packageInfo classes isEmpty ifTrue: [^''].
		classes := packageInfo classes select: [:each | classes includes: each].
	].
	stream := WriteStream on: String new.
	comma := ''.
	classes do: [:each | stream nextPutAll: comma; nextPutAll: each name. comma := ','].
	^stream contents
%

category: 'category'
method: JadeServer
getTestMethodsNamesOf: classNamed 
	"Answer a collection with all test selectors of the GemStone/S class named <classNamed>"

	| class comma stream testSelectors gsClass |
	stream := WriteStream on: String new.
	gsClass := self objectNamed: classNamed.
	gsClass isNil ifTrue: [^''].
	(class := self objectNamed: #'TestCase') isNil ifTrue: [^''].
	(gsClass isSubclassOf: class) ifFalse: [^''].
	testSelectors :=  gsClass testSelectors asSortedCollection.
	testSelectors isEmpty ifTrue: [^''].
	comma := ''.
	 testSelectors do: [:each | stream nextPutAll: comma; nextPutAll: each. comma := ','].
	^stream contents
%

category: 'category'
method: JadeServer
globalsFor: aSymbolDictionary

	| stream |
	stream := WriteStream on: String new.
	aSymbolDictionary keysAndValuesDo: [:eachKey :eachValue | 
		eachValue isBehavior ifFalse: [
			| data |
			data := (self _oopAndStringFor: eachValue) value.
			data size > 200 ifTrue: [data := data copyFrom: 1 to: 200].
			data := String withAll: (data asArray collect: [:each | (each >= Character space and: [each <= $~]) ifTrue: [each] ifFalse: [$?]]).
			stream
	"1"		nextPutAll: (self oopOf: eachValue) printString; tab;
	"2"		nextPutAll: eachKey; tab;
	"3"		nextPutAll: eachValue class name; tab;
	"4"		nextPutAll: data; tab;
				lf;
				yourself.
		].
	].
	^stream contents.

%

category: 'category'
method: JadeServer
groupListFor: aUserProfile

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := aUserProfile groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.

%

category: 'jadeite'
method: JadeServer
gsInteractionInformFailureHandler
  self interactionHandlerActive
    ifFalse: [ 
      ^ GsInteractionHandler new
        defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
        confirmBlock: [ :interaction | interaction ok ];
        informBlock: [ :interaction |  ];
        inspectBlock: [ :interaction |  ];
        yourself ].
  ^ GsInteractionHandler new
    confirmBlock: [ :interaction | 
          | exception answer |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'confirmMessageBox:'
            args: (Array with: interaction prompt).
          answer := exception defaultAction.	"expect printString of answer back. Jadeite has limited ability to convert client objects to oops"
          answer evaluate ];
    informBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'informMessageBox:'
            args: (Array with: interaction message).
          exception defaultAction.
          nil ];
    inspectBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'inspectServerObject:'
            args: (Array with: interaction theObject asOop).
          exception defaultAction.
          interaction theObject ]
%

category: 'category'
method: JadeServer
gsPackagePolicy

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	^class current.

%

category: 'category'
method: JadeServer
gsPackagePolicyClass

	^self objectInBaseNamed: #'GsPackagePolicy'.

%

category: 'category'
method: JadeServer
historyOf: aClass

	| history |
	(history := aClass classHistory) isNil ifTrue: [
		history := Array with: aClass.
	].
	^history.

%

category: 'category'
method: JadeServer
homeMethodFor: aGsMethod

	^aGsMethod
%

category: 'category'
method: JadeServer
implementorsOf: anObject

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer implementorsOf: symbol).

%

category: 'category'
method: JadeServer
implementorsOf: aGsMethod startingAt: aClass

	| selector myClass list |
	selector := aGsMethod selector.
	myClass := aClass.
	list := OrderedCollection new.
	[
		(myClass includesSelector: selector) ifTrue: [list add: myClass].
		(myClass := myClass superclass) notNil.
	] whileTrue: [].
	^self stringForClassList: list.

%

category: 'category'
method: JadeServer
initialize
	"#installTranscript is run from the Jadeite
	client post login method only. This avoids
	setting up ClientForwarder sends in a topaz
	session"

	classOrganizers := Array new: 4.
	self 
		registerOBNotifications;
		yourself.
%

category: 'category'
method: JadeServer
inspect: anObject

	| stream string |
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	self inspectNamedInstanceVariablesOf: anObject on: stream.
	anObject class format > 0 ifTrue: [
		1 to: (anObject _basicSize) do: [:i | 
			i printOn: stream.
			stream tab.
			self print: (self oopOf: (anObject _at: i)) on: stream.
			stream lf.
		].
	].
	(string := anObject printString) size > 5000 ifTrue: [string := (string copyFrom: 1 to: 5000) , '...'].
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	^stream 
		nextPutAll: string; 
		contents.
%

category: 'category'
method: JadeServer
inspectDictionary: aDictionary on: aStream

	| keys keyDict |
	keys := self keysForDictionary: aDictionary.
	keyDict := Dictionary new.
	keys do: [:each | 
		| key |
		key := each printString , '~' , (self oopOf: each) printString.
		key := key collect: [:char | char asciiValue < 32 ifTrue: [$?] ifFalse: [char]].
		keyDict
			at: key
			put: each.
	].
	keys size printOn: aStream.
	aStream lf.
	keyDict keys asSortedCollection do: [:each | 
		| index keyString key value valueString |
		index := each findLast: [:char | char = $~].
		keyString := each copyFrom: 1 to: index - 1.
		keyString charSize = 1 ifFalse:[
			keyString := '<<unprintable key. charSize > 1>>']. 
		key := keyDict at: each.
		value := aDictionary at: key. 
		valueString := (self printStringOf: value to: 10).
		valueString charSize = 1 ifFalse:[
			valueString := '<<unprintable value. charSize > 1>>']. 
		aStream nextPutAll: keyString , '->' , valueString; tab.
		self print: (self oopOf: value) on: aStream.
		aStream lf.
	].
	^aStream 
		lf; 
		contents.
%

category: 'category'
method: JadeServer
inspectNamedInstanceVariablesOf: anObject on: aStream

	| list size |
	list := anObject class allInstVarNames.
	size := list size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 200).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: aStream.
		aStream lf.
	].

%

category: 'category'
method: JadeServer
installTranscript

	| transcript |
	transcript := self objectInBaseNamed: #'Transcript'.

	"If no Transcript object, then install me!"
	(transcript == nil or: [transcript class name == self class name]) ifTrue: [
		UserGlobals at: #'Transcript' put: self.
		System commitTransaction.
		^self.
	].
	
	"Transcript object from Seaside"
	transcript class name = 'TranscriptProxy class' ifTrue: [
		| clientForwarder |
		clientForwarder := ClientForwarder new.
		clientForwarder	clientObject: 2.
		transcript registerTranscriptClientForwarder: clientForwarder.
		^self.
	].

%

category: 'jadeite'
method: JadeServer
interactionHandlerActive
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
%

category: 'category'
method: JadeServer
is32Bit

	^false.

%

category: 'category'
method: JadeServer
isClientForwarder: anObject

	^anObject _class name == #'ClientForwarder'.

%

category: 'category'
method: JadeServer
isPackagePolicyEnabled

	^self gsPackagePolicy notNil
%

category: 'category'
method: JadeServer
isResumableCategory: category number: number context: context

	| exceptionA receiver |
	category == GemStoneError ifTrue: [
		^number // 1000 = 2 or: [number // 1000 = 6].
	].
	(exceptionA := Globals at: #ExceptionA ifAbsent: [nil]) isNil ifTrue: [
		^true.
	].
	receiver := (context _frameContentsAt: 1) at: 8.
	(receiver isKindOf: exceptionA) ifTrue: [
		^receiver isResumable.
	].
	^true.

%

category: 'category'
method: JadeServer
keysForDictionary: aDictionary 

	^aDictionary keys.

%

category: 'category'
method: JadeServer
makeListener

	^nil.

%

category: 'category'
method: JadeServer
mcAddHttpRepository: aString

	| list repositoryClass repository group |
	list := aString subStrings: (Character codePoint: 255).
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!'].
	repository := repositoryClass
		location: (list at: 1)
		user: (list at: 2)
		password: (list at: 3).
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!'].
	group addRepository: repository.
	^repository
%

category: 'category'
method: JadeServer
mcAddPackage: aString

	self mcWorkingCopyClass forPackage: (self mcPackageClass named: aString).

%

category: 'category'
method: JadeServer
mcAddRepository: aRepository toPackage: aMCWorkingCopy

	aMCWorkingCopy repositoryGroup addRepository: aRepository.

%

category: 'category'
method: JadeServer
mcAllFileNamesIn: anMCRepository

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allFileNames do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcAllVersionInfoNamesIn: anMCRepository

	| stream |
	stream := WriteStream on: String new.
	anMCRepository allVersionInfos do: [:each | 
		stream nextPutAll: each name; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcAllVersionNamesInDict: anMCRepository

	| stream list |
	stream := WriteStream on: String new.
	list := anMCRepository dictionary values.
	list := list asSortedCollection: [:a :b | 
		a package name < b package name or: [
		a package name = b package name and: [
		a info date > b info date or: [
		a info date = b info date and: [
		a info time > b info time
	]]]]].
	stream := WriteStream on: String new.
	list do: [:each | 
		stream nextPutAll: each info name; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcClassesInCategory: aString package: aMCWorkingCopy

	| visibleClasses allClasses stream queue |
	visibleClasses := aString isNil ifTrue: [
		aMCWorkingCopy packageInfo classes.
	] ifFalse: [
		aMCWorkingCopy packageInfo classes select: [:each | 
			each _classCategory notNil and: [
			each _classCategory = aString or: [
			aString notNil and: [each _classCategory matchPattern: (Array with: aString with: $*)]]]]
	].
	allClasses := visibleClasses asIdentitySet.
	queue := visibleClasses asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| parent |
		parent := queue removeFirst superclass.
		(parent notNil and: [(allClasses includes: parent) not]) ifTrue: [
			queue add: parent.
			allClasses add: parent.
		].
	].
	stream := WriteStream on: String new.
	allClasses do: [:each |
		self
			_addClass: each 
			toStream: stream 
			isVisible: (visibleClasses includes: each)
			fromDictionary: nil.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcCreationTemplateFor: anMCRepository

	^anMCRepository asCreationTemplate.

%

category: 'category'
method: JadeServer
mcHttpRepository

	^self objectInBaseNamed: #'MCHttpRepository'.

%

category: 'category'
method: JadeServer
mcHttpRepository: aRepository user: userString password: passwordString

	aRepository
		user: userString;
		password: passwordString;
		yourself.

%

category: 'category'
method: JadeServer
mcHttpRepositoryClass

	^self objectInBaseNamed: #'MCHttpRepository'.

%

category: 'category'
method: JadeServer
mcInitials: aString
	"Do initial setup and return useful information"

	| mcPlatformSupport packagePolicyEnabledFlag string x |
	string := 'Jade-' , GsSession currentSession serialNumber printString , '-' , System myUserProfile userId.
	[
		self mcInitialsA: string.
	] whileFalse: [	"Keep shortening it till it fits!"
		string := string copyFrom: 1 to: string size - 1.
	].
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	mcPlatformSupport notNil ifTrue: [mcPlatformSupport setAuthorInitials: aString].
	packagePolicyEnabledFlag := (x := self objectInBaseNamed: #'GsPackagePolicy') isNil ifTrue: ['0'] ifFalse: [x current enabled ifTrue: ['1'] ifFalse: ['0']].
	^System session printString , Character space asString , 
		(GsSession serialOfSession: System session) printString , Character space asString , 
		packagePolicyEnabledFlag

%

category: 'category'
method: JadeServer
mcInitialsA: aString
	"Subclasses provide error handling, typically means string is too long"

	System _cacheName: aString.

%

category: 'category'
method: JadeServer
mcLoadedVersionNames

	| mcWorkingCopyClass stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	mcWorkingCopyClass allManagers do: [:each | 
		| packageOrVersion |
		packageOrVersion := each ancestors
			detect: [:ignored | true]
			ifNone: [each package].
		packageOrVersion := packageOrVersion notNil
			ifTrue: [packageOrVersion name]
			ifFalse: [''].
		stream
			nextPutAll: packageOrVersion; tab;
			nextPut: (each modified ifTrue: [$Y] ifFalse: [$N]); tab;
			nextPutAll: each package name;
			lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcNewDirectoryRepository: aString

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'FileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository
%

category: 'category'
method: JadeServer
mcNewFileTreeRepository: aString

	| mcRepositoryClass fileDirectoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCFileTreeRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository
%

category: 'category'
method: JadeServer
mcNewGitHubRepository: aString

	| mcRepositoryClass repository |
	(mcRepositoryClass := self objectInBaseNamed: #'MCGitHubRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcRepositoryClass location: aString.
	self mcRepositoryGroup addRepository: repository.
	^mcRepositoryClass
%

category: 'category'
method: JadeServer
mcNewServerDirectoryRepository: aString

	| mcDirectoryRepositoryClass fileDirectoryClass repository |
	(mcDirectoryRepositoryClass := self objectInBaseNamed: #'MCServerDirectoryRepository') isNil ifTrue: [self error: 'Monticello not available!'].
	(fileDirectoryClass := self objectInBaseNamed: #'ServerFileDirectory') isNil ifTrue: [self error: 'Monticello not available!'].
	repository := mcDirectoryRepositoryClass new directory: (fileDirectoryClass on: aString).
	self mcRepositoryGroup addRepository: repository.
	^repository
%

category: 'category'
method: JadeServer
mcPackageClass

	^self objectInBaseNamed: #'MCPackage'.

%

category: 'category'
method: JadeServer
mcPatchFrom: aString1 to: aString2 inFileBasedRepository: aFileRepository

	| index name leftSnapshot rightSnapshot patch |
	index := aString2 findLast: [:each | each = $-].
	name := aString2 copyFrom: 1 to: index - 1.
	(name includes: $.) ifTrue: [name := (name subStrings: $.) first].
	leftSnapshot := aString1 isNil ifTrue: [
		(self mcWorkingCopyClass allManagers detect: [:each | each package name = name]) package snapshot.
	] ifFalse: [
		(aFileRepository versionFromFileNamed: aString1) snapshot.
	].
	rightSnapshot := (aFileRepository versionFromFileNamed: aString2) snapshot.
	patch := rightSnapshot patchRelativeToBase: leftSnapshot.
	^self 
		_mcDescriptionOfPatch: patch
		baseName: aString1
		alternateName: aString2.

%

category: 'category'
method: JadeServer
mcputDefinition: aDefinition on: aStream

	| mcOrganizationDefinitionClass mcClassDefinitionClass mcMethodDefinitionClass |
	(mcOrganizationDefinitionClass := self objectInBaseNamed: 'MCOrganizationDefinition') isNil ifTrue: [^nil].
	(mcClassDefinitionClass := self objectInBaseNamed: 'MCClassDefinitionClass') isNil ifTrue: [^nil].
	(mcMethodDefinitionClass := self objectInBaseNamed: 'MCMethodDefinition') isNil ifTrue: [^nil].

	self _addToPureExportSet: aDefinition.
		aStream nextPutAll: (self oopOf: aDefinition) printString; tab;
			nextPutAll: aDefinition class name; tab.
			
		aDefinition class == mcOrganizationDefinitionClass ifTrue: [
			aDefinition categories do: [:eachCategory | 
				aStream nextPutAll: eachCategory; space]
		] ifFalse: [	aDefinition class == mcClassDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition superclassName; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition type; tab;
				yourself.
		] ifFalse: [aDefinition class == mcMethodDefinitionClass ifTrue: [
			aStream
				nextPutAll: aDefinition classIsMeta printString; tab;
				nextPutAll: aDefinition category; tab;
				nextPutAll: aDefinition selector; tab;
				nextPutAll: aDefinition className; tab;
				nextPutAll: aDefinition timeStamp printString; tab]]].
	
	^aStream.
	
%

category: 'category'
method: JadeServer
mcRemoveRepository: aRepository

	| repositoryClass group |
	(repositoryClass := self mcHttpRepositoryClass) isNil ifTrue: [self error: 'MCHttpRepository not found!'].
	(group := self mcRepositoryGroup) isNil ifTrue: [self error: 'MCRepositoryGroup not found!'].
	group removeRepository: aRepository.

%

category: 'category'
method: JadeServer
mcRemoveRepository: aRepository toPackage: aMCWorkingCopy

	aMCWorkingCopy repositoryGroup removeRepository: aRepository.

%

category: 'category'
method: JadeServer
mcRepositoryFrom: aRepository

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aRepository) printOn: stream.
	stream 
		tab;
		nextPutAll: aRepository description;
		tab;
		nextPutAll: aRepository class name;
		tab.
	^stream contents.

%

category: 'category'
method: JadeServer
mcRepositoryGroup

	| groupClass |
	(groupClass := self objectInBaseNamed: 'MCRepositoryGroup') isNil ifTrue: [^nil].
	^groupClass default.

%

category: 'category'
method: JadeServer
mcRepositoryList

	| group stream |
	(group := self mcRepositoryGroup) isNil ifTrue: [^nil].
	stream := WriteStream on: String new.
	group repositories do: [:each | 
		stream nextPutAll: (self mcRepositoryFrom: each).
		stream lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
mcTopazFrom: aString inDictionaryRepository: aDictionaryRepository

	| snapshot stream |
	snapshot := (aDictionaryRepository versionFromVersionNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '! ' , aString , ' in ' , aDictionaryRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.


%

category: 'category'
method: JadeServer
mcTopazFrom: aString inFileRepository: aFileRepository

	| snapshot stream |
	snapshot := (aFileRepository versionFromFileNamed: aString) snapshot.
	stream := (WriteStream on: String new)
		nextPutAll: '! ' , aString , ' in ' , aFileRepository printString; lf;
		yourself.
	self
		_mcTopazFrom: snapshot
		on: stream.
	^stream contents.


%

category: 'category'
method: JadeServer
mcUniqueVersionNameFor: anMCWorkingCopy

	^anMCWorkingCopy uniqueVersionName.

%

category: 'category'
method: JadeServer
mcUserAndPasswordInHTTP: anMCHttpRepository

	^anMCHttpRepository user , Character tab asString , anMCHttpRepository password.

%

category: 'category'
method: JadeServer
mcVersionInfoFrom: aVersionInfo

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aVersionInfo) printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo name; 
		lf; nextPutAll: aVersionInfo date yyyymmdd;
		lf.
	aVersionInfo time printOn: stream.
	stream 
		lf; nextPutAll: aVersionInfo author; 
		lf; nextPutAll: aVersionInfo id asString;
		lf.
	aVersionInfo ancestors do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	aVersionInfo stepChildren do: [:each | 
		stream nextPutAll: each name; tab.
	].
	stream lf.
	stream nextPutAll: aVersionInfo message.
	^stream contents.

%

category: 'category'
method: JadeServer
mcVersionInfoFromDictionaryPackageNamed: aString in: anMCDictionaryRepository

	| versionInfo |
	(versionInfo := anMCDictionaryRepository versionInfoFromVersionNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.

%

category: 'category'
method: JadeServer
mcVersionInfoFromFileNamed: aString in: anMCFileBasedRepository

	| versionInfo |
	(versionInfo := anMCFileBasedRepository versionInfoFromFileNamed: aString) isNil ifTrue: [^''].
	^self mcVersionInfoFrom: versionInfo.

%

category: 'category'
method: JadeServer
mcVersionLoad: aString fromDictionary: anMCDictionaryRepository autoMigrate: aBoolean

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCDictionaryRepository versionFromVersionNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCDictionaryRepository.
	mcPlatformSupport autoMigrate: autoMigrate.

%

category: 'category'
method: JadeServer
mcVersionLoad: aString fromFile: anMCFileBasedRepository autoMigrate: aBoolean

	| version package workingCopy mcPlatformSupport autoMigrate |
	mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
	autoMigrate := mcPlatformSupport autoMigrate.
	mcPlatformSupport autoMigrate: aBoolean.
	version := anMCFileBasedRepository loadVersionFromFileNamed: aString.
	version load.
	package := version package.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	workingCopy repositoryGroup addRepository: anMCFileBasedRepository.
	mcPlatformSupport autoMigrate: autoMigrate.

%

category: 'category'
method: JadeServer
mcVersionNameAndMessageFrom: aMCWorkingCopy

	(aMCWorkingCopy needsSaving or: [aMCWorkingCopy ancestors isEmpty]) ifTrue: [
		^'<new>	<new>'.
	].
	^aMCWorkingCopy currentVersionInfo name , Character tab asString , aMCWorkingCopy currentVersionInfo message.

%

category: 'category'
method: JadeServer
mcwcbWorkingCopies

	| mcWorkingCopyClass list stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	list := mcWorkingCopyClass allManagers.
	list := list asSortedCollection: [:a :b | a package name <= b package name].
	stream := WriteStream on: String new.
	list do: [:each |
		self saveWorkingCopy: each to: stream.
		stream lf].
	^stream contents
%

category: 'category'
method: JadeServer
mcWorkingCopyClass

	^self objectInBaseNamed: #'MCWorkingCopy'.

%

category: 'category'
method: JadeServer
mcWorkingCopyNamed: aString

	| mcWorkingCopyClass workingCopy stream |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^nil].
	workingCopy := mcWorkingCopyClass allManagers 
		detect: [:each | each package name = aString]
		ifNone: [^nil].
	stream := WriteStream on: String new.
	self 
		saveWorkingCopy: workingCopy 
		to: stream.
	^stream contents.

%

category: 'category'
method: JadeServer
methodsContaining: aString

	^self streamOfMethods: (self classOrganizer substringSearch: aString) first.

%

category: 'category'
method: JadeServer
methodsFor: childClass upTo: parentClass filter: aString isVariables: aBoolean 

	| filterList answerList aClass stream selectors |
	filterList := (aString subStrings: Character tab) reject: [:each | each isEmpty].
	aBoolean ifTrue: [filterList := (filterList collect: [:each | each asSymbol]) asIdentitySet].
	aClass := childClass.
	answerList := IdentitySet new.
	selectors := IdentitySet new.
	[
		| methods |
		methods := self 
			_methodsFor: aClass
			filter: filterList
			isVariables: aBoolean.
		methods do: [:each | 
			(selectors includes: each selector) ifFalse: [
				answerList add: each.
				selectors add: each selector.
			].
		].
		aClass = parentClass.
	] whileFalse: [
		aClass := aClass superclass.
	].
	stream := WriteStream on: String new.
	answerList do: [:each | self _addMethod: each toStream: stream].
	^stream contents
%

category: 'category'
method: JadeServer
methodSignatureForSelector: aSymbol

	^aSymbol.

%

category: 'category'
method: JadeServer
millisecondsElapsedTime: aBlock

	^Time millisecondsElapsedTime: aBlock.

%

category: 'category'
method: JadeServer
moveClassesInDictionary: sourceDictionary category: aString to: destinationDictionary

	sourceDictionary copy keysAndValuesDo: [:eachKey :eachValue | 
		(eachValue isBehavior and: [eachValue category = aString]) ifTrue: [
			sourceDictionary removeKey: eachKey.
			destinationDictionary
				at: eachKey
				put: eachValue.
		].
	].

%

category: 'category'
method: JadeServer
moveDictionary: source toBefore: target forUser: aUserProfile

	| list |
	list := aUserProfile symbolList.
	list remove: source.
	target notNil ifTrue: [
		list
			add: source 
			before: target.
	] ifFalse: [
		list addLast: source.
	].


%

category: 'category'
method: JadeServer
moveMethod: aGsMethod toCategory: aString
	aGsMethod inClass
		rwMoveMethod: aGsMethod selector
		toCategory: aString.

%

category: 'category'
method: JadeServer
mySessionInfo

	| dict stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self gemLogPath; cr.
	dict := System gemVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System gemConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
%

category: 'category'
method: JadeServer
nameForSharedPool: anObject forUser: aUserProfile

	| anArray dict sharedPoolClass |
	anArray := self dictionaryAndSymbolOf: anObject forUser: aUserProfile.
	anArray notNil ifTrue: [^anArray at: 2].
	(dict := aUserProfile objectNamed: anObject name) isNil ifTrue: [^'???'].
	(sharedPoolClass := self objectNamed: 'SharedPool') isNil ifTrue: [^'???'].
	((dict isKindOf: Class) and: [dict isSubclassOf: sharedPoolClass]) ifTrue: [^anObject name , ' _classVars'].
	^'???'.

%

category: 'category'
method: JadeServer
nameOfFirstDictionaryReferencing: aGlobal

	| list |
	list := self symbolList dictionaryAndSymbolOf: aGlobal.
	list isNil ifTrue: [^''].
	^list first name
%

category: 'category'
method: JadeServer
newUser: aString

	| userProfile stream |
	userProfile := UserProfile 
		newWithUserId: aString
		password: 'swordfish'
		privileges: #()
		inGroups: #().
	stream := WriteStream on: String new.
	self
		addUser: userProfile 
		toStream: stream.
	^stream contents.

%

category: 'category'
method: JadeServer
nextLine

	^readStream upTo: Character lf.

%

category: 'category'
method: JadeServer
nextLineAsList

	^(self nextLine subStrings: Character tab) reject: [:each | each isEmpty].

%

category: 'category'
method: JadeServer
nextPut: aCharacter

	self nextPutAll: aCharacter asString.

%

category: 'category'
method: JadeServer
nextPutAll: anObject

	| string args |
	string := self asString: anObject.
	args := Array
		with: self
		with: 1
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.

%

category: 'category'
method: JadeServer
obConfirmationRequest: anOBConfirmationRequest

	^String new 
		addAll: anOBConfirmationRequest cancelChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest okChoice;
		add: Character lf;
		addAll: anOBConfirmationRequest prompt;
		yourself.

%

category: 'category'
method: JadeServer
obInformRequest: anOBInformRequest

	^anOBInformRequest message
%

category: 'category'
method: JadeServer
objectForOop: anInteger

	self subclassResponsibility.
%

category: 'category'
method: JadeServer
objectInBaseNamed: aString

	^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].

%

category: 'category'
method: JadeServer
objectNamed: aString

	^System myUserProfile objectNamed: aString asSymbol.

%

category: 'category'
method: JadeServer
objectSecurityPolicyFor: anObject

	^anObject segment.

%

category: 'category'
method: JadeServer
obTextRequest: anOBTextRequest

	| prompt template |
	prompt := anOBTextRequest prompt.
	template := anOBTextRequest template.
	^String new 
		addAll: prompt size printString;
		add: Character lf;
		addAll: prompt;
		addAll: template;
		yourself.

%

category: 'category'
method: JadeServer
oopOf: anObject

	^anObject asOop.

%

category: 'category'
method: JadeServer
packagePolicy: aPackagePolicy includesSelector: aSymbol forClass: aClass

	^aPackagePolicy notNil and: [aPackagePolicy includesSelector: aSymbol for: aClass].

%

category: 'category'
method: JadeServer
postSaveClass: aGsClass activities: aString 

	| gsClass copyMethods migrateInstances recompileSubclasses removeFromClassHistory symbolList list index key oldClass newClass oldNewList stream |
	gsClass := (self historyOf: aGsClass) last.
	list := aString subStrings: Character tab.
	list := list collect: [:each | each = 'true'].
	symbolList := self symbolList.
	copyMethods := list at: 1.
	recompileSubclasses := list at: 2.
	migrateInstances := list at: 3.
	removeFromClassHistory := list at: 4.
	oldNewList := OrderedCollection new.
	stream := WriteStream on: String new.
	oldClass := (self historyOf: gsClass) asArray reverse at: 2.
	oldNewList add: oldClass -> gsClass.
	recompileSubclasses ifTrue: [
		(self classOrganizer allSubclassesOf: oldClass) do: [:each | 
			gsClass := GsSession currentSession execute: each definition.
			oldNewList add: each -> gsClass.
		].
	].
	copyMethods ifTrue: [
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			index := symbolList findFirst: [:eachDict | eachDict includes: newClass].
			index = 0 ifTrue: [self error: 'Where did the class go?'].
			key := (symbolList at: index) keyAtValue: newClass.
			list := newClass copyMethodsFrom: oldClass dictionaries: symbolList.
			list do: [:eachMethod | 
				stream
					nextPutAll: 'method'; tab;
					nextPutAll: index printString; tab;
					nextPutAll: key; tab;
					nextPutAll: eachMethod selector; lf;
					yourself]]].
	migrateInstances ifTrue: [
		System commitTransaction ifFalse: [self error: 'commit failed!'].
		oldNewList do: [:eachAssoc | 
			oldClass := eachAssoc key.
			newClass := eachAssoc value.
			list := oldClass migrateInstancesTo: newClass.
			list do: [:each | 
				each notEmpty ifTrue: [
					stream
						nextPutAll: 'migrate'; tab;
						nextPutAll: newClass name; tab;
						nextPutAll: each size printString; lf;
						yourself.
				].
			].
		].
	].
	removeFromClassHistory ifTrue: [
		oldNewList do: [:eachAssoc | 
			newClass := eachAssoc value.
			((self historyOf: newClass) asArray copyFrom: 1 to: (self historyOf: newClass) size - 1) do: [:each | 
				(self historyOf: newClass) removeVersion: each.
			].
		].
	].
	^stream contents.

%

category: 'category'
method: JadeServer
print: anObject on: aStream
	"convert multi-byte strings to single-byte"

	| string |
	string := self printStringOf: anObject.
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	aStream nextPutAll: string.

%

category: 'category'
method: JadeServer
printStringOf: anObject

	^anObject printString.
%

category: 'category'
method: JadeServer
printStringOf: anObject to: anInteger

	| string |
	(string := self printStringOf: anObject) size > anInteger ifTrue: [string := (string copyFrom: 1 to: anInteger) , '...'].
	string := String withAll: (string collect: [:each | (32 <= each asciiValue and: [each asciiValue <= 255]) ifTrue: [each] ifFalse: [$?]]).
	^string.
%

category: 'category'
method: JadeServer
privilegeListFor: aUserProfile

	| allPrivileges myPrivileges stream |
	allPrivileges := (aUserProfile class instVarAt: 6) at: #'PrivilegeNames'.
	myPrivileges := aUserProfile privileges.
	stream := WriteStream on: String new.
	allPrivileges do: [:each | 
		stream nextPutAll: each; tab.
		(myPrivileges includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
processes

	| scheduler stream |
	scheduler := ProcessorScheduler scheduler.
	stream := (WriteStream on: String new)
		nextPutAll: 'highestPriority'; 			space; nextPutAll: scheduler highestPriority 			printString; tab;
		nextPutAll: 'highIOPriority'; 			space; nextPutAll: scheduler highIOPriority 			printString; tab;
		nextPutAll: 'lowestPriority'; 			space; nextPutAll: scheduler lowestPriority 				printString; tab;
		nextPutAll: 'lowIOPriority'; 				space; nextPutAll: scheduler lowIOPriority 				printString; tab;
		nextPutAll: 'systemBackgroundPriority'; 	space; nextPutAll: scheduler systemBackgroundPriority 	printString; tab;
		nextPutAll: 'timingPriority'; 			space; nextPutAll: scheduler timingPriority 			printString; tab;
		nextPutAll: 'userBackgroundPriority'; 		space; nextPutAll: scheduler userBackgroundPriority 		printString; tab;
		nextPutAll: 'userInterruptPriority'; 		space; nextPutAll: scheduler userInterruptPriority 		printString; tab;
		nextPutAll: 'userSchedulingPriority'; 		space; nextPutAll: scheduler userSchedulingPriority 		printString; tab;
		yourself.
	scheduler readyProcesses 		do: [:each | self addProcess: each to: stream withStatus: 'ready'		scheduler: scheduler].
	scheduler suspendedProcesses 	do: [:each | self addProcess: each to: stream withStatus: 'suspended'	scheduler: scheduler].
	self waitingProcesses			do: [:each | self addProcess: each to: stream withStatus: 'waiting'	scheduler: scheduler].
	^stream contents.


%

category: 'category'
method: JadeServer
referencesToObject: anObject

	^self streamOfMethods: (self classOrganizer referencesToObject: anObject).

%

category: 'category'
method: JadeServer
registerOBNotifications

	| platform clientForwarder |
	(platform := self objectInBaseNamed: #'OBGemStonePlatform') isNil ifTrue: [^self].
	clientForwarder := ClientForwarder new.
	clientForwarder	clientObject: 1.
	self
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.

%

category: 'category'
method: JadeServer
registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

	platform 
		registerBrowseClientForwarder: clientForwarder;
		registerChoiceClientForwarder: clientForwarder;
		registerCloseClientForwarder: clientForwarder;
		registerConfirmationClientForwarder: clientForwarder;
		registerInformClientForwarder: clientForwarder;
		registerMultiLineTextClientForwarder: clientForwarder;
		registerTextClientForwarder: clientForwarder;
		yourself.

%

category: 'category'
method: JadeServer
removeCategory: aString fromBehavior: aBehavior

	aBehavior rwRemoveCategory: aString.

%

category: 'category'
method: JadeServer
removeClass: aClass from: aDictionary

	| key |
	key := aDictionary
		keyAtValue: aClass
		ifAbsent: [^false].
	aDictionary removeKey: key.
	^true.

%

category: 'category'
method: JadeServer
removeDictionary: aDictionary fromUser: aUserProfile

	| symbolList index |
	symbolList := aUserProfile symbolList.
	index := symbolList indexOf: aDictionary.
	aUserProfile removeDictionaryAt: index.

%

category: 'category'
method: JadeServer
removeGroup: aString fromUser: aUserProfile

	aUserProfile removeGroup: aString.

%

category: 'category'
method: JadeServer
removeKey: aString fromSymbolDictionary: aSymbolDictionary

	aSymbolDictionary removeKey: aString asSymbol.


%

category: 'category'
method: JadeServer
removeMethod: aGsMethod

	aGsMethod inClass removeSelector: aGsMethod selector.

%

category: 'category'
method: JadeServer
removePriorVersionsOf: aClass

	[
		1 < (self historyOf: aClass) size.
	] whileTrue: [
		(self historyOf: aClass) removeVersion: (self historyOf: aClass) first.
	].

%

category: 'category'
method: JadeServer
removePrivilege: aString fromUser: aUserProfile

	aUserProfile deletePrivilege: aString.

%

category: 'category'
method: JadeServer
renameCategory: oldString to: newString inBehavior: aBehavior

	aBehavior
		renameCategory: oldString asSymbol
		to: newString.

%

category: 'category'
method: JadeServer
reset
	"WriteStream protocol"
%

category: 'category'
method: JadeServer
runAsTest: aGsMethod

	aGsMethod inClass debug: aGsMethod selector.
	^true.

%

category: 'category'
method: JadeServer
runTestNamed: testName in: gsClass
	"The receiver run the test named <testName> of GemStone/S class named <gsClass>"
	| testResult |

	testResult := (self objectNamed: gsClass) run: testName asSymbol.

	^testResult errorCount printString, ',' ,testResult failureCount printString, ',', testResult passedCount printString
%

category: 'category'
method: JadeServer
runTestsNamed: testCollection in: gsClass
	"NO SENDERS"
	"The receiver run the all test of the collection <testCollection> of GemStone/S class named <gsClass>"
	| testResult |

	testResult := ((self objectNamed: gsClass) buildSuiteFromMethods: testCollection) run.

"	stream := WriteStream on: String new.
	testResult failures do: [:each | stream nextPutAll: ',', 'F_', each selector].
	testResult errors do: [:each | stream nextPutAll: ',', 'E_', each selector].
	testResult passed do: [:each | stream nextPutAll: ',', 'P_', each selector].

	^stream contents"

	^testResult printString
%

category: 'category'
method: JadeServer
saveWorkingCopy: wc to: stream

	self _addToPureExportSet: wc.
	stream
		nextPutAll: (self oopOf: wc) printString; tab;
		nextPutAll: wc package name; tab;
		nextPutAll: wc modified printString; tab;
		nextPutAll: wc ancestors size printString; tab;
		yourself.
	wc ancestors do: [:ancestor |
		self _addToPureExportSet: ancestor.
		(self oopOf: ancestor) printOn: stream.
		stream tab.
	].
	stream nextPutAll: wc repositoryGroup repositories size printString; tab.
	wc repositoryGroup repositories do: [:repository |
		self _addToPureExportSet: repository.
		(self oopOf: repository) printOn: stream.
		stream tab.
	].

%

category: 'category'
method: JadeServer
sbAddDictionary: anOrderedCollection

	| currentName newName symbolList index |
	symbolList := self symbolList.
	newName := anOrderedCollection removeFirst.
	anOrderedCollection notEmpty ifTrue: [
		currentName := anOrderedCollection removeFirst asSymbol.
		index := symbolList findFirst: [:each | each name = currentName].
	] ifFalse: [
		index := symbolList size + 1.
	].
	symbolList
		createDictionaryNamed: newName
		at: index.
	selections at: #'dictionary' put: newName.
	self systemBrowserUpdate.
%

category: 'category'
method: JadeServer
sbAddMethodCategory: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) addCategory: anOrderedCollection first.
	selections at: #'methodCategory' put: anOrderedCollection first.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbAddMissingAccessors: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) compileMissingAccessingMethods.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbAddNameOf: aClass

	writeStream nextPutAll: aClass name.
	1 < (self historyOf: aClass) size ifTrue: [
		writeStream nextPutAll: ' ('.
		((self historyOf: aClass) indexOf: aClass) printOn: writeStream.
		writeStream nextPut: $/.
		(self historyOf: aClass) size printOn: writeStream.
		writeStream nextPut: $).
	].
	writeStream tab.

%

category: 'category'
method: JadeServer
sbAddRepository: list

	| description repository packages |
	description := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = description].
	packages := self mcWorkingCopyClass allManagers select: [:each | list includes: each package name].
	packages do: [:each | each repositoryGroup addRepository: repository].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbBreak: anOrderedCollection

	| myClass gsMethod stepPoint |
	myClass := self sbClassFrom: anOrderedCollection.
	gsMethod := self compiledMethodAt: anOrderedCollection removeFirst asSymbol inClass: myClass.
	stepPoint := anOrderedCollection removeFirst asNumber.
	anOrderedCollection removeFirst = 'set' ifTrue: [
		gsMethod setBreakAtStepPoint: stepPoint.
	] ifFalse: [
		gsMethod clearBreakAtStepPoint: stepPoint.
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbBrowseClassReferences: anOrderedCollection

	| class |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	writeStream 
		nextPutAll: 'browseClassReferences'; lf;
		nextPutAll: (self referencesToObject: class); 
		yourself.

%

category: 'category'
method: JadeServer
sbBrowseGlobalReferences: anOrderedCollection

	| global |
	global := self objectForOop: anOrderedCollection removeFirst asNumber.
	writeStream 
		nextPutAll: 'browseGlobalReferences'; lf;
		nextPutAll: (self referencesToObject: global); 
		yourself.

%

category: 'category'
method: JadeServer
sbBrowseImplementors: anOrderedCollection

	writeStream 
		nextPutAll: 'browseImplementors'; lf;
		nextPutAll: (self implementorsOf: anOrderedCollection removeFirst);
		yourself.

%

category: 'category'
method: JadeServer
sbBrowseMethodsContaining: anOrderedCollection

	writeStream 
		nextPutAll: 'browseMethodsContaining'; lf;
		nextPutAll: (self methodsContaining: anOrderedCollection removeFirst);
		yourself.

%

category: 'category'
method: JadeServer
sbBrowseMethodsWithPragma: anOrderedCollection

	writeStream 
		nextPutAll: 'browseMethodsWithPragma'; lf;
		yourself.

%

category: 'category'
method: JadeServer
sbBrowseSenders: anOrderedCollection

	writeStream 
		nextPutAll: 'browseSenders'; lf;
		nextPutAll: (self sendersOf: anOrderedCollection removeFirst);
		yourself.

%

category: 'category'
method: JadeServer
sbChangeClassName: aList

	| oldName class newName changedIn |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [self error: 'Current name is ' , class name printString].
	newName := aList removeFirst asSymbol.
	class changeNameTo: newName.
	changedIn := OrderedCollection new.
	self symbolList do: [:each | 
		(each includes: class) ifTrue: [
			(each at: oldName ifAbsent: [nil]) == class ifFalse: [self error: 'Class not at name!'].
			(each includesKey: newName) ifTrue: [self error: 'Key already in use!'].
			each
				removeKey: oldName;
				at: newName put: class;
				yourself.
			changedIn add: each.
		].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbChangesInPackage: anOrderedCollection
	"where anOrderedCollection is {packageName, projectName}"

	self error: 'this message is no longer supported'
%

category: 'category'
method: JadeServer
sbCheckUniqueClassName: aList

	| oldName class newName |
	oldName := aList removeFirst asSymbol.
	class := self objectForOop: aList removeFirst asNumber.
	class name == oldName ifFalse: [
		writeStream nextPutAll: 'Current name is ' , class name printString. 
		^self.
	].
	newName := aList removeFirst asSymbol.
	self symbolList do: [:each | 
		((each includes: class) and: [each includesKey: newName]) ifTrue: [
			writeStream nextPutAll: 'Dictionary '.
			each name printOn: writeStream.
			writeStream nextPutAll: ' already has a global with name '.
			newName printOn: writeStream.
			^self.
		].
	].
	
%

category: 'category'
method: JadeServer
sbClass: aList

	| string newClass mcWorkingCopyClass packages dictName |
	string := aList first.
	newClass := string evaluate.
	self classOrganizer update.
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [
		packages := Array with: nil.
	] ifFalse: [
		packages := mcWorkingCopyClass allManagers collect: [:each | each package name].
		packages := packages select: [:each | (newClass category copyFrom: 1 to: (newClass category size min: each size)) = each].
		packages isEmpty ifTrue: [
			packages := Array with: nil.
		].
	].
	dictName := (newClass class canUnderstand: #'symbolDictionaryName')
		ifTrue: [newClass symbolDictionaryName]
		ifFalse: [
			| array |
			array := self dictionaryAndSymbolOf: newClass.
			array isNil
				ifTrue: ['UserGlobals']
				ifFalse: [array first name]].
	selections 
		at: #'package' 		put: packages first;
		at: #'dictionary' 		put: dictName asString;
		at: #'category' 		put: newClass category;
		at: #'className'	put: newClass name;
		at: #'class'				put: newClass;
		yourself.
	selectedClass := newClass.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbClassCategory: aList

	| category classes |
	category := aList removeFirst.
	category := category copyFrom: 1 to: category size - 1.
	classes := aList removeFirst subStrings reject: [:each | each isEmpty].
	classes := classes collect: [:each | self objectNamed: each asSymbol].
	classes := classes collect: [:each | each thisClass].
	classes do: [:each | each category: category].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbClassComment: anOrderedCollection

	| class doc txt |
	class := (self sbClassFrom: anOrderedCollection) thisClass.
	doc := (self objectInBaseNamed: #'GsClassDocumentation') newForClass: self.
	txt := (self objectInBaseNamed: #'GsDocText') new details: self sbNextParagraph trimSeparators.
	doc documentClassWith: txt.
	class rwComment: doc.
	self systemBrowserUpdate.
%

category: 'category'
method: JadeServer
sbClassesToDictionary: anOrderedCollection

	| action targetName target sourceNames sources classNames |
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	target := self symbolList detect: [:each | each name = targetName].
	sourceNames := self nextLineAsList collect: [:each | each asSymbol].
	sources := sourceNames collect: [:eachName | self symbolList detect: [:eachDictionary | eachDictionary name = eachName]].
	classNames := self nextLineAsList collect: [:each | each asSymbol].
	classNames do: [:eachName | 
		| source class |
		source := sources detect: [:eachDict | 
			class := eachDict detect: [:eachGlobal | eachGlobal isBehavior and: [eachGlobal name = eachName]] ifNone: [nil].
			class notNil.
		].
		target at: class name put: class.
		action = 'move' ifTrue: [source removeKey: class name].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbClassFrom: anOrderedCollection

	| selectedClassName selectedClassOop set myClass |
	selectedClassName := (anOrderedCollection removeFirst subStrings: Character space) first.
	selectedClassOop := anOrderedCollection removeFirst asNumber.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal |
			eachGlobal isBehavior ifTrue: [
				set addAll: (self historyOf: eachGlobal).
			].
		].
	].
	myClass := set detect: [:each | (self oopOf: each) = selectedClassOop].
	myClass name asString = selectedClassName ifFalse: [self error: 'Class not found!'].
	anOrderedCollection removeFirst = 'classTab' ifTrue: [myClass := myClass class].
	^myClass.

%

category: 'category'
method: JadeServer
sbComparePackages: anOrderedCollection

	| current ancestor repository patch string |
	current := self mcWorkingCopyClass forPackage: (self mcPackageClass named: anOrderedCollection removeFirst).
	ancestor := anOrderedCollection removeFirst.
	repository := anOrderedCollection removeFirst.
	repository := current repositoryGroup repositories detect: [:each | each description = repository].
	ancestor := repository class name = #'MCDictionaryRepository'
		ifTrue: [repository versionFromVersionNamed: ancestor]
		ifFalse: [repository versionFromFileNamed: ancestor , '.mcz'].
	patch := current package snapshot patchRelativeToBase: ancestor snapshot.
	string := self 
		_mcDescriptionOfPatch: patch
		baseName: ancestor info name
		alternateName: nil.
	writeStream 
		nextPutAll: 'comparePackages'; lf;
		nextPutAll: string;
		yourself.

%

category: 'category'
method: JadeServer
sbCopyMethodsFor: newClass

	| history oldClass symbolList |
	newClass isMeta ifFalse: [self sbCopyMethodsFor: newClass class].
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	newClass isMeta ifTrue: [oldClass := oldClass class].
	symbolList := self symbolList.
	oldClass selectors do: [:each | 
		| source category errors |
		source := (self compiledMethodAt: each inClass: oldClass) sourceString.
		category := self _behavior: oldClass categoryOfSelector: each.
		errors := newClass 
			compileMethod: source
			dictionaries: symbolList
			category: category.
		errors notNil ifTrue: [
			writeStream
				nextPutAll: 'compileError'; lf;
				nextPutAll: newClass name; tab;
				nextPutAll: category; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
			newClass removeSelector: each ifAbsent: [].
		].
	].

%

category: 'category'
method: JadeServer
sbFileOutClass: anOrderedCollection

	writeStream nextPutAll: (self sbClassFrom: anOrderedCollection) thisClass fileOutClass.

%

category: 'category'
method: JadeServer
sbFileOutDictionary: anOrderedCollection

	| dictionary |
	dictionary := self objectNamed: anOrderedCollection first.
	writeStream nextPutAll: '! ------- Create dictionary if it is not present
run
| aSymbol names userProfile |
aSymbol := ' , dictionary name printString , '.
userProfile := System myUserProfile.
names := userProfile symbolList names.
(names includes: aSymbol) ifFalse: [
	| symbolDictionary |
	symbolDictionary := SymbolDictionary new name: aSymbol; yourself.
	userProfile insertDictionary: symbolDictionary at: names size + 1.
].
' , '%
'.
	self classOrganizer
		fileOutClassesAndMethodsInDictionary: dictionary
		on: writeStream.

%

category: 'category'
method: JadeServer
sbFileOutMethod: anOrderedCollection

	| aClass |
	aClass := (self sbClassFrom: anOrderedCollection) thisClass.
	writeStream nextPutAll: aClass fileOutMethod: anOrderedCollection removeFirst.

%

category: 'category'
method: JadeServer
sbFindClass

	| classToPackageMap |
	classToPackageMap := self sbFindClassPackageMap.
	self symbolList do: [:eachDict | 
		| name |
		name := eachDict name.
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				| category |
				category := eachGlobal category.
				category isNil ifTrue: [category := ''].
"1"			self sbAddNameOf: eachGlobal.
				writeStream
"2"				nextPutAll: name; tab;
"3"				nextPutAll: category; tab;		"Class category"
"4"				nextPutAll: (classToPackageMap at: eachGlobal ifAbsent: ['']); tab;		"Package name if available"
					lf.
			].
		].
	].

%

category: 'category'
method: JadeServer
sbFindClassPackageMap

	| systemOrganizerClass mcWorkingCopyClass dictionary packageInfoList |
	dictionary := Dictionary new.
	(systemOrganizerClass := self objectInBaseNamed: #'SystemOrganizer') isNil ifTrue: [^dictionary].
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^dictionary].
	packageInfoList := mcWorkingCopyClass allManagers collect: [:each | each packageInfo].
	systemOrganizerClass new categoryDict keysAndValuesDo: [:catName :classes |
		| symbol packageInfo |
		symbol := catName asSymbol.
		packageInfo := packageInfoList detect: [:each | each includesSystemCategory: symbol] ifNone: [nil].
		packageInfo notNil ifTrue: [
			| name |
			name := packageInfo name.
			classes do: [:each | dictionary at: each put: name].
		].
	].
	^dictionary.

%

category: 'category'
method: JadeServer
sbFindSelectors: anOrderedCollection

	| allSymbols pattern |
	pattern := (anOrderedCollection collect: [:each | each = '*' ifTrue: [$*] ifFalse: [each]]) asArray.
	allSymbols := ((AllUsers userWithId: #SymbolUser ifAbsent: [AllUsers userWithId: #DataCurator]) resolveSymbol: #AllSymbols) value.
	allSymbols := allSymbols select: [:each |each asUppercase matchPattern: pattern].
	allSymbols := allSymbols select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	allSymbols := allSymbols asSortedCollection.
	allSymbols do: [:each | writeStream nextPutAll: each; nextPut: Character lf; yourself].

%

category: 'category'
method: JadeServer
sbInstVarsOldParent: oldParent newParent: newParent oldChild: oldChild

	| added removed newList used missing |
	added := newParent allInstVarNames asIdentitySet - oldParent allInstVarNames asIdentitySet.
	removed := oldParent allInstVarNames asIdentitySet - newParent allInstVarNames asIdentitySet.
	newList := oldChild instVarNames.
	used := IdentitySet new.
	(oldChild class canUnderstand: #'_methodDict') ifTrue: [
		oldChild _methodDict do: [:each | used addAll: each instVarsAccessed].
	].
	(oldChild class canUnderstand: #'persistentMethodDictsDo:') ifTrue: [
		oldChild persistentMethodDictsDo: [:eachDict | 
			eachDict do: [:eachMethod | 
				used addAll: eachMethod instVarsAccessed.
			].
		].
	].
	used := used * removed.	"Only interested in things that have been removed."
	missing := (used - newList asIdentitySet) asSortedCollection asArray.
	newList := newList , missing.
	newList := newList reject: [:each | added includes: each].
	^newList.

%

category: 'category'
method: JadeServer
sbListMethodPragmas
	"none before 3x"

%

category: 'category'
method: JadeServer
sbLoadLatestVersionOfConfiguration: anOrderedCollection

	anOrderedCollection do: [:each | 
		(self objectNamed: each) project latestVersion load.
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbMethodCategory: anOrderedCollection
	| behavior category |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection removeFirst.
	anOrderedCollection do: [:each | behavior rwMoveMethod: each asSymbol toCategory: category].
	self systemBrowserUpdate
%

category: 'category'
method: JadeServer
sbMethodClass: anOrderedCollection
	"Drag/drop method onto class"

	| sourceBehavior action targetName set target |
	sourceBehavior := self sbClassFrom: anOrderedCollection.
	action := anOrderedCollection removeFirst.
	targetName := anOrderedCollection removeFirst asSymbol.
	set := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			(eachGlobal isBehavior and: [eachGlobal name = targetName]) ifTrue: [set add: eachGlobal].
		].
	].
	1 < set size ifTrue: [self error: 'Target name is ambiguous!'].
	1 = set size ifFalse: [self error: 'Target not found!'].
	target := set asArray first.
	sourceBehavior isMeta ifTrue: [target := target class].
	anOrderedCollection do: [:each | 
		| gsMethod result |
		gsMethod := self compiledMethodAt: each asSymbol inClass: sourceBehavior.
		result := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
				compileMethod: gsMethod sourceString
				behavior: target
				symbolList: self symbolList 
				inCategory: (self _behavior: sourceBehavior categoryOfSelector: gsMethod selector).
		(result key notNil and: [action = 'move']) ifTrue: [
			sourceBehavior removeSelector: gsMethod selector.
		].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbMigrateAll: aClass

	| mcPlatformSupport classes instances |
	((mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport') notNil and: [mcPlatformSupport autoMigrate]) ifTrue: [^self].
	System commitTransaction ifFalse: [self error: 'commit failed!'].
	classes := (ClassOrganizer new allSubclassesOf: aClass) 
		inject: (IdentitySet withAll: (self historyOf: aClass))
		into: [:set :each | set addAll: (self historyOf: each); yourself].
	classes := classes asArray.
	instances := (self objectInBaseNamed: #'SystemRepository') listInstances: classes.
	1 to: classes size do: [:i | 
		| class |
		class := classes at: i.
		class 
			migrateInstances: (instances at: i) 
			to: (self historyOf: class) last.
		System commitTransaction ifFalse: [self error: 'commit failed!'].
	].

%

category: 'category'
method: JadeServer
sbNextParagraph

	| stream |
	stream := WriteStream on: String new.
	[
		readStream peek = $%.
	] whileFalse: [
		stream nextPutAll: self nextLine; lf.
	].
	self nextLine.
	^stream contents.

%

category: 'category'
method: JadeServer
sbObjectLog: anOrderedCollection

	| command priorities class log debuggerLogEntryClass | 
	(class := self objectInBaseNamed: #'ObjectLogEntry') isNil ifTrue: [^self].
	debuggerLogEntryClass := self objectInBaseNamed: #'DebuggerLogEntry'.
	(command := anOrderedCollection removeFirst) = 'delete' ifTrue: [
		anOrderedCollection do: [:each | 			| oop entry |
			oop := each asNumber.			entry := class objectLog detect: [:each2 | (self oopOf: each2) = oop] ifNone: [nil].			entry notNil ifTrue: [class objectLog remove: entry].
		].
		^self systemBrowserCommand.
	].
	writeStream nextPutAll: 'objectLog'; lf.
	priorities := anOrderedCollection removeFirst asArray collect: [:each | each asString asNumber].
	log := class objectLog select: [:each | priorities includes: each priority].
	log reverseDo: [:each | 
		| labelString objectString |
		objectString := String withAll: (each objectString asArray collect: [:char | 
			char asciiValue < 32 ifTrue: [Character space] ifFalse: [
			127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
		500 < objectString size ifTrue: [objectString := (objectString copyFrom: 1 to: 500) , '...'].
		each label = each object printString ifTrue: [
			labelString := ''.
		] ifFalse: [
			labelString := String withAll: (each labelString asArray collect: [:char | 
				char asciiValue < 32 ifTrue: [Character space] ifFalse: [
				127 < char asciiValue ifTrue: [$?] ifFalse: [char]]]).
			500 < labelString size ifTrue: [labelString := (labelString copyFrom: 1 to: 500) , '...'].
		].
"1"	(self oopOf: each) printOn: writeStream.
"2"	writeStream tab; nextPutAll: each class name; tab.
"3"	each pid printOn: writeStream. 
		writeStream tab.
"4"	each stamp rounded printOn: writeStream.
"5"	writeStream tab; nextPutAll: labelString; tab.
"6"	each priority printOn: writeStream.
		writeStream tab.
"7"	each tag printOn: writeStream.
"8"	writeStream tab; nextPutAll: objectString; tab.
		(debuggerLogEntryClass notNil and: [each isKindOf: debuggerLogEntryClass]) ifTrue: [
"9"		(self oopOf: each continuation) printOn: writeStream.
		] ifFalse: [
			writeStream nextPutAll: '0'.
		].
		writeStream lf.
	].

%

category: 'category'
method: JadeServer
sbPostSaveClass: anOrderedCollection
	"this has been Rowanized"

	self systemBrowserUpdate 
%

category: 'category'
method: JadeServer
sbReadMethodFilter

	| pieces |
	pieces := self nextLine subStrings: Character tab.
	methodFilterType := pieces at: 1.
	methodCommandResult type: methodFilterType.
	methodCommandResult writeTypeTo: writeStream

%

category: 'category'
method: JadeServer
sbRecompileSubclassesOf: newClass andCopyMethods: aBoolean

	| history oldClass symbolList list |
	history := self historyOf: newClass thisClass.
	oldClass := history at: history size - 1.
	symbolList := self symbolList.
	list := self classOrganizer subclassesOf: oldClass.
	list do: [:oldSubclass |
		| instVars classInstVars definition string newSubclass i j |
		instVars := self sbInstVarsOldParent: oldClass newParent: newClass oldChild: oldSubclass.
		classInstVars := self sbInstVarsOldParent: oldClass class newParent: newClass class oldChild: oldSubclass class.

		definition := oldSubclass definition.
		0 < (i := definition findString: 'instVarNames:' startingAt: 1) ifTrue: [
			j := definition indexOf: Character lf startingAt: i.
			string := String withAll: 'instVarNames: #('.
			instVars do: [:each | string addAll: each; add: Character space].
			string add: $).
			definition := (definition copyFrom: 1 to: i - 1) , string , (definition copyFrom: j to: definition size).
		].
		0 < (i := definition findString: 'classInstVars:' startingAt: 1) ifTrue: [
			j := definition indexOf: Character lf startingAt: i.
			string := String withAll: 'classInstVars: #('.
			classInstVars do: [:each | string addAll: each; add: Character space].
			string add: $).
			definition := (definition copyFrom: 1 to: i - 1) , string , (definition copyFrom: j to: definition size).
		].
		newSubclass := definition evaluate.
		aBoolean ifTrue: [self sbCopyMethodsFor: newSubclass].
		self classOrganizer update.
	].

%

category: 'category'
method: JadeServer
sbRemoveClasses
	| containers classNames |
	self nextLine = 'packageList'. "ignore this" 
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty])
				collect: [:each | (each subStrings: Character space) first asSymbol].
	containers do: 
			[:packageName |
			classNames
				do: [:className | (Rowan packageServiceClass forPackageNamed: packageName) removeClassNamed: className]].
	^self systemBrowserUpdate
%

category: 'category'
method: JadeServer
sbRemoveDictionaries: anOrderedCollection

	anOrderedCollection do: [:each | 
		self symbolList removeDictionaryNamed: each asSymbol.
	].
	self systemBrowserUpdate.
%

category: 'category'
method: JadeServer
sbRemoveGlobals

	| symbolList dictionaries globals |
	symbolList := self symbolList.
	dictionaries := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries := dictionaries collect: [:eachName | symbolList detect: [:eachDict | eachDict name = eachName]].
	globals := self nextLineAsList collect: [:each | each asSymbol].
	dictionaries do: [:eachDict | 
		globals do: [:eachKey | 
			eachDict removeKey: eachKey ifAbsent: [].
		].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbRemoveHistory: aClass

	(ClassOrganizer new allSubclassesOf: aClass) asArray , (Array with: aClass) do: [:eachNewClass | 
		(self historyOf: eachNewClass) asArray do: [:eachClass | 
			eachClass ~~ eachNewClass ifTrue: [
				(self historyOf: eachNewClass) removeVersion: eachClass.
			].
		].
	].

%

category: 'category'
method: JadeServer
sbRemoveKey: aSymbol fromDictionary: aDictionary

	aDictionary removeKey: aSymbol.

%

category: 'category'
method: JadeServer
sbRemoveMethodCategories: anOrderedCollection

	| behavior |
	behavior := self sbClassFrom: anOrderedCollection.
	anOrderedCollection do: [:each | behavior rwRemoveCategory: each].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbRemoveMethods: anOrderedCollection
	| behavior classEntity  notRemoved|
	behavior := self sbClassFrom: anOrderedCollection.
	classEntity := Rowan classServiceClass forClassNamed: behavior name meta: behavior isMeta.
	notRemoved := Array new. 
	anOrderedCollection do: [:each | classEntity removeSelector: each asSymbol ifAbsent: [notRemoved add: each]].
	self systemBrowserUpdate.
	notRemoved isEmpty ifFalse:[self error: 'The following selectors were not removed. Possibly in a superclass? ', notRemoved printString].
%

category: 'category'
method: JadeServer
sbRemovePriorVersions

	| isPackages containers classNames |
	isPackages := self nextLine = 'packageList'.
	containers := self nextLineAsList.
	classNames := (self nextLineAsList reject: [:each | each isEmpty]) collect: [:each | (each subStrings: Character space) first asSymbol].
	self symbolList do: [:eachDictionary | 
		| dictionaryName |
		dictionaryName := eachDictionary name asString.
		classNames do: [:eachName |
			| class flag |
			(class := eachDictionary at: eachName ifAbsent: [nil]) notNil ifTrue: [
				isPackages ifTrue: [
					flag := false.
					containers do: [:each | flag := flag or: [(class category copyFrom: 1 to: (class category size min: each size)) = each]].
				] ifFalse: [
					flag := containers includes: dictionaryName.
				].
				flag ifTrue: [
					| classHistory |
					classHistory := self historyOf: class.
					classHistory size - 1 timesRepeat: [
						classHistory removeVersion: classHistory first.
					].
				].
			].
		].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbRevertClass

	| isPackages container className |
	isPackages := self nextLine = 'packageList'.
	container := self nextLine trimSeparators.
	className := self nextLine trimSeparators.
	self symbolList do: [:eachDictionary | 
		| dictionaryName class flag |
		dictionaryName := eachDictionary name asString.
		(class := eachDictionary at: className ifAbsent: [nil]) notNil ifTrue: [
			isPackages ifTrue: [
				flag := (class category copyFrom: 1 to: (class category size min: container size)) = container.
			] ifFalse: [
				flag := container = dictionaryName.
			].
			flag ifTrue: [
				| history |
				history := class classHistory.
				(class == history last and: [1 < history size]) ifFalse: [self error: 'Unexpected class history!'].
				history removeVersion: class.
				class := history last.
				eachDictionary at: class name put: class.
			].
		].
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbRunClassTests: aString

	| behavior |
	behavior := self sbClassFrom: (aString subStrings: Character tab).
	^self defectiveTestsIn: behavior.

%

category: 'category'
method: JadeServer
sbRunMethodTests: aString

	| list class |
	list := aString subStrings: Character tab.
	class := (self sbClassFrom: list) thisClass.
	list do: [:each | class debug: each asSymbol].
	^true.

%

category: 'category'
method: JadeServer
sbSaveMethod: anOrderedCollection
	"Save in method editor"

	| behavior category string association gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	association := self		"key: GsNMethod value: (Array withAll: errors and warnings)"
		compileMethod: string 
		behavior: behavior 
		symbolList: self symbolList 
		inCategory: category asSymbol.
	(gsMethod := association key) isNil ifTrue: [
		System
			signal: 1001 
			args: (Array with: association value)
			signalDictionary: GemStoneError.
	].
	selections 
		at: #'methodCategory' 	put: (self _behavior: gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'					put: gsMethod selector asString;
		at: #'methodWarnings'	put: association value;
		yourself.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbSavePackage: list

	| packageName package repositoryDescription repository versionName httpUser httpPassword comment |
	packageName := list removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	repositoryDescription := list removeFirst.
	repository := self mcRepositoryGroup repositories detect: [:each | each description = repositoryDescription].
	versionName := list removeFirst.
	list notEmpty ifTrue: [httpUser := list removeFirst].
	list notEmpty ifTrue: [httpPassword := list removeFirst].
	comment := self sbNextParagraph.
	[
		comment notEmpty and: [comment last asciiValue <= 32].
	] whileTrue: [
		comment := comment copyFrom: 1 to: comment size - 1.
	].
	(repository class name = #'MCHttpRepository') ifTrue: [
		repository
			user: httpUser;
			password: httpPassword;
			yourself.
	].
	self 
		mcStore: package 
		name: versionName 
		message: comment 
		repository: repository.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbSetHomeDictionary: list

	| name dictionary packagePolicy |
	name := list removeFirst asSymbol.
	dictionary := self symbolList detect: [:each | each name = name].
	(packagePolicy := self gsPackagePolicy) notNil ifTrue: [
		packagePolicy homeSymbolDict: dictionary.
	].
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer
sbUniqueVersionName: aList

	| packageName package |
	packageName := aList removeFirst.
	package := self mcWorkingCopyClass allManagers detect: [:each | each package name = packageName].
	writeStream
		nextPutAll: 'uniqueVersionName'; lf;
		nextPutAll: package uniqueVersionName;
		yourself.


%

category: 'category'
method: JadeServer
sbUnloadPackage: anOrderedCollection
	| service |
	service := Rowan packageServiceClass forPackageNamed: anOrderedCollection removeFirst.
	service deletePackage.
	self systemBrowserUpdate
%

category: 'category'
method: JadeServer
sbUpdateClasses

	| tabName |
	tabName := self nextLine.
	writeStream nextPutAll: tabName; lf.
	tabName = 'classList' ifTrue: [^self sbUpdateClassList].
	tabName = 'classHierarchy' ifTrue: [^self sbUpdateClassHierarchy].
	self error: 'Unexpected token!'.

%

category: 'category'
method: JadeServer
sbUpdateMethod

	| classes method names selection aSymbol |

	aSymbol := methodCommandResult selectedSelectors first. 

	"Inherited implimentors"
	classes := self sbUpdateMethodInheritedImplementationsOf: aSymbol.
	names := classes collect: [:each | each name asString].
	methodCommandResult inheritedClasses: names. 
	methodCommandResult writeInheritedClassesTo: writeStream.  "Line 1"

	"Which inherited implementation is selected?"
	selection := self nextLine.
	(names includes: selection) ifFalse: [selection := names last].
	methodCommandResult inheritedClass: selection.
	methodCommandResult writeInheritedClassTo: writeStream.	"Line 2"

	method := self compiledMethodAt: aSymbol inClass: (classes detect: [:each | each name asString = selection]).
	self _describeMethod: method.

%

category: 'category'
method: JadeServer
sbUpdateMethodBreakPointsFor: aMethod
	"Answers an Array of step points"

	^aMethod _stepPointsFromBreakIpOffsets: aMethod _breakpointIpOffsets.		"at least as far back as 32-bit 6.3.0 and 64-bit 2.3.0, but not in 64-bit 3.0"

%

category: 'category'
method: JadeServer
sbUpdateMethodCategories

	methodCommandResult classNamesFrom: classList. 
	methodCommandResult updateMethodCategories; 
		writeMethodCategoriesTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
			hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream.
%

category: 'category'
method: JadeServer
sbUpdateMethodFilter
	
	selectedClass isNil ifTrue: [^self].
	methodFilterType = 'categoryList' ifTrue: [^self sbUpdateMethodCategories].
	methodFilterType = 'variableList' ifTrue: [^self sbUpdateMethodVariables].
	methodFilterType = 'pragmaList' ifTrue: [^self sbUpdateMethodPragmas].
	self error: 'Unexpected token!'.

%

category: 'category'
method: JadeServer
sbUpdateMethodFilterSelections

	| mySelections override |
	mySelections := self nextLineAsList.
	(override := selections at: #'methodCategory' ifAbsent: [nil]) notNil ifTrue: [mySelections := Array with: override].
	mySelections := methodFilters select: [:each | mySelections includes: each asString].
	mySelections notEmpty ifTrue: [methodFilters := mySelections].
	self writeList: mySelections.

%

category: 'category'
method: JadeServer
sbUpdateMethodInheritedImplementationsOf: aSymbol

	| classes currentClass |
	classes := OrderedCollection new.
	currentClass := classList last.
	[
		currentClass notNil.
	] whileTrue: [
		(self class: currentClass includesSelector: aSymbol) ifTrue: [classes add: currentClass].
		currentClass := currentClass superclass.
	].
	^classes reverse.

%

category: 'category'
method: JadeServer
sbUpdateMethodPragmas

	self sbUpdateMethodFilterSelections.

%

category: 'category'
method: JadeServer
sbUpdateMethods

	| selectors |
	selectedClass isNil ifTrue: [^self].
	methodFilterType = 'categoryList' ifTrue: [selectors := self sbUpdateMethodsByCategories] ifFalse: [
	methodFilterType = 'variableList' ifTrue: [selectors := self sbUpdateMethodsByVariables] ifFalse: [
	methodFilterType = 'pragmaList' ifTrue: [selectors := self sbUpdateMethodsByPragmas] ifFalse: [
		self error: 'Unrecognized methodFilterType: ' , methodFilterType printString]]].
	methodCommandResult selectors: selectors asSortedCollection asArray.
	methodCommandResult updateMethodsInfo.
	methodCommandResult writeMethodsTo: writeStream. 
	self sbUpdateMethodSelections

%

category: 'category'
method: JadeServer
sbUpdateMethodsByCategories

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		eachClass selectors do: [:eachSelector |
			( methodFilters includes: (self _behavior: eachClass categoryOfSelector: eachSelector)) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.

%

category: 'category'
method: JadeServer
sbUpdateMethodsByPragmas
	"none before 3.x"

	^#()
%

category: 'category'
method: JadeServer
sbUpdateMethodsByVariables

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	selectedClass selectors do: [:eachSelector | 
		| gsMethod |
		gsMethod := self compiledMethodAt: eachSelector inClass: selectedClass.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.

%

category: 'category'
method: JadeServer
sbUpdateMethodSelections

	| priorSelections override newSelections aList |
	aList := methodCommandResult selectors asArray. 
	priorSelections := self nextLineAsList.
	(override := selections at: #'method' ifAbsent: [nil]) notNil ifTrue: [priorSelections := Array with: override].
	newSelections := aList select: [:each | priorSelections includes: each asString].
	methodCommandResult selectedSelectors: newSelections. 
	methodCommandResult writeSelectedSelectorsTo: writeStream.
	newSelections size = 1 ifTrue: [self sbUpdateMethod].


%

category: 'category'
method: JadeServer
sbUpdateMethodVariables

	methodCommandResult classNamesFrom: classList. 
	methodCommandResult updateMethodVariables. 
	methodCommandResult writeMethodFiltersTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
		hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream. 


%

category: 'category'
method: JadeServer
sbUpdatePackage: aString
	| package workingCopy list index |
	self mcPackageClass isNil
		ifTrue: 
			[writeStream
				nextPut: $%;
				lf.
			writeStream
				nextPut: $%;
				lf.
			^self ].
	package := self mcPackageClass named: aString.
	workingCopy := self mcWorkingCopyClass forPackage: package.
	list := workingCopy ancestors collect: [:each | 0 -> each].
	index := 1.
	[list size < 4 and: [index <= list size]] whileTrue: 
			[| assoc |
			assoc := list at: index.
			assoc value ancestors do: [:parent | list add: assoc key + 1 -> parent].
			index := index + 1].
	list do: 
			[:each |
			| date time |
			date := each value date isNil
						ifTrue: ['']
						ifFalse: [each value date asStringUsingFormat: #(3 2 1 $- 1 1)].
			time := each value time isNil
						ifTrue: ['']
						ifFalse: [each value time asStringUsingFormat: #($: true false)].
			writeStream
				nextPutAll: each key printString;
				tab;
				nextPutAll: each value name;
				tab;
				nextPutAll: date;
				nextPut: $T;
				nextPutAll: time;
				tab;
				nextPutAll: (each value message
							collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$.] ifFalse: [char]]);
				lf].
	writeStream
		nextPut: $%;
		lf.
	workingCopy repositoryGroup repositories do: 
			[:each |
			writeStream
				nextPutAll: each class name;
				tab;
				nextPutAll: each description;
				tab;
				yourself.
			each class name = #MCHttpRepository
				ifTrue: 
					[writeStream
						nextPutAll: each user;
						tab;
						nextPutAll: each password;
						yourself]
				ifFalse: 
					[writeStream
						tab;
						tab].
			writeStream lf].
	writeStream
		nextPut: $%;
		lf
%

category: 'category'
method: JadeServer
sbUpdatePackagesOrDictionaries

	| selectedTab |
	selectedTab := self nextLine.
	"Removed for Rowan which may not have Monticello loaded, but have a package tab"
	"(self mcWorkingCopyClass isNil or: [self gsPackagePolicy isNil]) ifTrue: [selectedTab := 'dictionaryList']." 
	writeStream nextPutAll: selectedTab; lf.
	classList := OrderedCollection new.
	selectedTab = 'dictionaryList' ifTrue: [^self sbUpdateDictionaries].
	selectedTab = 'packageList' ifTrue: [^self sbUpdatePackages].
	selectedTab = 'projectList' ifTrue:[
		self sbUpdatePackages. 
		^self updateProjects].
	self error: 'unexpected token'.

%

category: 'category'
method: JadeServer
selectedClassOverridesSelector: aSymbol

	^selectedClass superclass notNil and: [selectedClass superclass canUnderstand: aSymbol].

%

category: 'category'
method: JadeServer
selectorsMatching: aString

	| user stream list |
	list := (aString subStrings: $*) asOrderedCollection collect: [:each | each asUppercase].
	list size - 1 to: 1 do: [:i | list add: $* afterIndex: i].
	aString last = $* ifTrue: [list addLast: $*].
	stream := WriteStream on: String new.
	user := AllUsers 
		userWithId: #SymbolUser 
		ifAbsent: [AllUsers userWithId: #DataCurator].
	list := list asArray.
	list := (user resolveSymbol: #AllSymbols) value select: [:each |each asUppercase matchPattern: list].
	list := list select: [:each | (self classOrganizer implementorsOf: each) notEmpty].
	list := list asSortedCollection.
	list do: [:each | stream nextPutAll: each; nextPut: Character lf; yourself].
	^stream contents.

%

category: 'category'
method: JadeServer
sendersOf: anObject

	| symbol |
	symbol := (anObject isKindOf: String)
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	^self streamOfMethods: (self classOrganizer sendersOf: symbol) first.

%

category: 'category'
method: JadeServer
sendSigAbortToSession: anInteger

	System sendSigAbortToSession: anInteger negated.

%

category: 'category'
method: JadeServer
sendSigUsr1ToSession: anInteger

	| description command result |
	description := System descriptionOfSession: anInteger.
	command := 'kill -usr1 ' , (description at: 2) printString.
	result := System performOnServer: command.
	result trimSeparators notEmpty ifTrue: [self error: result trimSeparators].

%

category: 'category'
method: JadeServer
setBreakAtStepPoint: anInteger inMethod: aGsMethod

	aGsMethod setBreakAtStepPoint: anInteger.

%

category: 'category'
method: JadeServer
show: anObject

	self nextPutAll: anObject printString.

%

category: 'category'
method: JadeServer
sleepAndCommit

	[
		System commitTransaction.
	] whileTrue: [
		(Delay forSeconds: 30) wait.
	].

%

category: 'category'
method: JadeServer
sourceFor: anObject in: aClass

	| behavior selector packageName category mcTimestamp dict source |
	selector := (anObject isKindOf: String) 
		ifTrue: [anObject asSymbol]
		ifFalse: [anObject selector].
	behavior := self
		behaviorFor: selector 
		in: aClass.
	category := self _behavior: behavior categoryOfSelector: selector.
	packageName := category first = $*
		ifTrue: [self _packageNameFor: category]
		ifFalse: [behavior thisClass _classCategory].
	packageName isNil ifTrue: [packageName := ''].
	mcTimestamp := ''.
	dict := behavior extraDict.
	dict notNil ifTrue: [
		dict := dict at: #'GSMethodStampDict' ifAbsent: [nil].
		dict notNil ifTrue: [
			mcTimestamp := dict
				at: selector
				ifAbsent: [''].
		].
	].
	source := behavior sourceCodeAt: selector.
	^(WriteStream on: String new)
		nextPutAll: packageName; tab;
		nextPutAll: category; tab;
		nextPutAll: mcTimestamp; lf;
		nextPutAll: source;
		contents.

%

category: 'category'
method: JadeServer
sourceForProcess: gsProcess frame: level

	self subclassResponsibility
%

category: 'category'
method: JadeServer
stackForProcess: aGsProcess

	| array stream |
	Exception
		category: nil
		number: nil
		do: [:ex :cat :num :args | nil].
	array := aGsProcess _reportOfSize: 5000.
	stream := WriteStream on: String new.
	array do: [:each | 
		stream nextPutAll: each; lf.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
step: aGsProcess inFrame: anInteger
	aGsProcess _stepOverInFrame: anInteger.
%

category: 'category'
method: JadeServer
stepPointsFor: aGsMethod in: aClass

	| behavior method source breakStepPoints stepPoint stream |
	behavior := self
		behaviorFor: aGsMethod selector
		in: aClass.
	source := behavior sourceCodeAt: aGsMethod selector.
	method := self compiledMethodAt: aGsMethod selector inClass: behavior.
	stream := WriteStream on: String new.
	breakStepPoints := (aGsMethod class canUnderstand: #'_breakpointIpOffsets')
		ifTrue: [aGsMethod _stepPointsFromBreakIpOffsets: aGsMethod _breakpointIpOffsets]
		ifFalse: [#()].
	stepPoint := 0.
	method _sourceOffsets do: [:each | 
		stepPoint := stepPoint + 1.
		(breakStepPoints includes: stepPoint) ifTrue: [stream nextPut: $B].
		each printOn: stream.
		stream nextPut: Character space.
	].
	stream lf; 
		nextPutAll: (self stringOfLineNumbersWithBreaksIn: method); lf;
		nextPutAll: source;
		yourself.
	^stream contents.

%

category: 'category'
method: JadeServer
stoneInfo

	| dict stream |
	stream := (WriteStream on: String new)
		nextPutAll: self streamType; tab;
		nextPutAll: self stringType; tab;
		cr;
		yourself.
	dict := System stoneVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System stoneConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
%

category: 'category'
method: JadeServer
stopSession: anInteger

	System stopSession: anInteger.

%

category: 'category'
method: JadeServer
streamOfMethods: aList

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self
			_addMethod: each 
			toStream: stream.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
streamType

	^'Legacy'
%

category: 'category'
method: JadeServer
stringForClassList: aList

	| stream |
	stream := WriteStream on: String new.
	aList do: [:each | 
		self 
			_addClass: each 
			toStream: stream.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
stringType

	^'String'
%

category: 'category'
method: JadeServer
subclassSelectorForClass: aClass

	(aClass isBytes and: [aClass superclass notNil and: [aClass superclass isBytes not]]) ifTrue: [
		^'byteSubclass:'.
	].
	(aClass isIndexable and: [aClass superclass notNil and: [aClass superclass isIndexable not]]) ifTrue: [
		^'indexableSubclass:'.
	].
	((aClass class canUnderstand: #'isTransientDB') and: [aClass isTransientDB]) ifTrue: [
		^'transientSubclass:'.
	].
	^'subclass:'.

%

category: 'category'
method: JadeServer
superclassesOf: aClass isMeta: aBoolean

	| myClass list |
	myClass := aBoolean ifTrue: [aClass class] ifFalse: [aClass].
	list := myClass _allSuperList , (Array with: myClass).
	^self stringForClassList: list.

%

category: 'category'
method: JadeServer
symbolList

	^Rowan image symbolList
%

category: 'category'
method: JadeServer
systemBrowser: aString
	^self copy systemBrowserA: aString.

%

category: 'category'
method: JadeServer
systemBrowserA: aString

	| time |
	time := self millisecondsElapsedTime: [
		selections := Dictionary new.
		readStream := ReadStream on: aString.
		writeStream := WriteStream on: String new.
		writeStream lf.
		self environment: (readStream upTo: Character space) asNumber.
		self systemBrowserCommand.
	].
	^time printString , writeStream contents.

%

category: 'category'
method: JadeServer
systemBrowserCommand

	| list command |
	list := self nextLineAsList asOrderedCollection.
	command := list removeFirst.
	command = 'addDictionary'				ifTrue: [^self sbAddDictionary: list].
	command = 'addMethodCategory' 		ifTrue: [^self sbAddMethodCategory: list].
	command = 'addMissingAccessors'		ifTrue: [^self sbAddMissingAccessors: list].
	command = 'addPackage' 				ifTrue: [^self sbAddPackage: list].
	command = 'addRepository'			ifTrue: [^self sbAddRepository: list].

	command = 'break' 					ifTrue: [^self sbBreak: list].
	command = 'browseClassReferences'		ifTrue: [^self sbBrowseClassReferences: list].
	command = 'browseGlobalReferences'		ifTrue: [^self sbBrowseGlobalReferences: list].
	command = 'browseImplementors'		ifTrue: [^self sbBrowseImplementors: list].
	command = 'browseMethodHistory'		ifTrue: [^self sbBrowseMethodHistory: list].
	command = 'browseMethodsContaining'	ifTrue: [^self sbBrowseMethodsContaining: list].
	command = 'browseMethodsWithPragma'	ifTrue: [^self sbBrowseMethodsWithPragma: list].
	command = 'browseSenders'			ifTrue: [^self sbBrowseSenders: list].

	command = 'changeClassName'			ifTrue: [^self sbChangeClassName: list].
	command = 'changesInPackage'			ifTrue: [^self sbChangesInPackage: list].
	command = 'checkUniqueClassName'		ifTrue: [^self sbCheckUniqueClassName: list].
	command = 'class' 					ifTrue: [^self sbClass: list].
	command = 'classCategory'				ifTrue: [^self sbClassCategory: list].
	command = 'classComment'			ifTrue: [^self sbClassComment: list].
	command = 'classesToDictionary'		ifTrue: [^self sbClassesToDictionary: list].
	command = 'comparePackages'			ifTrue: [^self sbComparePackages: list].

	command = 'fileOutClass'				ifTrue: [^self sbFileOutClass: list].
	command = 'fileOutDictionary'			ifTrue: [^self sbFileOutDictionary: list].
	command = 'fileOutMethod'			ifTrue: [^self sbFileOutMethod: list].
	command = 'findClass' 				ifTrue: [^self sbFindClass].
	command = 'findSelectors'				ifTrue: [^self sbFindSelectors: list].


	command = 'listMethodPragmas'			ifTrue: [^self sbListMethodPragmas: list].
	command = 'loadLatestVersion'			ifTrue: [^self sbLoadLatestVersionOfConfiguration: list].

	command = 'method' 					ifTrue: [^self sbSaveMethod: list].
	command = 'methodCategory'			ifTrue: [^self sbMethodCategory: list].
	command = 'methodClass'				ifTrue: [^self sbMethodClass: list].

	command = 'objectLog'				ifTrue: [^self sbObjectLog: list].

	command = 'postSaveClass'			ifTrue: [^self sbPostSaveClass: list].

	command = 'removeClasses'			ifTrue: [^self sbRemoveClasses].
	command = 'removeDictionaries'			ifTrue: [^self sbRemoveDictionaries: list].
	command = 'removeGlobals'			ifTrue: [^self sbRemoveGlobals].
	command = 'removeMethodCategories' 	ifTrue: [^self sbRemoveMethodCategories: list].
	command = 'removeMethods'			ifTrue: [^self sbRemoveMethods: list].
	command = 'removePriorVersions'		ifTrue: [^self sbRemovePriorVersions].
	command = 'removeRepository'			ifTrue: [^self sbRemoveRepository: list].
	command = 'revertClass'				ifTrue: [^self sbRevertClass].

	command = 'savePackage'				ifTrue: [^self sbSavePackage: list].
	command = 'setHomeDictionary'			ifTrue: [^self sbSetHomeDictionary: list].

	command = 'uniqueVersionName'		ifTrue: [^self sbUniqueVersionName: list].
	command = 'unloadPackage'			ifTrue: [^self sbUnloadPackage: list].
	command = 'update' 					ifTrue: [^self systemBrowserUpdate].

	self error: 'Unknown command: ' , command printString.

%

category: 'category'
method: JadeServer
systemBrowserSTON: aString
	^self copy systemBrowserSTONA: aString.

%

category: 'category'
method: JadeServer
systemBrowserSTONA: aString

	| time |
	time := self millisecondsElapsedTime: [
		selections := Dictionary new.
		readStream := ReadStream on: aString.
		writeStream := WriteStream on: String new.
		writeStream lf.
		self environment: (readStream upTo: Character space) asNumber.
		self systemBrowserCommand.
	].
	^STON toString: RowanCommandResult results

%

category: 'category'
method: JadeServer
systemConfigAsDictionary

	| char dict i line list stream |
	list := Array new.
	stream := GsFile openReadOnServer: '$GEMSTONE/data/system.conf'.
	[
		[
			line := stream nextLine reject: [:each | each == Character cr or: [each == Character lf]].
			(2 < line size and: [(line copyFrom: 1 to: 2) = '#=']) ifTrue: [
				list add: (WriteStream on: String new).
			] ifFalse: [
				list last nextPutAll: line; cr.
			].
			stream atEnd not.
		] whileTrue: [].
	] ensure: [
		stream close.
	].
	list := list copyFrom: 3 to: list size.
	list := list collect: [:each | each contents].
	dict := Dictionary new.
	list do: [:each | 
		line := (ReadStream on: each) nextLine.
		line = '# End of Default GemStone Configuration Options' ifTrue: [^dict].
		(line copyFrom: 1 to: 2) = '# ' ifFalse: [line error: 'Unrecognized config file format!'].
		i := 3.
		[
			i <= line size and: [(char := line at: i) == $_ or: [char isAlphaNumeric]].
		] whileTrue: [
			i := i + 1.
		].
		dict at: (line copyFrom: 3 to: i - 1) put: each.
	].
	self error: 'End of file not recognized!'.
%

category: 'category'
method: JadeServer
terminate: aGsProcess

	aGsProcess isNil ifTrue: [^self].
	aGsProcess terminate.
	(Delay forMilliseconds: 10) wait.	"allow forked processes to finish"

%

category: 'jadeite'
method: JadeServer
updateFromSton: stonString
  | services organizer resultString |
  [ 
  Rowan commandResultClass initializeResults.
  services := STON fromString: stonString.
  organizer := ClassOrganizer new.
  [ 
  services
    do: [ :service | 
      service organizer: organizer.
      service updateType: nil.	"Update type is only for returned commands"
      service command ifNil: [ service command: #'update' ].
      service servicePerform: service command withArguments: service commandArgs ] ]
    on: GsInteractionRequest
    do: [ :ex | 
      ex
        response:
          (ex interaction interactWith: self gsInteractionInformFailureHandler) ].
  self autoCommitIfRequired.
  Rowan loggingServiceClass current logSentServices.
  resultString := STON toString: Rowan commandResultClass results.
  ^ resultString ]
    on: Error
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      ex pass ]
%

category: 'category'
method: JadeServer
userList

	| list me stream |
	list := (AllUsers asSortedCollection: [:a :b | a userId <= b userId]) asOrderedCollection.
	me := System myUserProfile.
	list
		remove: me;
		addFirst: me;
		yourself.
	stream := WriteStream on: String new.
	list do: [:each | 
		self
			addUser: each 
			toStream: stream.
	].
	^stream contents.

%

category: 'category'
method: JadeServer
waitingProcesses

	^ProcessorScheduler scheduler waitingProcesses

%

category: 'category'
method: JadeServer
writeList: aList

	aList do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

%

category: 'category'
method: JadeServer
_addClass: each toStream: stream 

	self
		_addClass: each 
		toStream: stream 
		isVisible: true
		fromDictionary: nil.

%

category: 'category'
method: JadeServer
_addClass: aClass toStream: aStream isVisible: aBoolean fromDictionary: aDictionary
	"1. OOP; 2. key; 3. category; 4. dictionary name; 5. superclass OOP; 6. children; 7. Visible/Inherited; 8. Class History; 9. isTestCase"

	| testCaseClass history |
"1"	(self oopOf: aClass) printOn: aStream.
"2"	aStream tab; nextPutAll: (aDictionary  isNil ifTrue: [aClass name] ifFalse: [aDictionary keyAtValue: aClass ifAbsent: [aClass name]]); tab.
"3"	aClass category notNil ifTrue: [aStream nextPutAll: aClass category].
"4"	aStream tab; nextPutAll: (aDictionary isNil ifTrue: ['?'] ifFalse: [aDictionary name]).
"5"	aStream tab. (self oopOf: aClass superclass) printOn: aStream.
	aStream 
"6"		tab; "let client build children list"
"7"		tab; nextPut: (aBoolean ifTrue: [$V] ifFalse: [$I]);
		tab.
	(history := self historyOf: aClass) isNil ifTrue: [history := Array with: aClass].
"8"	(history indexOf: aClass) printOn: aStream.
	aStream nextPut: $/.
	history size printOn: aStream.
	aStream tab.
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
"9"	(testCaseClass notNil and: [aClass isSubclassOf: testCaseClass]) printOn: aStream.
	aStream lf.

%

category: 'category'
method: JadeServer
_addMethod: aGsMethod toStream: aStream
	"See GsMethod2>>initialize:"

	| inClass testCaseClass |
	inClass := aGsMethod inClass.
"1"	(self oopOf: aGsMethod) printOn: aStream.
	aStream 
"2"		tab; nextPutAll: aGsMethod selector; 
"3"		tab; nextPutAll: (self _behavior: inClass categoryOfSelector: aGsMethod selector);
		tab.

	"Class"
"4"	(self oopOf: inClass) printOn: aStream.
"5"	aStream tab; nextPutAll: inClass name; tab.
"6"	inClass category notNil ifTrue: [aStream nextPutAll: inClass category].
"7"	aStream tab; nextPutAll: (self nameOfFirstDictionaryReferencing: inClass thisClass); tab.

	"SUnit Test Method"
	testCaseClass := Globals
		at: #'TestCase'
		ifAbsent: [nil].
"8"	((testCaseClass notNil and: [inClass isSubclassOf: testCaseClass]) and: [inClass testSelectors includes: aGsMethod selector]) printOn: aStream.
	aStream lf.

%

category: 'category'
method: JadeServer
_addToPureExportSet: anObject

	System 
		_add: anObject 
		toGciSet: 39.  "PureExportSet"

%

category: 'category'
method: JadeServer
_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: each selectors; addAll: each class selectors.
	].
	^allSelectors
%

category: 'category'
method: JadeServer
_behavior: aBehavior categoryOfSelector: aSymbol

	^aBehavior categoryOfSelector: aSymbol
%

category: 'category'
method: JadeServer
_describeMCAddition: anMCAddition on: aStream

	aStream 
		nextPut: $A; tab;
		nextPutAll: (self oopOf: anMCAddition) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCAddition definition 
		on: aStream.

%

category: 'category'
method: JadeServer
_describeMCClassDefinition: anMCClassDefinition on: aStream

	| string |
	string := anMCClassDefinition definitionString collect: [:char |
		char = Character lf
			ifTrue: [Character cr]
			ifFalse: [char].
	].
	aStream
		nextPut: $C; tab;
		nextPutAll: string; lf;
		yourself.

%

category: 'category'
method: JadeServer
_describeMCDefinition: anMCDefinition on: aStream

	anMCDefinition isMethodDefinition ifTrue: [
		self 
			_describeMCMethodDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isOrganizationDefinition ifTrue: [
		self 
			_describeMCOrganizationDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	anMCDefinition isClassDefinition ifTrue: [
		self 
			_describeMCClassDefinition: anMCDefinition 
			on: aStream.
		^self.
	].
	self halt.

%

category: 'category'
method: JadeServer
_describeMCMethodDefinition: anMCMethodDefinition on: aStream
	| unicodeFreeSource |
	unicodeFreeSource := RowanMethodService removeUnicodeFromSource: anMCMethodDefinition source.
	aStream
		nextPut: $M; tab;
		nextPutAll: anMCMethodDefinition timeStamp; tab;
		nextPutAll: anMCMethodDefinition className; tab;
		nextPutAll: anMCMethodDefinition classIsMeta printString; tab;
		nextPutAll: anMCMethodDefinition category; tab;
		nextPutAll: anMCMethodDefinition selector; tab;
		nextPutAll: unicodeFreeSource size printString; tab;
		nextPutAll: unicodeFreeSource; lf.
%

category: 'category'
method: JadeServer
_describeMCModification: anMCModification on: aStream

	aStream nextPut: $M; tab;
		nextPutAll: (self oopOf: anMCModification) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCModification obsoletion 
		on: aStream.
	self 
		_describeMCDefinition: anMCModification modification 
		on: aStream.

%

category: 'category'
method: JadeServer
_describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream

	aStream
		nextPut: $O; tab;
		yourself.
	anMCOrganizationDefinition categories do: [:each | 
		aStream nextPutAll: each; tab.
	].
	aStream lf.

%

category: 'category'
method: JadeServer
_describeMCRemoval: anMCRemoval on: aStream

	aStream nextPut: $R; tab;
		nextPutAll: (self oopOf: anMCRemoval) printString; tab;
		yourself.
	self 
		_describeMCDefinition: anMCRemoval definition 
		on: aStream.

%

category: 'category'
method: JadeServer
_describeMethod: aMethod
  "Provide info needed to create a GsMethod in Jade client"

  "Nice to add packageName and mcTimestamp"

  | allSelectors class list oldGsMethod string x |
  self environment: (self environmentForMethod: aMethod).
  writeStream
    nextPutAll: (class := aMethod inClass) asOop printString;
    tab;
    nextPutAll: class printString;
    tab;
    nextPutAll: aMethod asOop printString;
    tab;
    nextPutAll:
        ((x := aMethod selector) isNil
            ifFalse: [ x ]
            ifTrue: [ '' ]);
    tab;
    nextPutAll: (self categoryOfMethod: aMethod);
    tab;
    nextPutAll: (self currentUserMayEditMethod: aMethod) asString;
    tab;
    lf.	"Line 1 for GsMethod (line 3 for JadeSystemBrowserPresenter)"	"1"	"2"	"3"	"4"	"5"	"6"	"Method source"
  writeStream nextPutAll: (string := aMethod sourceString).
  string last = Character lf
    ifFalse: [ writeStream lf ].
  writeStream
    nextPut: $%;
    lf.	"Lines 2-N"	"unimplemented selectors"	"https://github.com/jgfoster/Jade/issues/117"
  ((aMethod class includesSelector: #'_selectorPool')
    and: [ aMethod class includesSelector: #'_sourceOffsetOfFirstSendOf:' ])
    ifTrue: [ 
      allSelectors := self _allSelectors.
      (aMethod _selectorPool reject: [ :each | allSelectors includes: each ])
        do: [ :each | 
          (aMethod _sourceOffsetOfFirstSendOf: each) printOn: writeStream.
          writeStream
            space;
            nextPutAll: each;
            tab ] ].
  writeStream lf.	"Line N+1"	"Array of Associations (offset -> selector) indexed by step points"
  list := self sbUpdateMethodStepPointsFor: aMethod.
  list := list collect: [ :each | each key printString , ' ' , each value ].
  self writeList: list.	"Line N+2"	"breaks"
  list := self sbUpdateMethodBreakPointsFor: aMethod.
  self writeList: (list collect: [ :each | each printString ]).	"Line N+3"	"original method"
  oldGsMethod := (aMethod inClass class
    canUnderstand: #'persistentMethodDictForEnv:')
    ifTrue: [ 
      (aMethod inClass persistentMethodDictForEnv: 0)
        at: aMethod selector
        ifAbsent: [ aMethod ] ]
    ifFalse: [ aMethod ].
  aMethod ~~ oldGsMethod
    ifTrue: [ 
      string := oldGsMethod sourceString.
      writeStream nextPutAll: string.
      (string notEmpty and: [ string last = Character lf ])
        ifFalse: [ writeStream lf ] ].
  writeStream
    nextPut: $%;
    lf.	"method compile warnings"
  string := selections isNil
    ifTrue: [ '' ]
    ifFalse: [ selections at: #'methodWarnings' ifAbsent: [ '' ] ].
  string isNil
    ifTrue: [ string := '' ].
  writeStream
    nextPutAll: string;
    nextPut: $%;
    lf
%

category: 'category'
method: JadeServer
_mcDescriptionOfPatch: aPatch baseName: aString1 alternateName: aString2

	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aPatch) printOn: stream.
	stream 
		tab; nextPutAll: (aString1 isNil ifTrue: ['loaded'] ifFalse: [aString1]);
		nextPutAll: ' vs. ';
		nextPutAll: (aString2 isNil ifTrue: ['loaded'] ifFalse: [aString2]);
		lf.
	aPatch operations do: [:each | 
		each isAddition 		ifTrue: [self _describeMCAddition: 		each on: stream].
		each isModification 	ifTrue: [self _describeMCModification: 	each on: stream].
		each isRemoval 		ifTrue: [self _describeMCRemoval: 		each on: stream].
	].
	^stream contents.


%

category: 'category'
method: JadeServer
_mcTopazFrom: aSnapshot on: aStream

	| classes dict parents methods queue |
	classes := aSnapshot definitions select: [:each | each isClassDefinition].
	dict := Dictionary new.
	classes do: [:each | 
		| parent myself |
		parent := dict 
			at: each superclassName 
			ifAbsentPut: [nil -> Set new].
		myself := dict
			at: each className
			ifAbsentPut: [nil -> Set new].
		myself key: each.
		parent value add: myself.
	].
	dict := dict reject: [:each | each key isNil].
	parents := dict keys.
	dict copy do: [:each | 
		(parents includes: each key superclassName) ifTrue: [
			dict removeKey: each key className.
		].
	].
	queue := (dict asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
	[
		queue notEmpty.
	] whileTrue: [
		| assoc children def |
		assoc := queue removeFirst.
		children := (assoc value asSortedCollection: [:a :b | a key <= b key]) asOrderedCollection.
		queue := children , queue.
		def := assoc key.
		aStream 
			nextPutAll: '! - ' , def className; lf;
			nextPutAll: '! - ' , def commentStamp; lf;
			nextPutAll: 'run'; lf;
			nextPutAll: '(' , def superclassName; lf;
			tab; nextPutAll: 'subclass: ' , def className printString; lf;
			tab; nextPutAll: 'instVarNames: #(' , def instanceVariablesString , ')'; lf;
			tab; nextPutAll: 'classVars: #(' , def classVariablesString , ')'; lf;
			tab; nextPutAll: 'classInstVars: #(' , def classInstanceVariablesString , ')'; lf;
			tab; nextPutAll: 'poolDictionaries: #(' , def sharedPoolsString , ')'; lf;
			tab; nextPutAll: 'inDictionary: UserGlobals'; lf;
			tab; nextPutAll: 'instancesInvariant: false'; lf;
			tab; nextPutAll: 'isModifiable: false)'; lf;
			tab; nextPutAll: 'category: ' , def category printString , '.'; lf;
			nextPutAll: 'true.'; lf;
			nextPut: $%; lf;
			yourself.
	].
	methods := aSnapshot definitions select: [:each | each isMethodDefinition].
	methods := methods asSortedCollection.
	classes asSortedCollection do: [:eachClass | 
		| localMethods |
		localMethods := methods select: [:eachMethod | eachClass className = eachMethod className].
		methods removeAll: localMethods.
		aStream
			lf; nextPutAll: '! - *** - ' , eachClass className; lf;
			nextPutAll: 'removeAllClassMethods ' , eachClass className; lf;
			nextPutAll: 'removeAllMethods ' , eachClass className; lf;
			yourself.
		localMethods do: [:eachMethod | 
			| source |
			source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
			aStream
				nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
				nextPutAll: '! - ' , eachMethod timeStamp; lf;
				nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
				nextPutAll: source; lf;
				nextPut: $%; lf;
				yourself.
		].
	].
	aStream lf; nextPutAll: '! - *** - loose methods (where class is expected to be already defined)'; lf; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '! - (none)'; lf; lf].

	methods do: [:eachMethod | 
		| source |
		source := eachMethod source copyReplaceAll: Character cr asString with: Character lf asString.
		aStream
			nextPutAll: 'category: ''' , eachMethod category , ''''; lf;
			nextPutAll: '! - ' , eachMethod timeStamp; lf;
			nextPutAll: (eachMethod classIsMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']) , eachMethod className; lf;
			nextPutAll: source; lf;
			nextPut: $%; lf;
			yourself.
	].

	aStream lf; nextPutAll: '! - *** - class initialization'; lf.
	methods isEmpty ifTrue: [aStream nextPutAll: '! - (none)'; lf; lf].
	classes do: [:each | 
		aStream nextPutAll: 'send ' , each className , ' initialize'; lf.
	].


%

category: 'category'
method: JadeServer
_methodsFor: aClass categories: aList

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass _includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (self compiledMethodAt: eachSelector inClass: aClass).
			].
		].
	].
	^methods.


%

category: 'category'
method: JadeServer
_methodsFor: aClass filter: aList isVariables: aBoolean

	^aBoolean 
		ifTrue:	[self _methodsFor: aClass variables: 	aList]
		ifFalse:	[self _methodsFor: aClass categories: aList].

%

category: 'category'
method: JadeServer
_methodsFor: aClass variables: aList

	| methods |
	aList isEmpty ifTrue: [^aClass selectors collect: [:each | self compiledMethodAt: each inClass: aClass]].
	methods := IdentitySet new.
	aClass selectors do: [:each | 
		| method intersect |
		method := self compiledMethodAt: each inClass: aClass.
		intersect := method instVarsAccessed * aList.
		intersect notEmpty ifTrue: [methods add: method].
	].
	^methods.

%

category: 'category'
method: JadeServer
_oopAndStringFor: anObject

	^(self oopOf: anObject) -> anObject printString.

%

category: 'category'
method: JadeServer
_packageNameFor: aCategoryName

	| string mcWorkingCopyClass list |
	(mcWorkingCopyClass := self mcWorkingCopyClass) isNil ifTrue: [^''].
	string := aCategoryName asUppercase copyFrom: 2 to: aCategoryName size.
	list := mcWorkingCopyClass allManagers collect: [:each | each packageName].
	list := list select: [:each | (string copyFrom: 1 to: (string size min: each size)) = each asUppercase].
	list isEmpty ifTrue: [^''].
	list size = 1 ifTrue: [^list first].
	^(list asSortedCollection: [:a :b | a size <= b size]) last.

%

category: 'category'
method: JadeServer
_sourceForProcess: gsProcess frame: level

	| frame homeMethod stepPoint keys values gsMethod receiver |
	writeStream := WriteStream on: String new.
	(frame := gsProcess _frameContentsAt: level) isNil ifTrue: [^'No frame found for level ' , level printString].
	gsMethod := frame at: 1.
	stepPoint := self 
		_stepPointFromProcess: gsProcess 
		frame: frame 
		method: gsMethod
		level: level.
	writeStream
		nextPutAll: '<?xml version=''1.0'' ?><frame oop=';
		nextPutAll: (self oopOf: frame) printString printString;
		nextPutAll: ' ipOffset=';
		nextPutAll: (frame at: 2) printString printString;
		nextPutAll: ' frameOffset=';
		nextPutAll: ((frame at: 3) isNil ifTrue: [''] ifFalse: [(frame at: 3) printString]) printString;
		nextPutAll: ' stepPoint=';
		nextPutAll: stepPoint printString printString;
		nextPutAll: '>'; lf;
		yourself.
	receiver := frame at: 10.
	values := OrderedCollection new.
	(self isClientForwarder: receiver) ifTrue: [
		keys := OrderedCollection with: 'clientObject'.
		values add: receiver clientObject.
		receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
	] ifFalse: [
		((receiver isKindOf: BlockClosure) or: [receiver isKindOf: Class]) ifTrue: [
			keys := OrderedCollection new.
		] ifFalse: [
			keys := receiver class allInstVarNames asOrderedCollection collect: [:each | '-' , each].
			1 to: keys size do: [:i |
				values add: (receiver instVarAt: i).
			].
		].
	].
	keys addFirst: #'receiver'.
	values addFirst: receiver.
	keys addAll: (frame at: 9).
	keys := keys reject: [:each | each first == $.].
	values addAll: (frame size >= 11
		ifTrue: [frame copyFrom: 11 to: frame size]
		ifFalse: [#()]).
	1 to: (keys size min: values size) do: [:i | | oop assoc key value |
		key := keys at: i.
		value := values at: i.
		assoc := self _oopAndStringFor: value.
		oop := assoc key.
		value := assoc value.
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		writeStream
			nextPutAll: '<var oop=';
			nextPutAll: oop asString printString;
			nextPutAll: ' name=';
			nextPutAll: key asString printString;
			nextPutAll: ' ><';
			nextPutAll: '![';
			nextPutAll: 'CDATA';
			nextPutAll: '[';
			nextPutAll: value;
			nextPutAll: ']';
			nextPutAll: ']';
			nextPutAll: '></var>'; lf;
			yourself.
	].
	homeMethod := self homeMethodFor: gsMethod.
	writeStream 
		nextPutAll: '<source';
		nextPutAll: ' ><';
		nextPutAll: '![';
		nextPutAll: 'CDATA';
		nextPutAll: '[';
		yourself.
	self _describeMethod: homeMethod.
	writeStream
		nextPutAll: ']';
		nextPutAll: ']';
		nextPutAll: '></source>';
		nextPutAll: '</frame>'; lf;
		yourself.
	^self asAsciiString: writeStream contents.


%

category: 'category'
method: JadeServer
_stepPointFromProcess: gsProcess frame: aFrame method: gsMethod level: anInteger

	self subclassResponsibility.
%

category: 'category'
method: JadeServer
_stepPointsForBreaksInMethod: gsMethod

	^gsMethod _stepPointsFromBreakIpOffsets: gsMethod _breakpointIpOffsets
%

category: 'category'
method: JadeServer
_trimStackOf: aGsProcess toLevel: anInteger

	aGsProcess _trimStackToLevel: anInteger.
	^aGsProcess.

%

! Class implementation for 'JadeServer64bit'

!		Instance methods for 'JadeServer64bit'

category: 'category'
method: JadeServer64bit
addSessionWithId: anInteger toStream: aStream

	[
		super
			addSessionWithId: anInteger
			toStream: aStream.
	] on: Error do: [:ex | 
		ex resume: '?????'.
	].

%

category: 'category'
method: JadeServer64bit
addUser: aUserProfile toStream: aStream

	[
		super
			addUser: aUserProfile 
			toStream: aStream.
	] on: Error do: [:ex | 
		aStream lf.
		ex return.
	].

%

category: 'category'
method: JadeServer64bit
asString: anObject

	^[
		super asString: anObject.
	] on: Error do: [:ex | 
		ex return: '???'.
	].

%

category: 'category'
method: JadeServer64bit
homeMethodFor: aGsMethod

	^[aGsMethod homeMethod] on: Error do: [:ex | ex return: aGsMethod]
%

category: 'category'
method: JadeServer64bit
installTranscript

	[
		super installTranscript.
	] on: Error do: [:ex | 
		ex return.
	].

%

category: 'category'
method: JadeServer64bit
mcInitialsA: aString

	^[
		super mcInitialsA: aString.
		true.
	] on: Error do: [:ex | 
		ex return: false.
	].

%

category: 'category'
method: JadeServer64bit
metacelloConfigurations

	| list |
	list := Array new.
	Rowan image symbolList do: [:eachSymbolList | 
		eachSymbolList do: [:eachGlobal | 
			(eachGlobal isBehavior and: [
			(eachGlobal class includesSelector: #'isMetacelloConfig') and: [
			eachGlobal isMetacelloConfig]]) ifTrue: [list add: eachGlobal].
		].
	].
	^list
%

category: 'category'
method: JadeServer64bit
objectForOop: anInteger

	^Object _objectForOop: anInteger.

%

category: 'category'
method: JadeServer64bit
recompile: aMethod withSource: aString
	| behavior |
	behavior := aMethod inClass.
	[[ behavior rwCompileMethod: aString
				category: (self _behavior: behavior categoryOfSelector: aMethod selector) ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
								on: RwPerformingUnpackagedEditNotification
								do: [:ex | ex resume ].
			Rowan serviceClass rowanFixMe.	"need to handle compile errors"
			^true
%

category: 'category'
method: JadeServer64bit
sbRemoveKey: aSymbol fromDictionary: aDictionary

	| aClass array |
	aClass := aDictionary at: aSymbol.
	array := self dictionaryAndSymbolOf: aClass.
	((array at: 1) == aDictionary and: [
		(array at: 2) == aSymbol and: [
		(Class canUnderstand: #'removeFromSystem') and: [	"mark package as modified"
		aClass removeFromSystem]]]) ifFalse: [
			aDictionary removeKey: aSymbol.
		].
%

category: 'category'
method: JadeServer64bit
sourceForProcess: gsProcess frame: level

	^[
		self
			_sourceForProcess: gsProcess 
			frame: level.
	] on: Error do: [:ex | 
			ex return: (self asAsciiString: ('?????' , ex description , Character cr asString , (GsProcess stackReportToLevel: 50))).
	].

%

category: 'category'
method: JadeServer64bit
streamType

	| isLegacy type |
	type := Globals at: #'PositionableStream_position' ifAbsent: [#'Legacy'].
	(Globals includesKey: #'PositionableStreamLegacy') ifFalse: [^type].
	isLegacy := PositionableStream isLegacyStreamImplementation.
	(type = #'Legacy') == isLegacy ifTrue: [^type].
	self error: 'Inconsistent PositionableStream configuration'.

%

category: 'category'
method: JadeServer64bit
stringType

	^(Globals at: #StringConfiguration ifAbsent: [String]) name

%

category: 'category'
method: JadeServer64bit
systemBrowser: aString

	[
		^super systemBrowser: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].

%

category: 'category'
method: JadeServer64bit
systemBrowserSTON: aString

	[
		^super systemBrowserSTON: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].

%

category: 'category'
method: JadeServer64bit
_oopAndStringFor: anObject

	^[
		super _oopAndStringFor: anObject.
	] on: Error do: [:ex | 
		ex return: 0 -> ('<ERROR IN #printString for ' , anObject class name , '>').
	].

%

category: 'category'
method: JadeServer64bit
_stepPointFromProcess: gsProcess frame: aFrame method: gsMethod level: anInteger

	^gsProcess _stepPointAt: anInteger

%

category: 'category'
method: JadeServer64bit
_trimStackOf: aGsProcess toLevel: anInteger

	^[
		super
			_trimStackOf: aGsProcess 
			toLevel: anInteger.
	] on: Error do: [:ex | 
		self 
			_trimStackOf: aGsProcess 
			toLevel: anInteger - 1.
		ex return.
	].

%

! Class implementation for 'JadeServer64bit24'

!		Instance methods for 'JadeServer64bit24'

category: 'category'
method: JadeServer64bit24
inspect: anObject

	^(self isClientForwarder: anObject)
		ifTrue: [self inspectClientForwarder: anObject]
		ifFalse: [super inspect: anObject].

%

category: 'category'
method: JadeServer64bit24
inspectClientForwarder: anObject

	| stream |
	(stream := WriteStream on: String new)
		nextPutAll: 'ClientForwarder'; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf;
		nextPut: $1; lf;
		nextPutAll: 'clientObject'; tab;
		yourself.
	self print: (self oopOf: anObject clientObject) on: stream.
	stream lf; nextPutAll: (self printStringOf: anObject).
	^stream contents.

%

category: 'category'
method: JadeServer64bit24
isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder' 

%

category: 'category'
method: JadeServer64bit24
oopOf: anObject

	^Reflection oopOf: anObject.

%

category: 'category'
method: JadeServer64bit24
printStringOf: anObject

	^(self isClientForwarder: anObject)
		ifFalse: [anObject printString]
		ifTrue: ['aClientForwarder(' , anObject clientObject printString , ')'].

%

category: 'category'
method: JadeServer64bit24
registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

	super
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
	platform 
		registerMultipleChoiceClientForwarder: clientForwarder;
		yourself.

%

! Class implementation for 'JadeServer64bit3x'

!		Instance methods for 'JadeServer64bit3x'

category: 'category'
method: JadeServer64bit3x
addMethodCategoryNamesToMethodFilters

	classList do: [:each | 
		each 
			env: environment 
			categorysDo:[ :categName :selectors | methodFilters add: categName ].
	].

%

category: 'category'
method: JadeServer64bit3x
addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler

	| instVarNumber modeInfo modeInfo_forked modeInfo_terminated |
	super addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler.
	(instVarNumber := GsProcess instVarNames indexOf: #'modeInfo') == 0 ifTrue: [^self].
	modeInfo := aProcess instVarAt: instVarNumber.
	(modeInfo_forked := GsProcess _classVars at: #'ModeInfo_forked' ifAbsent: [nil]) ifNil: [^self].
	(modeInfo_terminated := GsProcess _classVars at: #'ModeInfo_terminated' ifAbsent: [nil]) ifNil: [^self].
	aStream
"9"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_forked) ifTrue: ['forked'] ifFalse: ['main']); tab;
"10"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_terminated) ifTrue: ['terminated'] ifFalse: ['']); tab;
		yourself.

%

category: 'category'
method: JadeServer64bit3x
asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self asString: anObject clientObject) , ')'].
	^[
		anObject printString.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<printString error: ' , ex description , '>>'.
	].

%

category: 'category'
method: JadeServer64bit3x
categoryOfMethod: aMethod

	| category selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	category := self _behavior: aMethod inClass categoryOfSelector: aMethod selector.
	category ifNil: [category := #'other'].
	^category.

%

category: 'category'
method: JadeServer64bit3x
class: aClass includesSelector: aSelector

	^aClass includesSelector: aSelector asSymbol environmentId: environment.

%

category: 'category'
method: JadeServer64bit3x
compiledMethodAt: aSymbol inClass: aClass

	| method | 
	method := aClass compiledMethodAt: aSymbol environmentId: environment.
	method ifNil: [self error: 'Lookup failed for selector ' , aSymbol , ' inClass ' , aClass name , ' in environment ' , environment printString].
	^method.
%

category: 'category'
method: JadeServer64bit3x
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings | 

	[[ 
			[[ method := aBehavior rwCompileMethod: methodString category: categorySymbol ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
						on: RwPerformingUnpackagedEditNotification
						do: [:ex | ex resume ].
		] on: CompileError do: [:ex |
		^nil -> (ex gsArguments at: 1)
	]] on: CompileWarning do: [:ex |
		warnings := ex gsArguments at: 1.
		ex resume.
	].
	^[	
		(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings.
	] on: Error do: [:ex | 
		ex return: method -> warnings.
	].
%

category: 'category'
method: JadeServer64bit3x
debugString: aString fromContext: anObject environment: anInteger

	[
		^super debugString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].

%

category: 'category'
method: JadeServer64bit3x
describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super describeMethod: aMethod
%

category: 'category'
method: JadeServer64bit3x
environment

	^environment
%

category: 'category'
method: JadeServer64bit3x
environment: anInteger

	environment := anInteger.

%

category: 'category'
method: JadeServer64bit3x
environmentForMethod: aGsNMethod

	^aGsNMethod environmentId
%

category: 'category'
method: JadeServer64bit3x
executeString: aString fromContext: anObject environment: anInteger

	[
		^super executeString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].

%

category: 'category'
method: JadeServer64bit3x
homeMethodFor: aGsMethod

	| result |
	result := super homeMethodFor: aGsMethod.
	(result class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , result class name].
	^result
%

category: 'category'
method: JadeServer64bit3x
initialize

	super initialize.
	environment := 0.

%

category: 'category'
method: JadeServer64bit3x
inspect: anObject
	| dynamic dynamicSize indexedSize instVarNames namedSize stream string isRcBag |
	(self isClientForwarder: anObject) ifTrue: [^self inspectClientForwarder: anObject].
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	instVarNames := anObject class allInstVarNames.
	namedSize := instVarNames size.
	dynamic := anObject dynamicInstanceVariables.
	dynamicSize := dynamic size.
	isRcBag := anObject class name == #RcIdentityBag.
	indexedSize := (anObject class isNsc or: [anObject class isIndexable]) ifFalse: [
		0.
	] ifTrue: [
		isRcBag ifTrue: [ anObject size] ifFalse: [(anObject _primitiveSize - namedSize)].
	].
	
	namedSize + dynamicSize + indexedSize printOn: stream.
	stream lf.
	1 to: instVarNames size do: [:i | 
		stream nextPutAll: (instVarNames at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: stream.
		stream lf.
	].
	1 to: dynamicSize do: [:i | 
		stream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: stream.
		stream lf.
	].
	isRcBag
		ifTrue: [ |aBag |
			aBag := anObject _asIdentityBag.
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (aBag _at: i )) on: stream.
				stream lf]]
		ifFalse: [
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (anObject _primitiveAt: i + namedSize)) on: stream.
				stream lf] ].

	(string := anObject printString) size > 100000 ifTrue: [string := (string copyFrom: 1 to: 100000) , '...'].
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each codePoint and: [each codePoint <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	^stream 
		nextPutAll: string; 
		contents.
%

category: 'category'
method: JadeServer64bit3x
inspectNamedInstanceVariablesOf: anObject on: aStream

	| list dynamic size |
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 200).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: aStream.
		aStream lf.
	].
	1 to: dynamic size do: [:i | 
		aStream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: aStream.
		aStream lf.
	].

%

category: 'transcript'
method: JadeServer64bit3x
installTranscript

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: self.
%

category: 'category'
method: JadeServer64bit3x
methodSignatureForSelector: aSymbol
	"Ruby bridge methods can have some strange selectors!"

	| class comma i j method source |
	environment ~~ 1 ifTrue: [^aSymbol].
	class := selectedClass whichClassIncludesSelector: aSymbol environmentId: environment.
	method := class compiledMethodAt: aSymbol environmentId: environment.
	source := (method sourceString subStrings: Character lf) first trimBlanks.
	(4 < source size and: [(source copyFrom: 1 to: 4) = 'def ']) ifTrue: [
		source := source copyFrom: 5 to: source size.
		(source includes: $#) ifTrue: [source := (source copyFrom: 1 to: (source indexOf: $#) - 1) trimBlanks].
		^source.
	].
	(i := aSymbol indexOf: $#) == 0 ifTrue: [^aSymbol].
	source := aSymbol copyFrom: 1 to: i - 1.
	(aSymbol copyFrom: i to: aSymbol size) = '#0__' ifTrue: [^source].
	comma := ''.
	source add: $(.
	j := (aSymbol at: i + 1) asString asNumber.
	1 to: j do: [:k | 
		source 
			add: comma;
			add: 'arg'.
		1 < j ifTrue: [source add: k printString].
		comma := $,.
	].
	(aSymbol at: i + 2) == $* ifTrue: [
		source 
			add: comma;
			add: (0 == j ifTrue: ['args'] ifFalse: ['rest']).
		comma := $,.
	].
	aSymbol last == $& ifTrue: [
		source
			add: comma;
			add: '&block'.
	].
	source add: $).
	^source.

%

category: 'category'
method: JadeServer64bit3x
nameOfFirstDictionaryReferencing: aGlobal

	| list |
	list := self symbolList dictionariesAndSymbolsOf: aGlobal.
	list isEmpty ifTrue: [^''].
	^list first first name
%

category: 'category'
method: JadeServer64bit3x
nextPutAll: anObject

	| exception |
	exception := ClientForwarderSend new 
		receiver: self 
		clientObj: 2
		selector:#'nextPutAll:'
		args: (Array with: (self asString: anObject)).
	exception defaultAction.  "return error direct to GCI"
%

category: 'category'
method: JadeServer64bit3x
objectSecurityPolicyFor: anObject

	^anObject objectSecurityPolicy.

%

category: 'category'
method: JadeServer64bit3x
packagePolicy: aPackagePolicy includesSelector: aSymbol forClass: aClass

	| dict |
	^aPackagePolicy notNil and: [
		(dict := aClass transientMethodDictForEnv: environment) notNil and: [
			dict keys includes: aSymbol.		"includesKey: requires protected mode!"
		].
	].

%

category: 'category'
method: JadeServer64bit3x
sbBrowseMethodsWithPragma: anOrderedCollection

	| methods symbol |
	symbol := anOrderedCollection removeFirst asSymbol.
	methods := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal class with: eachGlobal) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						(eachMethod pragmas anySatisfy: [:eachPragma | symbol == eachPragma keyword]) ifTrue: [methods add: eachMethod].
					].
				].
			].
		].
	].
	writeStream 
		nextPutAll: 'browseMethodsWithPragma'; lf;
		nextPutAll: (self streamOfMethods: methods);
		yourself.

%

category: 'category'
method: JadeServer64bit3x
sbClassComment: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) thisClass rwComment: self sbNextParagraph trimSeparators.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer64bit3x
sbFileOutMethod: anOrderedCollection

	| aClass |
	aClass := self sbClassFrom: anOrderedCollection.
	writeStream nextPutAll: (aClass fileOutMethod: anOrderedCollection removeFirst asSymbol environmentId: environment).


%

category: 'category'
method: JadeServer64bit3x
sbListMethodPragmas: aList

	| pragmas |
	pragmas := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal with: eachGlobal class) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						pragmas addAll: (eachMethod pragmas collect: [:eachPragma | eachPragma keyword]).
					].
				].
			].
		].
	].
	pragmas asSortedCollection do: [:each | writeStream nextPutAll: each; tab].
	^pragmas
%

category: 'category'
method: JadeServer64bit3x
sbMethod: anOrderedCollection

	| behavior category string gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	gsMethod := behavior
		compileMethod: string 
		dictionaries: self symbolList 
		category: category asSymbol 
		environmentId: environment.
	selections 
		at: #'methodCategory' 	put: (self _behavior: gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'			put: gsMethod selector asString;
		yourself.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodBreakPointsFor: aMethod
	"Answers an Array of step points"

	| list array |
	(array := aMethod _allBreakpoints) isNil ifTrue: [^#()].      "{ breakpointNumber1 . method . ipOffset1 . ... }"
	list := Array new.
	1 to: array size by: 3 do:[:k |
		list add: (aMethod
			_stepPointForMeth: (array at: k + 1)
			ip: (array at: k + 2)).
	].
	^list.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodPragmas

	selectedClass notNil ifTrue:[
		methodCommandResult selections add: selectedClass asString].
	methodCommandResult updateMethodPragmas.
	methodCommandResult writeMethodFiltersTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
		hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream. 


%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByCategories

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		(eachClass selectorsForEnvironment: environment) do: [:eachSelector |
			(methodFilters isEmpty or: [
				| category |
				category := eachClass categoryOfSelector: eachSelector environmentId: environment.
				(category isNil and: [ methodFilters includes: #'other']) or: [ methodFilters includes: category asSymbol]]) ifTrue: [
				| method |
				method := eachClass compiledMethodAt: eachSelector environmentId: environment.
				(method respondsTo: #isRubyBridgeMethod)
					ifTrue: [ 
						method isRubyBridgeMethod ifFalse: [
							selectors add: eachSelector ] ]
					ifFalse: [ selectors add: eachSelector ]
			].
		].
	].
	^selectors.
%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByPragmas

	| selectors |
	selectors := IdentitySet new.
	methodFilters isEmpty ifTrue: [^#()].
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		gsMethod pragmas do: [:eachPragma | 
			( methodFilters includes: eachPragma keyword) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByVariables

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodStepPointsFor: aMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aMethod _allDebugInfo: 10) ifNil: [^#()].
	list := (self homeMethodFor: aMethod)  _sourceOffsets.
	list := list collect: [:each | 		"exists as far back as 32-bit 6.3.0"
		| index selector |
		selector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [selector := selectors at: index + 1].
		each -> selector.
	].
	^list.


%

category: 'category'
method: JadeServer64bit3x
selectedClassOverridesSelector: aSymbol

	^selectedClass superclass notNil and: [(selectedClass superclass whichClassIncludesSelector: aSymbol environmentId: environment) ~~ nil].

%

category: 'transcript'
method: JadeServer64bit3x
uninstallTranscript

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: nil.
%

category: 'category'
method: JadeServer64bit3x
_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: (each selectorsForEnvironment: environment); addAll: (each class selectorsForEnvironment: environment).
	].
	^allSelectors
%

category: 'category'
method: JadeServer64bit3x
_behavior: aBehavior categoryOfSelector: aSymbol

	^aBehavior categoryOfSelector: aSymbol environmentId: environment
%

category: 'category'
method: JadeServer64bit3x
_describeMethod: aMethod

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super _describeMethod: aMethod
%

category: 'category'
method: JadeServer64bit3x
_methodsFor: aClass categories: aList

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (self compiledMethodAt: eachSelector inClass: aClass).
			].
		].
	].
	^methods.


%

category: 'category'
method: JadeServer64bit3x
_stepPointsForBreaksInMethod: gsMethod
	"Presumably there is a way to do this, just not the same as 32-bit and 64-bit 2.x"

	^#()
%

! Class implementation for 'JadeServer64bit32'

!		Instance methods for 'JadeServer64bit32'

category: 'category'
method: JadeServer64bit32
dictionaryAndSymbolOf: aClass

	| array |
	array := self symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].

%

category: 'category'
method: JadeServer64bit32
dictionaryAndSymbolOf: aClass forUser: aUserProfile

	| array |
	array := aUserProfile symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].

%

category: 'category'
method: JadeServer64bit32
gsPackagePolicy

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	class enabled ifFalse: [^nil].
	^class current

%

category: 'category'
method: JadeServer64bit32
stepThrough: aGsProcess inFrame: anInteger
  aGsProcess _stepThrough
%

! Class implementation for 'JadeServer64bit35'

!		Instance methods for 'JadeServer64bit35'

category: 'other'
method: JadeServer64bit35
stepThrough: aGsProcess inFrame: anInteger
  aGsProcess stepThroughFromLevel: anInteger
%

! Class implementation for 'RowanCommandResult'

!		Class methods for 'RowanCommandResult'

category: 'accessing'
classmethod: RowanCommandResult
addResult: service
	service command: nil;
			commandArgs: nil. 
	self updateClientBoundServices: service.
	^service
%

category: 'accessing'
classmethod: RowanCommandResult
basicAddResult: service
	self results add: service
%

category: 'initailize'
classmethod: RowanCommandResult
initializeResults

	SessionTemps current at: #rowanCommandResults put: Array new.
%

category: 'instance creation'
classmethod: RowanCommandResult
new

	| inst |
	inst := super new initialize.
	self addResult: inst.
	^inst
%

category: 'accessing'
classmethod: RowanCommandResult
removeResult: aResult

	self results remove: aResult
%

category: 'accessing'
classmethod: RowanCommandResult
results

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanCommandResults ifAbsentPut: [Array new]
%

category: 'private'
classmethod: RowanCommandResult
updateClientBoundServices: clientBoundService
	"We're about to add a service to the results collection. 
	That service will be sent to the client. Since services
	are not canonical, we need to do some housekeeping
	to ensure that we don't already have this service 
	somewhere in other client-bound services"
	(self results includes: clientBoundService) ifTrue:[
		self removeResult: clientBoundService].
	self basicAddResult: clientBoundService. 
	self results do:[:service |
		service updateInternalService: clientBoundService.
		clientBoundService updateInternalService: service].
%

!		Instance methods for 'RowanCommandResult'

category: 'accessing'
method: RowanCommandResult
command: anObject

	"results don't have commands"
%

category: 'accessing'
method: RowanCommandResult
commandArgs: anObject

	"results don't have commandArgs"
%

category: 'initialization'
method: RowanCommandResult
initialize
%

category: 'testing'
method: RowanCommandResult
isMethodService

	^false
%

category: 'private'
method: RowanCommandResult
rowanFixMe
%

category: 'accessing'
method: RowanCommandResult
updateInternalService: service
%

! Class implementation for 'RowanService'

!		Class methods for 'RowanService'

category: 'autocommit'
classmethod: RowanService
autoCommit

	^SessionTemps current at: #'Jadeite_AutoCommit' ifAbsentPut: [false]
%

category: 'autocommit'
classmethod: RowanService
breakPointsAreEnabled

	^SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' ifAbsentPut: [true]
%

category: 'autocommit'
classmethod: RowanService
flipAutoCommit
	| newValue |
	newValue := self autoCommit == #failed ifTrue:[false] ifFalse:[self autoCommit not].
	^self setAutoCommit: newValue
%

category: 'instance creation'
classmethod: RowanService
new

	^super new initialize
%

category: 'other'
classmethod: RowanService
rowanFixMe

	"send this message to see everywhere that GS_Jade should be fixed"
%

category: 'examples'
classmethod: RowanService
sampleService

	^self new sampleService
%

category: 'autocommit'
classmethod: RowanService
setAutoCommit: object

	^SessionTemps current at: #'Jadeite_AutoCommit' put: object
%

category: 'autocommit'
classmethod: RowanService
setBreakPointsAreEnabled: boolean
  ^ SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' put: boolean
%

category: 'accessing'
classmethod: RowanService
version
  "change this method carefully and only at Jadeite release boundaries.
	Failure to do so will prevent logins"

  ^ 3092
%

!		Instance methods for 'RowanService'

category: 'other'
method: RowanService
answer: anObject

	| answeringService |
	answeringService := RowanAnsweringService new. 
	answeringService answer: anObject. 
	RowanCommandResult addResult: answeringService.
%

category: 'rowan'
method: RowanService
browserTool

	^self projectTools browser
%

category: 'commands support'
method: RowanService
classHierarchy: theClasses
  | superclassChains levels services hierarchies toExpand hierarchyServices |
  superclassChains := self superclassChainsFor: theClasses.
  hierarchies := self extendHierarchies: superclassChains.
  levels := self hierarchiesByLevel: hierarchies.
  services := Dictionary new.
  toExpand := Set new.
  self
    services: services
    from: levels
    expand: toExpand
    classes: theClasses.
  hierarchyServices := services reject: [ :array | array isEmpty ].
  hierarchyServices copy
    keysAndValuesDo: [ :key :value | 
      hierarchyServices
        at: key
        put:
          (value asSet asSortedCollection: [ :x :y | x name < y name ]) asArray ].
  ^ hierarchyServices
%

category: 'accessing'
method: RowanService
command

	^command
%

category: 'accessing'
method: RowanService
command: aSymbol

	command := aSymbol
%

category: 'accessing'
method: RowanService
commandArgs

	^commandArgs ifNil:[commandArgs := Array new]
%

category: 'accessing'
method: RowanService
commandArgs: anArray

	"for tests" 

	commandArgs := anArray
%

category: 'symbol dictionaries'
method: RowanService
createDefaultSymbolDictionary

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName
%

category: 'samples'
method: RowanService
createSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName
%

category: 'symbol dictionaries'
method: RowanService
createSymbolDictionaryNamed: aName

	| dictionary size |
	dictionary := SymbolDictionary new.
	dictionary at: aName asSymbol put: dictionary.
	size := Rowan image symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^ dictionary
%

category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionary

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName
%

category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionaryName

	^'RowanProjects'
%

category: 'rowan'
method: RowanService
definitionClass

	^self subclassResponsibility
%

category: 'rowan'
method: RowanService
definitionClassName

	^self definitionClass name
%

category: 'replication'
method: RowanService
excludedInstVars

	^#( #organizer)
%

category: 'commands support'
method: RowanService
extendHierarchies: hierarchies
	
	"extend the hierarchies by one level
	of subclasses"

	| extendedHierarchies |
	extendedHierarchies := Array new. 
	hierarchies do:[:hierarchy |
		| theClass subclasses |
		theClass := hierarchy last. 
		(subclasses := organizer subclassesOf: theClass) isEmpty 
			ifTrue:[extendedHierarchies add: hierarchy]
			ifFalse:[
				subclasses do:[:sub |
					extendedHierarchies add: (hierarchy copy add: sub; yourself)
				]]].
	^extendedHierarchies
%

category: 'perform'
method: RowanService
handleDeletedService
  self updateType: #'removed:'.
  RowanCommandResult addResult: self
%

category: 'commands support'
method: RowanService
hierarchiesByLevel: hierarchies

	"Return dictionary of classes by level. 
	Example: 
		hierarchies - #(#(Object Collection Array) #(Object AbstractException Exception))
	Return: 
		#(#nil->#(Object) Object->#(Collection AbstractException) Collection->#(Array) AbstractException->#(Exception)
	"
	| levels |
	levels := hierarchies inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls theSuper classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			((dict at: #'nil') includes: cls) ifFalse:[(dict at: #'nil') add: cls].
		]
		ifFalse:[
				theSuper := chain at: index - 1.
				((dict at: theSuper) includes: cls) ifFalse:[(dict at: theSuper) add: cls]
				]].
			dict].
	^levels
%

category: 'initialization'
method: RowanService
initialize
%

category: 'testing'
method: RowanService
isClassService

	^false
%

category: 'testing'
method: RowanService
isDictionaryService

	^false
%

category: 'testing'
method: RowanService
isMethodService

	^false
%

category: 'testing'
method: RowanService
isPackageService

	^false
%

category: 'testing'
method: RowanService
isProjectService

	^false
%

category: 'testing'
method: RowanService
isUpdating

	^command == #update
%

category: 'perform'
method: RowanService
isUpdatingButFoundToBeDeleted
  ^ self command == #'update' and: [ self wasDeleted ]
%

category: 'accessing'
method: RowanService
jadeiteServer

	^(Rowan jadeServerClassNamed: #JadeServer) theJadeiteServer
%

category: 'accessing'
method: RowanService
organizer: anOrganizer

	organizer := anOrganizer.
%

category: 'rowan'
method: RowanService
projectTools

	^Rowan projectTools
%

category: 'samples'
method: RowanService
removeSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
%

category: 'symbol dictionaries'
method: RowanService
removeSymbolDictionaryNamed: aName

	| index |
	index := Rowan image symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
%

category: 'other'
method: RowanService
rowanFixMe
		
	"marker for all things broken in Rowan"
%

category: 'rowan'
method: RowanService
rowanLoadedPackageNames

	| stream packages |
	self rowanFixMe.	"handle modified package display"
	stream := WriteStream on: String new.
	packages := Rowan packageNames.
	packages do: 
					[:package |
					stream
						nextPutAll: package;
						tab;
						nextPut: ((RwPackage newNamed: package) isDirty ifTrue:[$Y] ifFalse:[$N]);
						tab;
						nextPutAll: package;
						lf].
	^stream contents
%

category: 'accessing'
method: RowanService
rowanProjectName

	"all services should be able to return a project name
	even if they are not truly packaged" 

	^nil
%

category: 'samples'
method: RowanService
sampleSymbolDictionaryName

	^'SampleSymbolDictionaryName'
%

category: 'perform'
method: RowanService
servicePerform: symbol withArguments: collection
  "each service updates itself after performing a command.
	Therefore, if the command is #update, don't run it here"

  SessionTemps current
    at: #'versionsVerified'
    ifAbsent: [ 
      SessionTemps current at: #'versionsVerified' put: false.
      self
        inform:
          'Version mismatch failure. Client version is older than server version.' ].
  symbol == #'update'
    ifTrue: [ ^ self ].
  ^ super perform: symbol withArguments: collection
%

category: 'commands support'
method: RowanService
services: services from: levels expand: toExpand classes: theClasses
  "In order to avoid the expense of creating duplicate services, we cache
them in the newServices temporary for look up"

  | newServices |
  newServices := Array new.
  theClasses
    do: [ :aClass | toExpand addAll: (organizer allSuperclassesOf: aClass) ].
  levels
    keysAndValuesDo: [ :key :value | 
      | newKey service |
      newKey := key = #'nil'
        ifTrue: [ #'nil' ]
        ifFalse: [ 
          service := newServices
            detect: [ :classService | classService name = key name ]
            ifNone: [ RowanClassService new classServiceFromOop: key asOop ].
          (toExpand includes: service theClass)
            ifTrue: [ service expand: true ]
            ifFalse: [ service expand: false ].
          service ].
      services
        at: newKey
        put:
          (value
            collect: [ :cls | 
              service := newServices
                detect: [ :classService | classService name = cls name ]
                ifNone: [ RowanClassService new classServiceFromOop: cls asOop ].
              (toExpand includes: service theClass)
                ifTrue: [ service expand: true ]
                ifFalse: [ service expand: false ].
              service ]) ]
%

category: 'replication'
method: RowanService
stonOn: stonWriter
    | instanceVariableNames |
    instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
    stonWriter writeObject: self
        streamMap: 
            [:dictionary |
            instanceVariableNames do: 
                    [:each |
                    (self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
                        ifNotNil: [:value | dictionary at: each asSymbol put: value]
                        ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
%

category: 'replication'
method: RowanService
stonStringFor: anObject

	"return a string representing a complete object structure
	suitable for replicating on the client."

	^STON toString: anObject
%

category: 'private'
method: RowanService
stripOutUnicode: string
  | asciiString |
  asciiString := string
    collect: [ :char | 
      ((self validLowRangeCharacters includes: char) not
        and: [ char asciiValue < 32 or: [ char asciiValue > 255 ] ])
        ifTrue: [ $? ]
        ifFalse: [ char ] ].
  ^ asciiString
%

category: 'commands support'
method: RowanService
superclassChainsFor: behaviors
	organizer := ClassOrganizer new. 
	^behaviors collect:[:behavior | | supers |
			supers := organizer allSuperclassesOf: behavior. 
			supers add: behavior. 
			supers].
%

category: 'symbol dictionaries'
method: RowanService
symbolDictionaryNamed: aName

	| symbolList  index |
	symbolList := Rowan image symbolList.
	index :=symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].
%

category: 'initialization'
method: RowanService
update
%

category: 'update'
method: RowanService
updateInternalService: updatedService

	"no internally held services to update"
%

category: 'update'
method: RowanService
updateLatest
  "subclasses may want to special behavior to update themselves
	to their loaded version"

  self update
%

category: 'accessing'
method: RowanService
updateType: aSymbol

	updateType := aSymbol
%

category: 'accessing'
method: RowanService
userGlobals
  ^ Rowan image symbolList objectNamed: #'UserGlobals'
%

category: 'private'
method: RowanService
validLowRangeCharacters
  ^ Array with: Character lf with: Character tab
%

category: 'testing'
method: RowanService
wasDeleted

	^false
%

category: 'fileout'
method: RowanService
writeFileOutHeaderOn: stream
	"This method will write a fileout header onto the given file.
	Adapted from GBS - GbxBrowser>>writeFileOutHeaderOn:"

	| rawVer beVer cr |
	stream nextPutAll: 'fileformat utf8';
			cr. 
	rawVer := System _version.
	beVer := ''.
	cr := String with: Character cr.
	"Comment each newline"
	(rawVer subStrings: (Array with: Character lf)) do: [:line | beVer := beVer , '! ' , line , cr].
	stream
		nextPutAll: '!';
		cr;
		nextPutAll: '! From ';
		nextPutAll: beVer;
		cr;
		nextPutAll: '! On ';
		nextPutAll: Date today printString;
		nextPutAll: ', ';
		nextPutAll: Time now printString;
		cr;
		nextPutAll: '!';
		cr;
		flush
%

! Class implementation for 'RowanAnsweringService'

!		Instance methods for 'RowanAnsweringService'

category: 'private'
method: RowanAnsweringService
addLowerCaseSymbolsIn: theClass To: array
  array addAll: theClass selectors.
  array addAll: theClass class selectors.
  array addAll: theClass instVarNames.
  array addAll: theClass class instVarNames.
  array addAll: theClass classVarNames.
%

category: 'client commands'
method: RowanAnsweringService
allClassesStartingWith: string
  answer := SortedCollection new.
  organizer classes
    do: [ :cls | 
      (cls name beginsWith: string)
        ifTrue: [ answer add: cls name asString ] ].
  answer := answer asArray.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
allClassNames
  answer := SortedCollection new.
  answer addAll: (organizer classes collect: [ :cls | cls name asString ]).
  answer := answer asArray.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
allTestsIn: classServices
	answer := Array new. 
	classServices do:[:service | answer addAll: service allTests].
	RowanCommandResult addResult: self.
%

category: 'accessing'
method: RowanAnsweringService
answer

	^answer
%

category: 'Updating'
method: RowanAnsweringService
answer: anObject

	answer := anObject
%

category: 'client commands'
method: RowanAnsweringService
autoCommit

	answer := RowanService autoCommit. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
autocompleteSymbols
  | newClassNames newLowerCaseSymbols |
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  organizer classes
    do: [ :cls | 
      newClassNames add: cls name asString.
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  SessionTemps current
    at: #'autocompleteSymbolCache'
    put: (Array with: newClassNames with: newLowerCaseSymbols).
  answer := Array
    with: newClassNames asOrderedCollection
    with: newLowerCaseSymbols asOrderedCollection.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
%

category: 'client command support'
method: RowanAnsweringService
basicExec: aString context: oop
	answer := [true -> (aString evaluateInContext: (Object _objectForOop: oop) symbolList: Rowan image symbolList) asOop] 
		on: CompileError do: [:ex | 
			false -> ex errorDetails
	].
	answer key ifTrue:[(RowanService autoCommit == true) ifTrue:[System commitTransaction]].
	^answer
%

category: 'client command support'
method: RowanAnsweringService
basicMethodHistoryFor: methodService
  | rowanMethodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  answer := (rowanMethodHistory at: methodService ifAbsentPut: [ Array new ])
    asOrderedCollection.
  answer
    addFirst: (rowanMethodHistory keys detect: [ :svc | svc = methodService ])
%

category: 'client command support'
method: RowanAnsweringService
basicPrintStringOf: oop toMaxSize: integer
  | object |
  object := Object _objectForOop: oop.
  ^ self basicPrintStringOfObject: object toMaxSize: integer
%

category: 'client command support'
method: RowanAnsweringService
basicPrintStringOfObject: object toMaxSize: integer
  "avoid the oop conversion when we already have the object"

  | printString |
  printString := self stripOutUnicode: object printString.
  printString := printString size > integer
    ifTrue: [ (printString copyFrom: 1 to: integer) , '...' ]
    ifFalse: [ printString ].
  ^ printString
%

category: 'client command support'
method: RowanAnsweringService
basicSortedSelectors
  | selectors |
  selectors := IdentitySet new.
  organizer classes
    do: [ :aClass | 
      | metaClass |
      metaClass := aClass.
      2
        timesRepeat: [ 
          | methodDictionary |
          methodDictionary := metaClass _fullMethodDictEnv0.
          methodDictionary
            valuesDo: [ :method | 
              | selector |
              selector := method selector.
              selector charSize = 1
                ifTrue: [ selectors add: selector ].
              method _selectorPool
                do: [ :sentSelector | 
                  sentSelector charSize = 1
                    ifTrue: [ selectors add: sentSelector ] ] ].
          metaClass := metaClass class ] ].
  ^ selectors asSortedCollection asArray
%

category: 'client command support'
method: RowanAnsweringService
basicSortedSymbols
  | sortedSymbols |
  sortedSymbols := SortedCollection new.
  ((AllUsers userWithId: #'SymbolUser') resolveSymbol: #'AllSymbols') value
    keysDo: [ :symbol | 
      symbol charSize = 1
        ifTrue: [ sortedSymbols add: symbol ] ].
  ^sortedSymbols asArray
%

category: 'client commands'
method: RowanAnsweringService
breakPointsAreEnabled
  answer := RowanService breakPointsAreEnabled.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
canAccessServiceClasses
  "Being able to see RowanLoggingService is a good
	indication that all of the service classes are visible"

  answer := [ '[RowanLoggingService] value. true.' evaluate ]
    on: Error
    do: [ :ex | false ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
classHasSubclasses: oop
  | behavior |
  behavior := Object _objectForOop: oop.
  answer := behavior subclasses notEmpty.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
clearAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod clearAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
clearMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        clearMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
disableAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod _disableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
disableMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        disableMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
doClientAndServerVersionsMatch: clientVersion
  "Not to be sent through services so return an answer directly.
	Sent immediately after Jadeite login"

  SessionTemps current at: #'versionsVerified' put: false.
  clientVersion = RowanService version
    ifTrue: [ 
      answer := true.
      SessionTemps current at: #'versionsVerified' put: true ]
    ifFalse: [ 
      answer := clientVersion > RowanService version
        ifTrue: [ 
          'Client (' , clientVersion printString , ') is more recent than server ('
            , RowanService version printString , ')' ]
        ifFalse: [ 
          'Server (' , RowanService version printString , ') is more recent than client ('
            , clientVersion printString , ')' ] ].
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
enableAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: organizer;
    basicBreakpointMethods.
  GsNMethod _enableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
enableMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: organizer;
        enableMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
exec: aString

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
exec: aString context: oop

	answer := self basicExec: aString context: oop. 
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
exec: aString context: oop inWindow: handle
  answer := self exec: aString context: oop.
  answer key
    ifTrue: [ RowanBrowserService new saveRootObject: answer value windowHandle: handle ].	"return answer for testing"
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
exec: aString inFrame: level ofProcess: processOop context: oop
  | symbolList frameContents symbolDictionary process |
  symbolList := Rowan image symbolList.
  process := Object _objectForOop: processOop.
  process _isTerminated
    ifTrue: [ 
      RowanCommandResult addResult: self.
      ^ self ].
  frameContents := process _frameContentsAt: level.
  frameContents
    ifNotNil: [ 
      symbolDictionary := SymbolDictionary new.
      1 to: (frameContents at: 9) size do: [ :index | 
        ((frameContents at: 9) at: index) first = $.
          ifFalse: [ 
            symbolDictionary
              at: ((frameContents at: 9) at: index) asSymbol
              put: (frameContents at: 11 + index - 1) ] ].
      symbolList add: symbolDictionary before: symbolList first ].
  [ 
  answer := [ 
  true
    ->
      (aString evaluateInContext: (Object _objectForOop: oop) symbolList: symbolList)
        asOop ]
    on: CompileError
    do: [ :ex | false -> ex errorDetails ].
  answer key
    ifTrue: [ 
      RowanService autoCommit == true
        ifTrue: [ System commitTransaction ] ].
  RowanCommandResult addResult: self ]
    ensure: [ 
      1 to: (frameContents at: 9) size do: [ :index | 
        | argsAndTemps |
        argsAndTemps := frameContents at: 9.
        (argsAndTemps at: index) first = $.
          ifFalse: [ 
            | variableService |
            process
              _frameAt: level
              tempAt: index
              put: (symbolDictionary at: (argsAndTemps at: index)).
            variableService := RowanVariableService
              oop: (symbolDictionary at: (argsAndTemps at: index)) asOop
              key: (argsAndTemps at: index)
              value: (symbolDictionary at: (argsAndTemps at: index)) printString
              className: (frameContents at: 8) class name asString.
            RowanCommandResult addResult: variableService ] ].
      symbolList remove: symbolDictionary ].	"return answer for testing"
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
expressionSelector: string
  "try to uncover a selector in the string. 
	If it's not possible, return the string and 
	let the application do with it what it will"

  | messageNode |
  messageNode := [ RBParser parseExpression: string ]
    on: Error
    do: [ :ex | nil ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: string , ' #foo' ]
        on: Error
        do: [ :ex | nil ] ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: '#foo ' , string ]
        on: Error
        do: [ :ex | nil ] ].
  answer := messageNode
    ifNil: [ string ]
    ifNotNil: [ 
      messageNode isMessage
        ifTrue: [ messageNode buildSelector ]
        ifFalse: [ 
          messageNode isCascade
            ifTrue: [ messageNode messages first buildSelector]
            ifFalse: [ string ] ] ].
  RowanCommandResult addResult: self.
  ^ answer	"return answer for testing"
%

category: 'client commands'
method: RowanAnsweringService
flipTranscript
	self isTranscriptInstalled ifTrue:[
		self jadeiteServer uninstallTranscript]
	ifFalse:[
		self jadeiteServer installTranscript]
%

category: 'client commands'
method: RowanAnsweringService
initializeAutoCommit

	RowanService setAutoCommit: false
%

category: 'client commands'
method: RowanAnsweringService
initializeBreakPointsAreEnabled
  RowanService setBreakPointsAreEnabled: true
%

category: 'client commands'
method: RowanAnsweringService
interactionHandlerActive
  answer := SessionTemps current
    at: #'rowanServiceInteractionActive'
    ifAbsent: [ true ].
  RowanCommandResult addResult: self
%

category: 'testing'
method: RowanAnsweringService
isTranscriptInstalled

	^self transcriptObject == self jadeiteServer
%

category: 'client commands'
method: RowanAnsweringService
isVariable: oop
	| anObject |
	anObject := Object _objectForOop: oop. 
	answer := anObject class isVariable.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
loadedPackageExists: packageName
	
	| actualName |
	actualName := Rowan image packageNames detect:[:loadedName | loadedName asLowercase = packageName asLowercase] ifNone:[]. 
	answer := (Rowan image loadedPackageNamed: actualName ifAbsent: []) notNil. 
	command := nil. 
	commandArgs := nil. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
lowercaseSelectorsMatching: lowercaseSymbol
  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
methodHistoryFor: methodService
  self basicMethodHistoryFor: methodService.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
methodReferenceCounts: methodServices
  answer := Array new.
  methodServices
    do: [ :methodService | answer add: (organizer sendersOf: methodService selector) first size ].
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
needsCommit

	answer := System needsCommit.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
newPackageNamed: packageName
  | packageService |
  packageService := RowanPackageService new name: packageName.
  packageService update.
  answer := packageService.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
printStringOf: oop toMaxSize: integer
  answer := self basicPrintStringOf: oop toMaxSize: integer.
  RowanCommandResult addResult: self.
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
printStringOfOops: oops toMaxSize: integer
  answer := Array new.
  oops
    do: [ :oop | answer add: (self basicPrintStringOf: oop toMaxSize: integer) ].
  RowanCommandResult addResult: self.
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
profile: block
  "not used yet. Utility method needs testing.
	Make sure block execution time is long enough - say 1 second. 
	Results may not be consistent

	Usage example: 
| block answeringService | 
block := [| browserService profMonitor |
		browserService := (RowanBrowserService new) .
		10 timesRepeat:[browserService packagesWithTests]]. 
answeringService := RowanAnsweringService new profile: block.
answeringService answer. "

  | time ns |
  time := System millisecondsToRun: block.
  ns := ProfMonitor computeInterval: time / 1000.
  answer := ProfMonitor monitorBlock: block intervalNs: ns.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
removeMethodHistoryFor: methodService
  | rowanMethodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  rowanMethodHistory removeKey: methodService ifAbsent: [  ]
%

category: 'client commands'
method: RowanAnsweringService
resolveAsService: name
  | projectService packageService classService dictionaryService |
  projectService := RowanProjectService new name: name.
  projectService update projectIsLoaded
    ifTrue: [ 
      answer := projectService.
      ^ RowanCommandResult addResult: self ].
  packageService := RowanPackageService new name: name.
  packageService update projectName
    ifNotNil: [ 
      answer := packageService.
      ^ RowanCommandResult addResult: self ].
  dictionaryService := RowanDictionaryService new name: name.
  dictionaryService update classes notEmpty
    ifTrue: [ 
      answer := dictionaryService.
      ^ RowanCommandResult addResult: self ].
  classService := RowanClassService new name: name.
  classService update projectName
    ifNotNil: [ 
      answer := classService.
      ^ RowanCommandResult addResult: self ].
  answer := nil.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
runMethodTests: methodServices

	| behavior |
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior := methodService classFromName. 
			behavior debug: methodService selector]].
	answer := true. 
	RowanCommandResult initializeResults. "squash any client updates during server test run"
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
selectorsMatching: lowercaseSymbol
  "assume we're passed a lower case symbol to avoid
	case sensitive misses"

  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
selectorsMatchingPattern: pattern
  answer := self basicSortedSelectors.
  answer := answer select: [ :each | each _matchPatternNoCase: pattern ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
setAutoCommit: object

	answer := RowanService setAutoCommit: object.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
setBreakPointsAreEnabled: boolean
  boolean
    ifTrue: [ self enableAllBreaks ]
    ifFalse: [ self disableAllBreaks ].
  RowanService setBreakPointsAreEnabled: boolean.
  answer := boolean.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
setEnableInteractionHandler: boolean
  SessionTemps current at: #'rowanServiceInteractionActive' put: boolean
%

category: 'client commands'
method: RowanAnsweringService
sortedSelectors
  answer := self basicSortedSelectors.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
sortedSymbols
  answer := self basicSortedSymbols. 
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
subclassCreationTemplate: className
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
%

category: 'client commands'
method: RowanAnsweringService
symbolExists: aSymbol
  answer := (Rowan image symbolList resolveSymbol: aSymbol) isNil not.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
symbolsMatching: string
  answer := self basicSortedSymbols
    select: [ :symbol | string sunitMatch: symbol ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
transcriptInstalled

	answer := self isTranscriptInstalled.
	RowanCommandResult addResult: self.
%

category: 'private'
method: RowanAnsweringService
transcriptObject
	
	^(SessionTemps current  at: #'TranscriptStream_SessionStream')
%

category: 'client commands'
method: RowanAnsweringService
turnOffTranscriptWrites

	self isTranscriptInstalled ifTrue:[
		self flipTranscript]
%

category: 'client commands'
method: RowanAnsweringService
updateAutocompleteSymbols
  | cache newClassNames newLowerCaseSymbols |
  cache := SessionTemps current
    at: #'autocompleteSymbolCache'
    ifAbsent: [ ^ self	"autocomplete not activated" ].
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  organizer classes
    do: [ :cls | 
      (cache first includes: cls name asString)
        ifFalse: [ newClassNames add: cls name asString ].
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  (SessionTemps current at: #'autocompleteSymbolCache') first
    addAll: newClassNames.
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  cache last
    do: [ :selector | 
      (newLowerCaseSymbols includes: selector)
        ifTrue: [ newLowerCaseSymbols remove: selector ] ].
  (SessionTemps current at: #'autocompleteSymbolCache') last
    addAll: newLowerCaseSymbols.
  answer := Array with: newClassNames with: newLowerCaseSymbols.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
%

! Class implementation for 'RowanAutoCommitService'

!		Instance methods for 'RowanAutoCommitService'

category: 'client commands'
method: RowanAutoCommitService
autoCommit: boolean

	self class setAutoCommit: boolean.
	autoCommit := self class autoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAutoCommitService
flipAutoCommit

	autoCommit := self class flipAutoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
%

! Class implementation for 'RowanBrowserService'

!		Instance methods for 'RowanBrowserService'

category: 'client commands'
method: RowanBrowserService
abortTransaction
  | autoCommitService autoCommitState |
  autoCommitState := RowanService autoCommit == #'failed'
    ifTrue: [ true ]
    ifFalse: [ RowanService autoCommit ].
  System abortTransaction.
  autoCommitService := RowanAutoCommitService new.
  autoCommitService autoCommit: autoCommitState.
  self updateProjects.
  self updateDictionaries
%

category: 'client commands'
method: RowanBrowserService
abortTransactionAndUpdateServices: services
  self abortTransaction.
  services
    do: [ :service | 
      "we just updated projects, package, & dictionary services"
      (service isProjectService not
        and: [ service isDictionaryService not and: [ service isPackageService not ] ])
        ifTrue: [ 
          service
            organizer: organizer;
            updateLatest ] ]
%

category: 'client commands'
method: RowanBrowserService
allClasses
	allClasses := self basicAllClasses.
	updateType := #classes. "#classes not used at the moment so no updates will be done"
	RowanCommandResult addResult: self
%

category: 'client commands support'
method: RowanBrowserService
basicAllClasses
  | theClasses |
  theClasses := SortedCollection sortBlock: [ :x :y | x name < y name ].
  theClasses
    addAll:
      (organizer classes
        collect: [ :class | 
          | service |
          service := RowanClassService new name: class name.
          service packageName: class rowanPackageName.
          service projectName: class rowanProjectName.
          service ]).
  ^ theClasses asArray
%

category: 'client commands'
method: RowanBrowserService
classHierarchy
	| theClasses |
	theClasses := allClasses collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
%

category: 'private'
method: RowanBrowserService
classHierarchy: theClasses
  hierarchyServices := super classHierarchy: theClasses.
  ^ hierarchyServices
%

category: 'client commands'
method: RowanBrowserService
classHierarchyForDictionariesNamed: dictionaryNames
  | theClasses |
  theClasses := OrderedCollection new.
  dictionaryNames
    do: [ :dictionaryName | 
      (Rowan globalNamed: dictionaryName)
        keysAndValuesDo: [ :key :value | 
          value isClass
            ifTrue: [ theClasses add: value ] ] ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
classHierarchyForPackagesNamed: packageNames
  | theClasses |
  theClasses := OrderedCollection new.
  packageNames
    do: [ :packageName | 
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClasses 
            collect: [ :cls | cls handle]).
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClassExtensions
            collect: [ :cls | cls handle]) ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
compileClass: definitionString
  | anonymousMethod |
  self confirmDuplicateName: definitionString.
  anonymousMethod := definitionString
    _compileInContext: nil
    symbolList: Rowan image symbolList.
  SessionTemps current at: #'jadeiteCompileClassMethod' put: anonymousMethod
%

category: 'client commands support'
method: RowanBrowserService
confirmDuplicateName: definitionString
  | className |
  className := (definitionString subStrings at: 3) copyWithout: $'.
  (Rowan image symbolList resolveSymbol: className asSymbol)
    ifNotNil: [ 
      className = selectedClass
        ifFalse: [ 
          (self confirm: 'Class name is already an object. Continue?')
            ifFalse: [ ^ Error signal: 'Class not compiled. Name already exists.' ] ] ]
%

category: 'client commands'
method: RowanBrowserService
defaultClassHierarchy
	hierarchyServices := Dictionary new.   
	organizer hierarchy keysAndValuesDo: [:key :value |
		| classService |
		classService := key == #nil ifTrue:[#nil] ifFalse: [RowanClassService basicForClassNamed: key name].
		hierarchyServices at: classService put: (value collect:[:cls | RowanClassService basicForClassNamed: cls name]) asArray.
	].
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanBrowserService
findRemovedServices: services

	services do:[:service | 
		service wasDeleted ifTrue:[
				service updateType: #removed:.
				RowanCommandResult addResult: service.
		]
	].
%

category: 'initialize'
method: RowanBrowserService
initialize
  super initialize.
  newCachedSelectors := Array new.
  newCachedClasses := Array new
%

category: 'accessing'
method: RowanBrowserService
newCachedClasses
	^newCachedClasses
%

category: 'accessing'
method: RowanBrowserService
newCachedClasses: object
	newCachedClasses := object
%

category: 'accessing'
method: RowanBrowserService
newCachedSelectors
	^newCachedSelectors
%

category: 'accessing'
method: RowanBrowserService
newCachedSelectors: object
	newCachedSelectors := object
%

category: 'window registry'
method: RowanBrowserService
openWindows

	"for testing"

	^SessionTemps current at: #rowanServicesWindowRegistry ifAbsent:[]
%

category: 'client commands'
method: RowanBrowserService
packagesWithTests
  organizer := ClassOrganizer new.	"when we call this method, our world has changed from a reload, etc."
  testPackages := Set new.
  testCount := 0.
  testPackages := Set new.
  testCount := 0.
  (organizer allSubclassesOf: TestCase)
    do: [ :sub | 
      | packageName testMethodCount |
      testMethodCount := (sub sunitSelectors
        select: [ :each | each beginsWith: 'test' ]) size.	"sending #testSelectors was slower"
      testCount := testCount + testMethodCount.
      testMethodCount > 0
        ifTrue: [ 
          packageName := sub rowanPackageName.
          packageName = Rowan unpackagedName
            ifFalse: [ 
              testPackages
                add:
                  (RowanPackageService new
                    name: packageName;
                    updateProjectName;
                    yourself) ].
          (Rowan image loadedClassExtensionsForClass: sub)
            do: [ :loadedThing | 
              testPackages
                add:
                  (RowanPackageService new
                    name: loadedThing loadedPackage name;
                    updateProjectName;
                    yourself)	"don't update the entire package for performance improvement" ] ] ].
  updateType := #'testPackages:'.
  testPackages := testPackages asArray.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
recompileMethodsAfterClassCompilation
  "compileClass: must be run first"

  | theClass classService packageService projectService |
  theClass := [ 
  [ (SessionTemps current at: #'jadeiteCompileClassMethod') _executeInContext: nil ]
    on: CompileWarning
    do: [ :ex | ex resume ] ]
    ensure: [ SessionTemps current at: #'jadeiteCompileClassMethod' put: nil ].
  classService := RowanClassService new name: theClass name.
  classService update.
  classService isNewClass: true.	"if nothing else, the dirty state of the package/project services
	should be updated. Would like a less heavy weight solution than this, though."
  packageService := RowanPackageService
    forPackageNamed: classService packageName.
  packageService update.
  projectService := RowanProjectService newNamed: packageService projectName.
  projectService update.
  packageService selectedClass: classService.
  RowanCommandResult addResult: classService.
  selectedClass := classService.
  updateType := #'none'.
  self updateSymbols: (Array with: theClass name asString).
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
releaseWindowHandle: integer
  | registry |
  Rowan loggingServiceClass current
    logComment: 'Release window with handle: ' , integer printString.
  registry := SessionTemps current
    at: #'rowanServicesWindowRegistry'
    ifAbsent: [ ^ self ].
  registry removeKey: integer ifAbsent: [  ]
%

category: 'client commands'
method: RowanBrowserService
reloadProjects: projectServices andUpdateServices: services
  | projectNames answeringService |
  services do: [ :service | service organizer: organizer ].
  projectServices do: [ :service | service organizer: organizer ].
  projectServices do: [ :projectService | projectService reloadProject ].
  projectNames := projectServices
    collect: [ :projectService | projectService name ].
  services
    do: [ :service | 
      (projectNames includes: service rowanProjectName)
        ifTrue: [ service updateLatest ] ].
  answeringService := RowanAnsweringService new organizer: organizer.
  answeringService updateAutocompleteSymbols
%

category: 'client commands'
method: RowanBrowserService
removeDictionariesNamed: dictionaryNames
	"remove from both transient & persistent symbol lists" 

	dictionaryNames do:[:dictionaryName | 
		| dictionaryNameSymbol |
		dictionaryNameSymbol := dictionaryName asSymbol.
		(Rowan image symbolList names includes: dictionaryNameSymbol) ifTrue:[
			Rowan image symbolList removeDictionaryNamed: dictionaryNameSymbol].
		(System myUserProfile symbolList names includes: dictionaryNameSymbol) ifTrue:[
			System myUserProfile symbolList removeDictionaryNamed: dictionaryNameSymbol]].
	self updateDictionaries.
%

category: 'client commands'
method: RowanBrowserService
removeMethods: methodServices

	| notRemoved |
	notRemoved := Array new. 
	removedMethods := Array new. 
	methodServices do: [:methodService |
		| classService |
		classService := RowanClassService forClassNamed: methodService className. 
		classService meta: methodService meta. 
		classService removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		classService updatePackageProject.
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService]. 
		removedMethods add: methodService].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:ea | ea selector]) printString].
	updateType := #methodsRemoved:. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanBrowserService
saveRootObject: oop windowHandle: integer
  " a window has been opened on the client. Save the 
	root object of the window so it won't be recycled"

  | dictionary |
  dictionary := SessionTemps current
    at: #'rowanServicesWindowRegistry'
    ifAbsentPut: [ Dictionary new ].
  dictionary at: integer ifAbsentPut: [ Array new ].
  (dictionary at: integer) add: (Object _objectForOop: oop)
%

category: 'other'
method: RowanBrowserService
selectedClass

	^selectedClass
%

category: 'other'
method: RowanBrowserService
selectedClass: object

	selectedClass := object
%

category: 'perform'
method: RowanBrowserService
servicePerform: symbol withArguments: collection
	super perform: symbol withArguments: collection.
%

category: 'client commands'
method: RowanBrowserService
unloadProjectsNamed: array
  array
    do: [ :projectName | 
      | project |
      project := Rowan image loadedProjectNamed: projectName ifAbsent: [  ].
      project
        ifNotNil: [ Rowan projectTools delete deleteProjectNamed: projectName ] ].
  self updateProjects
%

category: 'update'
method: RowanBrowserService
update
	self updateProjects
%

category: 'client commands'
method: RowanBrowserService
updateDictionaries

	dictionaries := Rowan image symbolList names collect:[:name | RowanDictionaryService new name: name asString].
	dictionaries := dictionaries asOrderedCollection. 
	updateType ifNil: [updateType := OrderedCollection new]. 
	updateType add: #dictionaryListUpdate:.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
updateProjects
  | sortedProjects |
  self packagesWithTests. "make sure tests are always updated" 
  sortedProjects := SortedCollection sortBlock: [ :a :b | a name < b name ].
  sortedProjects addAll: Rowan image loadedProjects.
  projects := sortedProjects
    collect: [ :project | RowanProjectService newNamed: project name ].
  updateType := Array with: updateType with: #'projectsUpdate:browser:'. "temporary hack" 
  RowanCommandResult addResult: self
%

category: 'update'
method: RowanBrowserService
updateSymbols: classNames
  newCachedClasses addAll: classNames.
  updateType := #'addCachedSymbols:'
%

! Class implementation for 'RowanClassService'

!		Class methods for 'RowanClassService'

category: 'instance creation'
classmethod: RowanClassService
basicForClassNamed: className 
	"Don't get method services. Efficient for classes with many methods"
	^self new basicForClassNamed: className
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className 

	^self new forClassNamed: className
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className meta: aBoolean

	| inst |
	inst := self forClassNamed: className subStrings first.
	inst meta: aBoolean.
	^inst
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className package: packageName

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst
%

category: 'instance creation'
classmethod: RowanClassService
minimalForClassNamed: className 
	"Don't get method services. Efficient for classes with many methods"
	^self new minimalForClassNamed: className
%

!		Instance methods for 'RowanClassService'

category: 'comparing'
method: RowanClassService
= classService
	(classService class canUnderstand: #isClassService) ifFalse:[^false].
	^classService isClassService
			ifTrue: [  name asString = classService name asString and: [meta = classService meta]]
			ifFalse: [^false]
%

category: 'client commands'
method: RowanClassService
addCategory: string

	| theClass |

	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theClass addCategory: string.
%

category: 'constants'
method: RowanClassService
addSubclassWarningString

	^'Superclass is not packaged. Enter the desired package name'
%

category: 'client commands'
method: RowanClassService
allSubclassServices
  | subclassServices |
  subclassServices := self theClass subclasses asArray
    collect: [ :aClass | RowanClassService minimalForClassNamed: aClass name ].
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: subclassServices.
  (hierarchyServices at: #'expand')
    do: [ :classService | classService allSubclassServices ]
%

category: 'Accessing'
method: RowanClassService
allTests
  | allSelectors theClass |
  self isTestCase
    ifFalse: [ ^ Array new ].
  theClass := self theClass thisClass.
  theClass isAbstract
    ifTrue: [ ^ Array new ].
  allSelectors := self theClass thisClass allTestSelectors.
  ^ allSelectors
    collect: [ :selector | 
      | methodService |
      methodService := RowanMethodService
        forSelector: selector
        class: (theClass whichClassIncludesSelector: selector asString)
        meta: false
        organizer: organizer.
      methodService
        definedClassName: (theClass whichClassIncludesSelector: selector asString) name asString.	"may get changed in client"
      methodService ]
%

category: 'testing'
method: RowanClassService
arePackageAndProjectClean

	^self packageIsDirty not and:[self projectIsDirty not]
%

category: 'initialization'
method: RowanClassService
basicForClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self].
	self basicRefreshFrom: theClass.
%

category: 'initialization'
method: RowanClassService
basicRefreshFrom: theClass
	| classOrMeta theFilters |
	oop := theClass asOop.
	command := nil. 
	commandArgs := nil. 
	superclassName := theClass superClass ifNotNil:[:theSuper | theSuper name asString]. 
	comment := theClass rwComment. 
	organizer ifNil: [organizer := ClassOrganizer new]. "for Jade and tests"
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	self setComment.
	template := self classCreationTemplate.
	theFilters := SortedCollection new.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	self setIsTestCase.
	self updateIsExtension.
%

category: 'Accessing'
method: RowanClassService
behavior

	| behavior |
	behavior := self theClass. 
	meta == true ifTrue:[behavior := behavior class].
	^behavior
%

category: 'client commands'
method: RowanClassService
classComment: string
	| theClass |
	theClass := self theClass. 
	theClass rwComment: string.
%

category: 'rowan'
method: RowanClassService
classCreationTemplate
	
	^self browserTool classCreationTemplateForClass: self theClass hybridBrowser: true.
%

category: 'client commands'
method: RowanClassService
classHierarchy
	hierarchyServices := self classHierarchy: (Array with: self theClass). 
	RowanCommandResult addResult: self.
%

category: 'Accessing'
method: RowanClassService
classHierarchyNames

	| names |
	names := Array new. 
	hierarchyServices keys do:[:classService | 
		classService == #nil ifFalse:[names add: classService name]].
	^names
%

category: 'Accessing'
method: RowanClassService
classInstVarNames
	^classInstVarNames
%

category: 'Updating'
method: RowanClassService
classInstVarNames: newValue
	classInstVarNames := newValue
%

category: 'Accessing'
method: RowanClassService
classOrMeta

	^meta 
			ifTrue:[self theClass class] 
			ifFalse: [self theClass].
%

category: 'instance creation'
method: RowanClassService
classServiceFromOop: anOop
	| theClass className classService |
	theClass := Object _objectForOop: anOop. 
	className := theClass name. 
	classService := RowanClassService new name: className.
	^className asString = name asString ifTrue:[
			className asString = 'Object' 
				ifTrue:[
					classService basicRefreshFrom: theClass]
				ifFalse:[
					classService fastRefresh]]
		ifFalse:[
			classService minimalRefreshFrom: theClass]
%

category: 'Accessing'
method: RowanClassService
classType
	^classType
%

category: 'Updating'
method: RowanClassService
classType: newValue
	classType := newValue
%

category: 'Accessing'
method: RowanClassService
classVarNames
	^classVarNames
%

category: 'Updating'
method: RowanClassService
classVarNames: newValue
	classVarNames := newValue
%

category: 'Accessing'
method: RowanClassService
comment
	^comment
%

category: 'Updating'
method: RowanClassService
comment: newValue
	comment := newValue
%

category: 'constants'
method: RowanClassService
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	
	[ [ [ [ method := aBehavior rwCompileMethod: methodString category: categorySymbol.]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: false ]]
			on: CompileError
			do: [:ex | ^nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume]]
					on: RwPerformingUnpackagedEditNotification
					do: [:ex | ex resume ] .
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] on: Error
		do: [:ex | ex return: method -> warnings]
%

category: 'client commands'
method: RowanClassService
copyClassTo: newClassName
  | newTemplate newClass newClassService index |
  (Rowan image symbolList resolveSymbol: newClassName)
    ifNotNil: [ ^ self inform: newClassName , ' already exists' ].
  index := template findPattern: (Array with: name) startingAt: 1.
  newTemplate := template copy.
  newTemplate removeFrom: index to: index + name size - 1.
  newTemplate insertAll: newClassName at: index.
  newClass := GsCurrentSession currentSession execute: newTemplate.
  newClassService := RowanClassService new name: newClassName.
  self theClass thisClass
    methodsDo: [ :selector :gsMethod | 
      newClassService
        compileMethod: gsMethod sourceString
        behavior: newClass
        symbolList: Rowan image symbolList
        inCategory:
          (self theClass thisClass categoryOfSelector: selector) asSymbol ].
  self theClass thisClass class
    methodsDo: [ :selector :gsMethod | 
      newClassService
        compileMethod: gsMethod sourceString
        behavior: newClass class
        symbolList: Rowan image symbolList
        inCategory:
          (self theClass thisClass class categoryOfSelector: selector) asSymbol ].
  newClassService update.
  (RowanPackageService new name: newClassService packageName) update.
  (RowanDictionaryService new name: dictionaryName) update
%

category: 'Updating'
method: RowanClassService
definedPackageName: newValue

	definedPackageName := newValue
%

category: 'Updating'
method: RowanClassService
expand: boolean

	expand := boolean
%

category: 'client commands'
method: RowanClassService
fastRefresh
	"pushes less information to ston so it's faster"

	| theClass |
	theClass := self theClass. 
	self refreshFrom: theClass. 
	methods do:[:service1 |
			service1 source: nil;
				stepPoints: Array new].
	visibleTests do:[:service2 |
			service2 source: nil;
				stepPoints: Array new.
			].
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanClassService
fileoutCategories: array
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	array do:[:category |
		self behavior fileOutCategory: category on: ws]. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
%

category: 'client commands'
method: RowanClassService
fileoutClass
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: self behavior fileOutClass. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
%

category: 'client commands'
method: RowanClassService
fileoutMethods: array
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	array do:[:methodService |
		self behavior fileOutMethod: methodService selector on: ws]. 
	answeringService answer: ws contents. 
	RowanCommandResult addResult: answeringService.
%

category: 'Accessing'
method: RowanClassService
filters
	^filters
%

category: 'Updating'
method: RowanClassService
filters: newValue
	filters := newValue
%

category: 'initialization'
method: RowanClassService
forClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self refreshFrom: theClass.
%

category: 'client commands'
method: RowanClassService
fullHierarchy
  | behavior sortedSubclasses |
  behavior := self theClass.
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: Array new.
  sortedSubclasses := behavior subclasses
    asSortedCollection: [ :x :y | x name < y name ].
  RowanCommandResult addResult: self.
  sortedSubclasses
    do: [ :subclass | 
      | classService |
      classService := (self classServiceFromOop: subclass asOop) meta: meta.
      (hierarchyServices at: #'expand') add: classService.
      classService allSubclassServices ]
%

category: 'comparing'
method: RowanClassService
hash
	^self name hash bitXor: meta hash
%

category: 'Accessing'
method: RowanClassService
hierarchyServices

	^hierarchyServices
%

category: 'initialization'
method: RowanClassService
initialize

	isExtension := false.
	selectedMethods := Array new.
	meta := false. "assume most of our work is on the instance side"
	selectedPackageServices := Array new.
	isNewClass := false.
	methods := Array new.
	isInSymbolList := true.
	categories := Array new.
%

category: 'initialization'
method: RowanClassService
initializeCategoriesFor: classOrMeta

	| theFilters |
	theFilters := SortedCollection new.
	classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category asString].
	categories := theFilters asOrderedCollection.
%

category: 'method history'
method: RowanClassService
initializeMethodHistoryFor: source
  "about to compile a method. If possible, ensure it's method history is setup."

  | rowanMethodHistory methodHistory selector methodService |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  selector := [ (Rowan platform parseSelectorFrom: source) asSymbol ]
    on: CompileWarning
    do: [ :ex | ex resume ].
  selector = #'_____could_not_parse_selector_from_method_source_____'
    ifTrue: [ ^ self	"invalid source, continue and let save method fail" ]
    ifFalse: [ 
      | compiledMethod |
      compiledMethod := (Object _objectForOop: oop)
        compiledMethodAt: selector
        environmentId: 0
        otherwise: nil.
      compiledMethod
        ifNil: [ ^ self	"we'll create history after the method is compiled" ].
      methodService := RowanMethodService
        forSelector: selector
        class: self theClass
        meta: meta
        organizer: organizer.
      methodHistory := rowanMethodHistory
        at: methodService
        ifAbsentPut: [ Array new ] ]
%

category: 'initialization'
method: RowanClassService
initializeTestMethodsFor: aClass
	| testSelectors |
	(aClass inheritsFrom: TestCase) ifTrue:[
		aClass isAbstract ifTrue:[^self]. 
		testSelectors := aClass thisClass allTestSelectors.
		methods do:[:methodService | 
			methodService isTestMethod: (testSelectors includes: methodService selector)]].
%

category: 'initialization'
method: RowanClassService
initializeVariablesFor: classOrMeta

	| theFilters |
	theFilters := SortedCollection new.
	theFilters addAll: (classOrMeta allInstVarNames collect:[:instVar | instVar asString]).
	variables := theFilters asOrderedCollection.
%

category: 'Accessing'
method: RowanClassService
instVarNames
	^instVarNames
%

category: 'Updating'
method: RowanClassService
instVarNames: newValue
	instVarNames := newValue
%

category: 'testing'
method: RowanClassService
isClassService

	^true
%

category: 'Updating'
method: RowanClassService
isExtension: boolean

	isExtension := boolean
%

category: 'Updating'
method: RowanClassService
isNewClass: boolean
	isNewClass := boolean
%

category: 'testing'
method: RowanClassService
isPackageClean

	^self packageIsDirty not
%

category: 'testing'
method: RowanClassService
isProjectClean
  ^ self projectIsDirty not
%

category: 'Updating'
method: RowanClassService
isTestCase

	^isTestCase
%

category: 'Updating'
method: RowanClassService
isTestCase: aBoolean

	isTestCase := aBoolean
%

category: 'Accessing'
method: RowanClassService
meta

	^meta
%

category: 'Updating'
method: RowanClassService
meta: anObject

	meta := anObject
%

category: 'Accessing'
method: RowanClassService
methods

	"for testing"
	^methods
%

category: 'private'
method: RowanClassService
methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult
	| methodService |

	methodService := RowanMethodService forGsNMethod: gsNMethod organizer: organizer. 
	methodService compilationWarnings: compilationResult value.
	^methodService
%

category: 'private'
method: RowanClassService
methodServicesFor: classOrMeta organizer: theOrganizer

	methods addAll: (classOrMeta selectors collect:[:sel | 
			RowanMethodService 
				forSelector: sel class: classOrMeta thisClass meta: meta organizer: theOrganizer])
%

category: 'private'
method: RowanClassService
methodsIn: theClass categories: theCategories

	| selectors |
	selectors := Array new. 
	theCategories do:[:category |
		selectors addAll: (theClass selectorsIn: category)]. 
	^methods select:[:methodService | selectors includes: methodService selector]
%

category: 'testing'
method: RowanClassService
methodsNamed: selector
	"For testing. Multiple because class could have both instance and class methods"

	^methods select:[:methodService | methodService selector = selector]
%

category: 'initialization'
method: RowanClassService
minimalForClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self minimalRefreshFrom: theClass.
%

category: 'initialization'
method: RowanClassService
minimalRefreshFrom: theClass
	| classOrMeta  |
	command := nil. 
	commandArgs := nil. 
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	oop := theClass asOop.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	template := self classCreationTemplate.
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	self setIsTestCase.
%

category: 'client commands'
method: RowanClassService
moveMethods: methodServices to: category
	| behavior |
	behavior := self classOrMeta.
	methodServices do: [:methodService | 
			behavior rwMoveMethod: methodService selector toCategory: category.
			methodService category: category].
	self update. 
	self selectedMethods: methodServices.
%

category: 'Accessing'
method: RowanClassService
name
	^name
%

category: 'Updating'
method: RowanClassService
name: newValue
	name := newValue asString
%

category: 'private'
method: RowanClassService
objectInBaseNamed: aString

	^Rowan image symbolList objectNamed: aString asSymbol
%

category: 'client commands'
method: RowanClassService
oneLevelClassHierarchy
  "good for expanding an existing hierarchy quickly"

  | behavior sortedSubclasses |
  behavior := self theClass.
  hierarchyServices := Dictionary new.
  hierarchyServices at: #'expand' put: Array new.
  sortedSubclasses := behavior subclasses
    asSortedCollection: [ :x :y | x name < y name ].
  sortedSubclasses
    do: [ :subclass | 
      | classService |
      classService := (self classServiceFromOop: subclass asOop) meta: meta.
      (hierarchyServices at: #'expand') add: classService ].
  RowanCommandResult addResult: self
%

category: 'Accessing'
method: RowanClassService
oop
	^oop
%

category: 'Updating'
method: RowanClassService
oop: newValue
	oop := newValue
%

category: 'testing'
method: RowanClassService
packageIsDirty

	| behavior |
	behavior := self theClass.
	behavior rowanPackageName =  Rowan unpackagedName ifTrue:[^true]. "avoid a refresh by assuming it's dirty" 
	^(RowanPackageService new name: behavior rowanPackageName) rowanDirty
%

category: 'Accessing'
method: RowanClassService
packageName
	
	^packageName
%

category: 'Updating'
method: RowanClassService
packageName: pkgName
	
	packageName := pkgName
%

category: 'Accessing'
method: RowanClassService
poolDictionaryNames
	^poolDictionaryNames
%

category: 'Updating'
method: RowanClassService
poolDictionaryNames: newValue
	poolDictionaryNames := newValue
%

category: 'printing'
method: RowanClassService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'testing'
method: RowanClassService
projectIsDirty

	| behavior |
	behavior := self theClass.
	behavior rowanProjectName =  Rowan unpackagedName ifTrue:[^true]. "avoid a refresh by assuming it's dirty" 
	^(RowanProjectService new name: behavior rowanProjectName) rowanDirty
%

category: 'other'
method: RowanClassService
projectName

	^projectName
%

category: 'Updating'
method: RowanClassService
projectName: newValue
	projectName := newValue
%

category: 'initialization'
method: RowanClassService
refreshFrom: theClass
	| classOrMeta  |
	self basicRefreshFrom: theClass. 
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self refreshMethodsFor: classOrMeta
%

category: 'initialization'
method: RowanClassService
refreshMethodsFor: classOrMeta
	| gsNMethods |
	methods := SortedCollection sortBlock: [:x :y | x selector < y selector].
	self methodServicesFor: classOrMeta organizer: organizer.
	methods := methods asOrderedCollection.
	classOrMeta allInstVarNames do:[:instVar | 
			gsNMethods := organizer accessorsOf: instVar inClass: classOrMeta.
			gsNMethods do:[:gsNMethod |
				| service |
				service := methods detect:[:methodService | methodService selector = gsNMethod selector] ifNone:[].
				service ifNotNil:[
					service accessedInstVars add: instVar asString]
	]].
	self initializeTestMethodsFor: classOrMeta thisClass. 
	self setVisibleTests. "methods must be available"
%

category: 'client commands'
method: RowanClassService
removeCategories: theCategories
	| theClass  | 
	self refreshFrom: self theClass. 
	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theCategories do: [:category |
		theClass rwRemoveCategory: category.
		].
%

category: 'client commands'
method: RowanClassService
removeMethods: methodsToRemove

	| notRemoved |
	notRemoved := Array new. 
	methodsToRemove do: [:methodService |
		self removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService.
		]].
	self updateTests.
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:svc | svc selector]) printString].
%

category: 'rowan'
method: RowanClassService
removeSelector: selector

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta
%

category: 'rowan'
method: RowanClassService
removeSelector: selector ifAbsent: absentBlock
	| theClass |
	theClass := self theClass. 
	meta ifTrue: [theClass := theClass class].
	(theClass compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	[self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ]
%

category: 'client commands'
method: RowanClassService
renameCategoryFrom: old to: new

	| affectedSelectors behavior |

	self update. 
	self addCategory: new. 
	behavior := self classOrMeta.
	affectedSelectors := behavior selectorsIn: old.
	methods := methods select:[:methodService | affectedSelectors includes: methodService selector].
	self moveMethods: methods to: new.
	self removeCategories: (Array with: old)
%

category: 'client commands'
method: RowanClassService
renameClass: oldClassName to: newClassName
  | references newMethods newClass oldClass |
  newMethods := Array new.
  oldClass := Rowan image resolveClassNamed: oldClassName.
  newClass := Rowan projectTools browser
    renameClassNamed: oldClassName
    to: newClassName.
  oop := newClass asOop.
  name := newClassName.
  self update.
  renamedName := oldClassName.
  self updateMethodsAfterRenameFrom: oldClassName to: newClassName.
  self updateSubclassesAfterRenameOf: newClass.
  references := organizer update referencesToObject: oldClass.
  references do: [ :method | 
    | newSource compileResult failedCompile methodService oldSource |
    failedCompile := false.
    oldSource := method sourceString.
    newSource := self
      replaceSubString: oldClassName
      in: oldSource
      with: newClassName.
    compileResult := [ 
    method inClass
      rwCompileMethod: newSource
      category: (method inClass categoryOfSelector: method selector) asSymbol ]
      on: CompileError
      do: [ :ex | 
        failedCompile := true.
        method ].
    methodService := RowanMethodService
      forGsNMethod: compileResult
      organizer: organizer.
    failedCompile
      ifTrue: [ methodService comparisonSource: oldClassName ]
      ifFalse: [ methodService comparisonSource: oldSource ].
    methodService failedCompile: failedCompile.
    methodService renamedName: oldClassName.
    newMethods add: methodService ].
  RowanCommandResult addResult: (RowanAnsweringService new answer: newMethods)
%

category: 'private'
method: RowanClassService
replaceSubString: old in: string with: new
	| offset newSource |
	newSource := string. 
	offset := 1. 	
	[(offset := newSource findString: old startingAt: offset) = 0] whileFalse:[
		newSource := newSource copyReplaceFrom: offset to: offset + old size - 1 with: new. 
		offset := offset + new size. 
	].
	^newSource
%

category: 'rowan'
method: RowanClassService
rowanProjectName

	^projectName
%

category: 'client commands'
method: RowanClassService
runClassTests: classService

	"if it errors, the client will handle the error. 
	If it passes, we return true and the client
	will display decent results." 
	| behavior |
	behavior := classService theClass. 
	self refreshFrom: behavior.
	self tests do:[:methodService |
			behavior debug: methodService selector]. 
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
%

category: 'client commands'
method: RowanClassService
runMethodTests: methodServices

	| behavior |
	behavior := self theClass.  
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior debug: methodService selector]].
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
%

category: 'client commands'
method: RowanClassService
saveMethodSource: source category: category
  | behavior compilationResult gsNMethod updatedCategory methodService |
  meta
    ifNil: [ 
      behavior := Object _objectForOop: oop.
      meta := behavior isMeta ]
    ifNotNil: [ 
      behavior := meta
        ifTrue: [ self theClass class ]
        ifFalse: [ self theClass ] ].
  oop := behavior asOop.
  self initializeMethodHistoryFor: source.
  updatedCategory := category ifNil: [ 'other' ].
  compilationResult := self
    compileMethod: source
    behavior: behavior
    symbolList: Rowan image symbolList
    inCategory: updatedCategory asSymbol.
  (gsNMethod := compilationResult key) isNil
    ifTrue: [ 
      System
        signal: 1001
        args: (Array with: compilationResult value)
        signalDictionary: GemStoneError ].
  self update.
  methodService := self
    methodServiceFrom: gsNMethod
    in: behavior
    compiltationResult: compilationResult.
  RowanCommandResult addResult: methodService.
  RowanQueryService new
    organizer: ClassOrganizer new;
    hierarchyImplementorsOf: methodService selector
      inClass: methodService className.	"this will update hierarchy method indicators for client"
  self selectedMethods: (Array with: methodService).
  self updateDirtyState.
  self updateTests.
  self
    updateSymbols:
      gsNMethod _selectorPool asArray , (Array with: methodService selector).
  methodService addToMethodHistory
%

category: 'other'
method: RowanClassService
selectedMethods
	"client side selection. Used after a method compile" 
	^selectedMethods
%

category: 'Updating'
method: RowanClassService
selectedMethods: theMethods
	selectedMethods := theMethods
%

category: 'Accessing'
method: RowanClassService
selectedPackageServices
	^selectedPackageServices
%

category: 'Updating'
method: RowanClassService
selectedPackageServices: newValue
	selectedPackageServices := newValue
%

category: 'Accessing'
method: RowanClassService
selectors

	^methods collect:[:methodService | methodService selector]
%

category: 'perform'
method: RowanClassService
servicePerform: symbol withArguments: collection
  | wasClean |
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  wasClean := self isPackageClean.
  super servicePerform: symbol withArguments: collection.
  updateAfterCommand == false
    ifFalse: [ self update ]. 
  wasClean
    ifTrue: [ self updatePackageProject ]
%

category: 'Accessing'
method: RowanClassService
setComment
  comment := self theClass thisClass comment
%

category: 'private'
method: RowanClassService
setDictionary: classOrMeta
	| dictionaryList |
		dictionaryList := Rowan image symbolList dictionariesAndSymbolsOf: classOrMeta thisClass.
		dictionaryName := dictionaryList isEmpty 
		ifTrue:[String new]
		ifFalse:[dictionaryList first first name asString].
%

category: 'Updating'
method: RowanClassService
setIsTestCase

	isTestCase := self theClass isSubclassOf: TestCase
%

category: 'client commands'
method: RowanClassService
setIsTestCaseCommand

	self setIsTestCase.
%

category: 'client commands'
method: RowanClassService
setVisibleTests
	visibleTests := SortedCollection sortBlock: [:x :y | x selector < y selector]. 
	visibleTests addAll: self allTests.
	visibleTests := visibleTests asArray.
%

category: 'client commands'
method: RowanClassService
subclassCreationTemplate
  | answerService newClassPackageName |
  answerService := RowanAnsweringService new.
  newClassPackageName := self theClass rowanPackageName = Rowan unpackagedName
    ifTrue: [ self addSubclassWarningString ]
    ifFalse: [ self theClass rowanPackageName ].
  answerService
    answer:
      (self browserTool
        classCreationTemplateForSubclassOf: name
        className: 'NewSubclass'
        category: newClassPackageName).
  RowanCommandResult addResult: answerService
%

category: 'private'
method: RowanClassService
subclassServices: subclasses

	| sortedSubclasses |

	sortedSubclasses := SortedCollection sortBlock: [:x :y | x name < y name]. 
	sortedSubclasses addAll: subclasses. 
	^(sortedSubclasses collect:[:cls | (self classServiceFromOop: cls asOop) meta: meta]) asArray.
%

category: 'Accessing'
method: RowanClassService
subclassType
	^subclassType
%

category: 'Updating'
method: RowanClassService
subclassType: newValue
	subclassType := newValue
%

category: 'Accessing'
method: RowanClassService
superclassName
	^superclassName
%

category: 'Updating'
method: RowanClassService
superclassName: newValue
	superclassName := newValue
%

category: 'Accessing'
method: RowanClassService
template
	^template
%

category: 'Updating'
method: RowanClassService
template: newValue
	template := newValue
%

category: 'private'
method: RowanClassService
tests

	^methods select:[:methodService | methodService selector asString matchPattern: #('test' $*)]
%

category: 'instance creation'
method: RowanClassService
theClass
	| theClass |
	theClass := oop ifNil:[Rowan globalNamed: name] ifNotNil: [Object _objectForOop: oop].
	theClass isMeta ifTrue:[oop := theClass thisClass asOop]. 
	(Rowan globalNamed: name) ifNil:[isInSymbolList := false]. 
	theClass ifNil: [^nil]. 
	^theClass thisClass
%

category: 'updates'
method: RowanClassService
update

	self updateClass.
%

category: 'updates'
method: RowanClassService
updateClass

	"It's possible to have a nil class. For example, if we added & selected
	a class then aborted."

	| theClass |
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self]. 
	theClass isBehavior ifFalse:[oop := theClass asOop. ^self].
	self refreshFrom: theClass.
	RowanCommandResult addResult: self
%

category: 'updates'
method: RowanClassService
updateDirtyState
	| projectService | 
	selectedPackageServices do:[:packageService | 
		packageService update. 
		RowanCommandResult addResult: packageService].
	projectService := RowanProjectService newNamed: self theClass rowanProjectName. 
	RowanCommandResult addResult: projectService.
%

category: 'initialization'
method: RowanClassService
updateIsExtension
  isExtension := ((selectedPackageServices
    collect: [ :packageService | packageService name ])
    includes: definedPackageName) not
%

category: 'updates'
method: RowanClassService
updateLatest
  oop := ((Rowan image symbolList resolveSymbol: name)
    ifNil: [ 
      wasRemoved := true.
      updateType := #'removedClass:'.
      RowanCommandResult addResult: self.
      ^ self ]) value asOop.
  super updateLatest
%

category: 'private'
method: RowanClassService
updateMethodsAfterRenameFrom: oldClassName to: newClassName
  methods
    do: [ :methodService | 
      methodService
        renamedName: oldClassName;
        className: newClassName. 
      RowanCommandResult addResult: methodService ]
%

category: 'updates'
method: RowanClassService
updatePackageProject
	| packageService projectService |

	packageService := RowanPackageService new name: packageName. 
	packageService update. 
	projectService := RowanProjectService new name: projectName. 
	projectService update.
%

category: 'private'
method: RowanClassService
updateSubclassesAfterRenameOf: newClass
  organizer := ClassOrganizer new.
  (organizer allSubclassesOf: newClass)
    do: [ :subclass | 
      | subclassService |
      subclassService := RowanClassService minimalForClassNamed: subclass name.
      RowanCommandResult addResult: subclassService ]
%

category: 'updates'
method: RowanClassService
updateSymbols: newSymbols
  | browserService |
  browserService := RowanBrowserService new.
  browserService newCachedSelectors addAll: newSymbols.
  browserService updateType: #'addCachedSymbols:'. 
  RowanCommandResult addResult: browserService
%

category: 'updates'
method: RowanClassService
updateTests
  "update the test browsers on certain operations"

  RowanBrowserService new packagesWithTests.
  (RowanPackageService new
    name: packageName;
    yourself) testClasses.
  RowanCommandResult addResult: self update
%

category: 'Accessing'
method: RowanClassService
version
	^version
%

category: 'Updating'
method: RowanClassService
version: newValue
	version := newValue
%

category: 'Accessing'
method: RowanClassService
versions
	^versions
%

category: 'Updating'
method: RowanClassService
versions: newValue
	versions := newValue
%

category: 'Accessing'
method: RowanClassService
visibleTests

	^visibleTests
%

category: 'testing'
method: RowanClassService
wasDeleted
	^(Rowan globalNamed: name) isNil
%

category: 'Accessing'
method: RowanClassService
wasRemoved: boolean

	wasRemoved := boolean
%

! Class implementation for 'RowanDebuggerService'

!		Instance methods for 'RowanDebuggerService'

category: 'debug string'
method: RowanDebuggerService
debugStringFrom: aString
  | debugStream newStream char peekChar |
  debugStream := ReadStream on: aString trimLeadingBlanks.
  [ 
  peekChar := debugStream peek.
  peekChar = Character tab
    or: [ peekChar = Character cr or: [ peekChar = Character lf ] ]	"trimLeadingBlanks doesn't look for tab or cr or lf" ]
    whileTrue: [ debugStream next ].
  debugStream contents isEmpty
    ifTrue: [ ^ 'nil halt.' ].
  newStream := WriteStream on: String new.
  (char := debugStream next) = $|
    ifTrue: [ 
      newStream nextPut: char.
      newStream
        nextPutAll: (debugStream upTo: $|);
        nextPut: $|;
        nextPut: Character space;
        nextPutAll: 'nil halt. ' ]
    ifFalse: [ 
      newStream
        nextPutAll: 'nil halt. ';
        nextPut: char ].
  newStream nextPutAll: debugStream upToEnd.
  ^ newStream contents
%

category: 'release'
method: RowanDebuggerService
releaseProcessOop: oop
  "not really releasing it. The client should have registered
	the process with the debugger window it opened before
	this is run"

  | jadeiteProcesses process |
  ((process := Object _objectForOop: oop) isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      jadeiteProcesses remove: process ifAbsent: [  ] ]
%

category: 'release'
method: RowanDebuggerService
saveProcessOop: processOop
  | jadeiteProcesses process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      (jadeiteProcesses includes: process)
        ifFalse: [ 
          "tests may pass through this method twice"
          jadeiteProcesses add: process ] ]
%

category: 'perform'
method: RowanDebuggerService
servicePerform: symbol withArguments: collection
	^self perform: symbol withArguments: collection.
%

category: 'clientCommands'
method: RowanDebuggerService
terminateProcess: processOop
  | process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifFalse: [ ^ self ].
  RowanDebuggerService new releaseProcessOop: processOop.
  process terminate.
  (Delay forMilliseconds: 10) wait	"allow forked processes to finish"
%

category: 'updating'
method: RowanDebuggerService
update
  processes := OrderedCollection
    with:
      (RowanProcessService onActiveProcess: (Object _objectForOop: initialProcessOop)).
  ProcessorScheduler scheduler readyProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'ready')]. 
  ProcessorScheduler scheduler suspendedProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'suspended')]. 
  ProcessorScheduler scheduler waitingProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'waiting')]. 
  RowanCommandResult addResult: self.
  self releaseProcessOop: initialProcessOop
%

! Class implementation for 'RowanDictionaryService'

!		Instance methods for 'RowanDictionaryService'

category: 'accessing'
method: RowanDictionaryService
classes
	^classes
%

category: 'accessing'
method: RowanDictionaryService
classes: object
	classes := object
%

category: 'client commands'
method: RowanDictionaryService
classHierarchy
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	"reuse behavior in package service for now" 
	hierarchyServices := (RowanPackageService new classes: classes) classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
%

category: 'Updating'
method: RowanDictionaryService
defaultTemplate: newValue
	defaultTemplate := newValue
%

category: 'command support'
method: RowanDictionaryService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: nil packageName: nil
%

category: 'initialization'
method: RowanDictionaryService
initialize

	self setDefaultTemplate
%

category: 'client commands'
method: RowanDictionaryService
insertAt: index

	| theDictionary |
	theDictionary := SymbolDictionary new. 
	theDictionary at: name asSymbol put: theDictionary. 
	System myUserProfile insertDictionary: theDictionary at: index. 
	RowanBrowserService new updateDictionaries.
%

category: 'testing'
method: RowanDictionaryService
isDictionaryService
  ^ true
%

category: 'accessing'
method: RowanDictionaryService
name
	^name
%

category: 'accessing'
method: RowanDictionaryService
name: object
	name := object
%

category: 'client commands'
method: RowanDictionaryService
removeClass: classService
	self removeClassNamed: classService name. 
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
%

category: 'client commands'
method: RowanDictionaryService
removeClassNamed: className

	self browserTool removeClassNamed: className.
%

category: 'client commands'
method: RowanDictionaryService
removeGlobalNamed: symbol
	| dictionary |
	dictionary := (System myUserProfile resolveSymbol: name asSymbol) value.
	dictionary ifNotNil: [
		dictionary removeKey: symbol ifAbsent:[]].
	self update.
%

category: 'perform'
method: RowanDictionaryService
servicePerform: symbol withArguments: collection
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
  self update
%

category: 'client commands'
method: RowanDictionaryService
setDefaultTemplate

	defaultTemplate := self genericClassCreationTemplate.
%

category: 'updates'
method: RowanDictionaryService
update
  | dictionary sorted |
  classes := Array new.
  sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
  dictionary := Rowan image symbolList objectNamed: name.
  dictionary ifNil: [ ^ self ].
  (dictionary isKindOf: SymbolDictionary)
    ifFalse: [ ^ self ].
  dictionary
    keysAndValuesDo: [ :key :value | 
      value isClass
        ifTrue: [ 
          | classService |
          classService := RowanClassService new name: key asString.
          classService versions: value classHistory size.
          classService version: (value classHistory indexOf: value).
          classes add: classService ]
        ifFalse: [ 
          | printString theKey |
          printString := [ 
          value printString charSize > 1
            ifTrue: [ '<<unprintable string. charSize > 1>>' ]
            ifFalse: [ value printString ] ]
            on: Error
            do: [ :ex | 'unprintable string. Error - <' , ex printString , '>' ].
          key charSize = 1
            ifTrue: [ theKey := key ]
            ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
          sorted
            add:
              (Array
                with: name , '.' , theKey
                with: value class name
                with: value asOop
                with: printString) ] ].
  globals := sorted asArray.
  RowanCommandResult addResult: self
%

category: 'testing'
method: RowanDictionaryService
wasDeleted
  ^ (Rowan globalNamed: name) isNil
%

! Class implementation for 'RowanFrameService'

!		Class methods for 'RowanFrameService'

category: 'other'
classmethod: RowanFrameService
process: aGsProcess level: anInteger organizer: aClassOrganizer

	^self basicNew
		initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer;
		yourself
%

!		Instance methods for 'RowanFrameService'

category: 'other'
method: RowanFrameService
initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer
  "In 3.2.15 the server does some whacky things with IP, stepPoint, and nested methods.
	See http://kermit.gemtalksystems.com/bug?bug=45553 --JGF"

  | frameData gsNMethod homeMethodService |
  frameData := aGsProcess _frameContentsAt: anInteger.
  frameData isNil
    ifTrue: [ ^ self	"not sure if bad frame data is a 3.2.15 bug or not" ].
  oop := (frameData at: 8) asOop.
  gsNMethod := frameData at: 1.
  label := aGsProcess _reportAt: anInteger.
  method := RowanMethodService
    forGsNMethod: gsNMethod
    organizer: aClassOrganizer.
  homeMethodService := RowanMethodService
    forGsNMethod: gsNMethod homeMethod
    organizer: aClassOrganizer.
  method breakPoints: homeMethodService breakPoints.
  homeMethodSelector := gsNMethod homeMethod selector.
  homeMethodClassName := gsNMethod homeMethod inClass
    ifNotNil: [ :cls | 
      | className |
      className := cls name asString.
      classIsResolvable := (Rowan image
        resolveClassNamed: cls theNonMetaClass name asString) isNil not.
      className ].
  stepPoint := gsNMethod == gsNMethod homeMethod
    ifTrue: [ aGsProcess _stepPointAt: anInteger ]
    ifFalse: [ gsNMethod homeMethod _stepPointForMeth: gsNMethod ip: (frameData at: 2) ].
  vars := self varsFor: frameData
%

category: 'perform'
method: RowanFrameService
servicePerform: symbol withArguments: collection
	^self perform: symbol withArguments: collection.
%

category: 'other'
method: RowanFrameService
varsFor: anArray

	| keys list receiver values |
	receiver := anArray at: 10.
	values := OrderedCollection new.
	(Reflection classOf: receiver) name == #'ClientForwarder' ifTrue: [
		keys := OrderedCollection with: 'clientObject'.
		values add: receiver clientObject.
		receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
	] ifFalse: [
		((receiver isKindOf: BlockClosure) or: [receiver isKindOf: Class]) ifTrue: [
			keys := OrderedCollection new.
		] ifFalse: [
			keys := receiver class allInstVarNames asOrderedCollection collect: [:each | '-' , each].
			1 to: keys size do: [:i |
				values add: (receiver instVarAt: i).
			].
		].
	].
	keys addFirst: #'receiver'.
	values addFirst: receiver.
	keys addAll: (anArray at: 9).
	keys := keys reject: [:each | each first == $.].
	values addAll: (anArray size >= 11
		ifTrue: [anArray copyFrom: 11 to: anArray size]
		ifFalse: [#()]).
	list := Array new.
	1 to: (keys size min: values size) do: [:i | | theOop key value valueClass | 
		key := keys at: i.
		value := values at: i.
		valueClass := value class.
		theOop := value asOop.
		value := [
			value printString.
		] on: Error do: [:ex | 
			ex return: '(' , value class name , ' printString error: ' , ex description , ')'. 
		].
		value size > 500 ifTrue: [value := (value copyFrom: 1 to: 500) , '...'].
		value := value collect: [:char | (char asciiValue < 32 or: [127 < char asciiValue]) ifTrue: [$?] ifFalse: [char]].
		list add: (RowanVariableService oop: theOop key: key value: value className: valueClass name asString).
	].
	^list
%

! Class implementation for 'RowanInspectorService'

!		Instance methods for 'RowanInspectorService'

category: 'command support'
method: RowanInspectorService
addDynamicInstVars: anObject
  | dynamic dynamicSize |
  dynamic := anObject dynamicInstanceVariables.
  dynamicSize := dynamic size.
  1 to: dynamicSize do: [ :i | 
    objects
      add:
        ('--' , (self stripOutUnicode: (dynamic at: i)))
          -> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ]
%

category: 'command support'
method: RowanInspectorService
addFirstIndexedVars: anObject
  | max |
  self setIndexedSize: anObject.
  max := indexedSize min: maxIndexedVars.
  objects addAll: (self safeVariablesFrom: 1 to: max).
  visibleIndices := max
%

category: 'command support'
method: RowanInspectorService
addInstVars: anObject
  | namedSize |
  instVarNames := anObject class allInstVarNames.
  namedSize := instVarNames size.
  1 to: namedSize do: [ :i | 
    objects
      add:
        ('-' , (self stripOutUnicode: (instVarNames at: i) asString))
          -> (Reflection oopOf: (anObject instVarAt: i)) ]
%

category: 'client commands'
method: RowanInspectorService
addKey: keyString
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := keyString
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: newObject ifAbsentPut: nil.
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop
%

category: 'client commands'
method: RowanInspectorService
addObject: string after: index
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject class isIndexable
    ifTrue: [ 
      | insertionCollection |
      insertionCollection := (theObject isKindOf: CharacterCollection)
        ifTrue: [ newObject ]
        ifFalse: [ Array with: newObject ].
      theObject insertAll: insertionCollection at: index + 1 ]
    ifFalse: [ theObject add: newObject ].
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop.
  theObject class isIndexable
    ifTrue: [ self inspect: oop from: visibleIndices to: visibleIndices + 1 ]
%

category: 'client commands'
method: RowanInspectorService
executeThenInspect: string context: anOop inWindow: handle
  | answer |
  answer := RowanAnsweringService new basicExec: string context: anOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
%

category: 'client commands'
method: RowanInspectorService
executeThenInspect: string inFrame: level process: processOop context: contextOop inWindow: handle
  | answer |
  answer := RowanAnsweringService new exec: string inFrame: level ofProcess: processOop context: contextOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
%

category: 'initialization'
method: RowanInspectorService
initialize
  super initialize.
  objects := OrderedCollection new.
  instVarsAreRemovable := false.
  isUnordered := false
%

category: 'client commands'
method: RowanInspectorService
inspect: oopOrObject
  ^ self inspect: oopOrObject inWindow: nil
%

category: 'client commands'
method: RowanInspectorService
inspect: anOop from: indexStart to: indexStop
  | anObject stop |
  anObject := Object _objectForOop: anOop.
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanInspectorService
inspect: oopOrObject inWindow: handle
  | anObject |
  self setOopFrom: oopOrObject.
  handle
    ifNotNil: [ RowanBrowserService new saveRootObject: oop windowHandle: handle ].
  anObject := Object _objectForOop: oop.
  isVariable := anObject class isVariable.
  (self isClientForwarder: anObject)
    ifTrue: [ ^ self inspectClientForwarder: anObject ].
  className := anObject class name.
  myself := 'self' -> (self selfPrintString: anObject).
  (anObject isKindOf: Dictionary superclass)
    ifTrue: [ ^ objects addAll: (self inspectDictionary: anObject from: 1 to: maxIndexedVars) ].
  self addInstVars: anObject.
  self addDynamicInstVars: anObject.
  self addFirstIndexedVars: anObject.
  RowanCommandResult addResult: self
%

category: 'command support'
method: RowanInspectorService
inspectClientForwarder: anObject

	oop := Reflection oopOf: anObject. 
	myself := 'self' -> anObject clientObject printString. 
	RowanCommandResult addResult: self.
%

category: 'command support'
method: RowanInspectorService
inspectDictionary: aDictionary from: start to: stop
  | sortedKeys service |
  isDictionary := true.
  instVarsAreRemovable := true.
  self addInstVars: aDictionary.
  self addDynamicInstVars: aDictionary.
  service := RowanAnsweringService new.
  sortedKeys := (aDictionary keys
    collect: [ :key | 
      (service basicPrintStringOfObject: key toMaxSize: self maxPrintStringSize)
        -> key ]) asSortedCollection.
  visibleIndices := (sortedKeys size min: stop) min: self maxVariables.
  nextIndices := OrderedCollection new.
  (sortedKeys copyFrom: start to: visibleIndices)
    do: [ :assoc | 
      nextIndices
        add:
          assoc key
            ->
              (Array
                with: (Reflection oopOf: assoc value)
                with: (Reflection oopOf: (aDictionary at: assoc value))) ].
  indexedSize := aDictionary size.
  RowanCommandResult addResult: self.
  ^ nextIndices
%

category: 'testing'
method: RowanInspectorService
isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder'
%

category: 'constants'
method: RowanInspectorService
maxPrintStringSize
  "currently matches Jadeite client"

  ^ 100000
%

category: 'constants'
method: RowanInspectorService
maxVariables
	"See Jadeite issue #668. Currently, 
	Dolphin doesn't seem to be able to 
	handle more than about 65k elements
	in a list
	https://github.com/GemTalk/Jadeite/issues/668#issuecomment-566795924" 
	
	^65000
%

category: 'client commands'
method: RowanInspectorService
nextIndexedVarsFrom: indexStart to: indexStop
  | stop anObject |
  anObject := Object _objectForOop: oop.
  (anObject isKindOf: Dictionary superclass)
    ifTrue: [ 
      ^ objects
        addAll:
          (self inspectDictionary: anObject from: indexStart to: indexStop) ].
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanInspectorService
oop: anInteger

	oop := anInteger
%

category: 'private'
method: RowanInspectorService
reinspect: theObject
  | formerlyVisible |
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  (visibleIndices > 0 and: [ visibleIndices < formerlyVisible ])
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'client commands'
method: RowanInspectorService
removeDynamicInstVars: dynamicInstVarNames
  | theObject |
  theObject := Object _objectForOop: oop.
  dynamicInstVarNames
    do: [ :dynamicInstVar | theObject removeDynamicInstVar: dynamicInstVar ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeIndexedInstVarsAt: indices
  | theObject removalObjects |
  theObject := Object _objectForOop: oop.
  removalObjects := indices collect: [ :index | theObject at: index ].
  removalObjects do: [ :removalObject | theObject remove: removalObject ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeKeys: keyOops
  | theObject removalKey |
  theObject := Object _objectForOop: oop.
  keyOops
    do: [ :keyOop | 
      removalKey := Object _objectForOop: keyOop.
      theObject removeKey: removalKey.
      visibleIndices := visibleIndices - 1 max: 0 ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeOop: elementOop
  | theObject removal |
  theObject := Object _objectForOop: oop.
  removal := Object _objectForOop: elementOop.
  theObject remove: removal.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
replaceElement: oldOop with: string
  | theObject newObject formerlyVisible oldObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  oldObject := Object _objectForOop: oldOop.
  theObject remove: oldObject.
  theObject add: newObject.
  selectionOop := newObject asOop.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'command support'
method: RowanInspectorService
safeVariablesFrom: indexStart to: indexStop
  [ ^ self variablesFrom: indexStart to: indexStop ]
    on: Error
    do: [ :ex | 
      indexedSize := 0.
      instVarsAreRemovable := false.
      visibleIndices := 0 to: 0.
      nextIndices := 0 to: 0.
      isVariable := false.
      statusText := 'Error getting object contents: ' , ex description.
      ^ OrderedCollection new ]
%

category: 'client commands'
method: RowanInspectorService
save: string dynamicInstVar: instVarName
  | theObject newObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject dynamicInstVarAt: instVarName put: newObject.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
save: string indexedVarAt: index
  | theObject newObject formerlyVisible |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: index put: newObject.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'client commands'
method: RowanInspectorService
save: string namedInstVar: instVarName
  | theObject newObject instVarIndex |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  instVarIndex := theObject class allInstVarNames indexOf: instVarName.
  theObject instVarAt: instVarIndex put: newObject.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
saveKey: keyOop value: string
  | theObject newObject key |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  key := Object _objectForOop: keyOop.
  theObject at: key put: newObject.
  self reinspect: theObject
%

category: 'printing'
method: RowanInspectorService
selfPrintString: anObject
  ^ [ self stripOutUnicode: anObject printString ]
    on: Error
    do: [ :ex | 
      | printString |
      printString := 'Error printing object with oop ' , oop printString
        , '. Error text: ' , ex printString ]
%

category: 'private'
method: RowanInspectorService
setIndexedSize: anObject
  indexedSize := anObject class isVariable
    ifFalse: [ 0 ]
    ifTrue: [ 
      instVarsAreRemovable := true.
      (self shouldReferenceBySize: anObject)
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - anObject class allInstVarNames size ] ]
%

category: 'private'
method: RowanInspectorService
setOopFrom: oopOrObject
  oop := isOop
    ifTrue: [ oopOrObject ]
    ifFalse: [ Reflection oopOf: oopOrObject ]
%

category: 'testing'
method: RowanInspectorService
shouldReferenceBySize: anObject

	(anObject isKindOf: UnorderedCollection) ifTrue:[^true].
	(anObject isKindOf: MultiByteString) ifTrue:[^true]. 
	^false
%

category: 'command support'
method: RowanInspectorService
variablesFrom: indexStart to: indexStop
  | namedSize anObject indexedVars |
  indexedVars := OrderedCollection new.
  anObject := Object _objectForOop: oop.
  namedSize := anObject class allInstVarNames size.
  isUnordered := anObject isKindOf: UnorderedCollection.
  isUnordered
    ifTrue: [ 
      | sorted service |
      service := RowanAnsweringService new.
      sorted := (anObject
        collect: [ :var | 
          (service basicPrintStringOfObject: var toMaxSize: self maxPrintStringSize)
            -> (Reflection oopOf: var) ]) asSortedCollection: [:x :y | x key < y key]. 
      indexStart to: indexStop do: [ :i | indexedVars add: (sorted at: i) ] ]
    ifFalse: [ indexStart to: indexStop do: [ :i | indexedVars add: i printString -> (Reflection oopOf: (anObject at: i)) ] ].
  ^ indexedVars
%

! Class implementation for 'RowanLoggingService'

!		Class methods for 'RowanLoggingService'

category: 'accessing'
classmethod: RowanLoggingService
current

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanLoggingService ifAbsentPut: [RowanLoggingService new]
%

category: 'accessing'
classmethod: RowanLoggingService
current: anObject

	SessionTemps current at: #rowanLoggingService put: anObject
%

!		Instance methods for 'RowanLoggingService'

category: 'accessing'
method: RowanLoggingService
fileName
	^fileName
%

category: 'accessing'
method: RowanLoggingService
fileName: object
	fileName := object
%

category: 'initialization'
method: RowanLoggingService
initialize

	super initialize. 
	isLogging := false.
%

category: 'client commands'
method: RowanLoggingService
logComment: string

	| stonString ws |
	isLogging ifFalse:[^self].
	comment := string.
	id := id + 1. 
	date := Date today.
	time := Time now.
	location := #server.
	stonString := STON toString: self.
	ws := FileStreamPortable 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close].
	comment := nil "service may be reused. Clear comment"
%

category: 'accessing'
method: RowanLoggingService
logFileContents

	| rs |
	rs := [FileStreamPortable read: fileName] on: Error do:[:ex | ^String new].
	[^rs contents] ensure: [rs close]
%

category: 'client commands'
method: RowanLoggingService
logReceivedServices

	mode := #received.
	self class current: self. 
	self logServices
%

category: 'client commands'
method: RowanLoggingService
logSentServices

	mode := #sent.
	services := RowanCommandResult results copy asOrderedCollection.
	self logServices.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanLoggingService
logServices

	| stonString ws |
	isLogging ifFalse:[^self].
	id := id + 1. 
	date := Date today.
	time := Time now.
	location := #server.
	stonString := STON toString: self.
	ws := FileStreamPortable 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close]
%

! Class implementation for 'RowanMethodService'

!		Class methods for 'RowanMethodService'

category: 'instance creation'
classmethod: RowanMethodService
forGsNMethod: aGsNMethod organizer: anOrganizer
	^self new
		initialize: aGsNMethod organizer: anOrganizer;
		yourself
%

category: 'instance creation'
classmethod: RowanMethodService
forSelector: sel class: theClass meta: boolean organizer: anOrganizer

	| service |
	service := self new. 
	service selector: sel;
		meta: boolean.
	service forClass: theClass organizer: anOrganizer.
	^service
%

category: 'utility'
classmethod: RowanMethodService
put: string onStreamWithoutUnicode: stream

	string do:[:char | 
			char asInteger > 255 ifTrue:[
				stream nextPutAll: '$\x', char asInteger asHexString]
			ifFalse:[
				stream nextPut: char
			]].
%

category: 'utility'
classmethod: RowanMethodService
removeUnicode: string

		| ws | 
		ws := WriteStream on: String new. 
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents
%

category: 'utility'
classmethod: RowanMethodService
removeUnicodeFromSource: string

	"possibly a unicode string which is not yet 
	presentable in Jadeite."

	(string isKindOf: MultiByteString) ifTrue:[
		| ws | 
		ws := WriteStream on: String new. 
		ws nextPutAll: '''METHOD SOURCE CANNOT BE DISPLAYED
This method source contains unicode and is not displayable in Jadeite. 
An approximation of the source code is given printed below with $\x<char hex value>
replacing the actual unicode character.''';
			cr; 
			cr.
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents]
	ifFalse:[
		^string].
%

category: 'instance creation'
classmethod: RowanMethodService
source: source selector: selector category: category className: className packageName: packageName meta: boolString

	| service |
	self rowanFixMe. "Need to include super and sub implementors"
	service := self new. 
	service 
		source: source;
		selector: selector;
		category: category asString;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service
%

!		Instance methods for 'RowanMethodService'

category: 'comparing'
method: RowanMethodService
= methodService
	(methodService class canUnderstand: #isMethodService) ifFalse:[^false].
	methodService isMethodService ifFalse:[^false].
	^selector = methodService selector
		and: [className asString = methodService className asString and: [meta = methodService meta]]
%

category: 'Accessing'
method: RowanMethodService
accessedInstVars
  ^ accessedInstVars
%

category: 'Updating'
method: RowanMethodService
accessedInstVars: anArray
	accessedInstVars := anArray
%

category: 'rowan'
method: RowanMethodService
addOrUpdateMethod

		self browserTool  
                   addOrUpdateMethod: source
                   inProtocol: category
                   forClassNamed: self classService name
                   isMeta: meta
                   inPackageNamed: self classService packageName
%

category: 'method history'
method: RowanMethodService
addToMethodHistory
  | rowanMethodHistory methodHistory |
  rowanMethodHistory := self userGlobals
    at: #'RowanMethodHistory'
    ifAbsentPut: [ Dictionary new ].
  methodHistory := rowanMethodHistory at: self ifAbsentPut: [ Array new ].
  methodHistory add: self
%

category: 'client commands'
method: RowanMethodService
allReferences
  | methods |
  oop := self gsNMethod asOop.
  methods := organizer sendersOf: selector.
  references := methods first
    collect: [ :gsNMethod | self class forGsNMethod: gsNMethod organizer: organizer ].
  RowanCommandResult addResult: self
%

category: 'Accessing'
method: RowanMethodService
breakPoints

	^breakPoints
%

category: 'Accessing'
method: RowanMethodService
breakPoints: collection
  breakPoints := collection
%

category: 'initialization'
method: RowanMethodService
breakPointsFor: aGsNMethod
  "Answers an Array stepPoints"

  | theMethod |
  theMethod := aGsNMethod isMethodForBlock
    ifTrue: [ 
      isMethodForBlock := true.
      aGsNMethod homeMethod ]
    ifFalse: [ aGsNMethod ].
  homeMethodOop := theMethod asOop.
	^ self _initializeBreakPointsFor: theMethod
%

category: 'Accessing'
method: RowanMethodService
category
	^category
%

category: 'Updating'
method: RowanMethodService
category: newValue
	category := newValue asString
%

category: 'Accessing'
method: RowanMethodService
classFromName
  "the dictionary browser may have versions numbers in the name"

  | nameSymbol |
  nameSymbol := (className copyUpTo: Character space) asSymbol.
  ^ (System myUserProfile resolveSymbol: nameSymbol) value
%

category: 'Accessing'
method: RowanMethodService
className
	^className
%

category: 'Updating'
method: RowanMethodService
className: newValue
	className := newValue asString
%

category: 'Accessing'
method: RowanMethodService
classOrMeta

	^meta 
			ifTrue:[self classFromName class] 
			ifFalse: [self classFromName].
%

category: 'Accessing'
method: RowanMethodService
classService

	^classService ifNil:[classService := RowanClassService forClassNamed: className package: packageName]
%

category: 'Updating'
method: RowanMethodService
classService: newValue
	classService := newValue
%

category: 'client commands'
method: RowanMethodService
clearBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method clearBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
clearMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self clearBreakAt: breakPoint ]
%

category: 'Updating'
method: RowanMethodService
comparisonSource: string

	comparisonSource := string
%

category: 'Accessing'
method: RowanMethodService
compilationWarnings

	^compilationWarnings
%

category: 'Updating'
method: RowanMethodService
compilationWarnings: newValue
	compilationWarnings := newValue
%

category: 'client commands'
method: RowanMethodService
debugTest: testSelector inClassName: theClassName
  testResult := 'passed'.
  [ (Rowan image objectNamed: theClassName) debug: testSelector asSymbol ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanMethodService
debugTestAsFailure: testSelector inClassName: theClassName
  testResult := 'passed'.
  [ 
  ((Rowan image objectNamed: theClassName) selector: testSelector asSymbol)
    debugAsFailure ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanMethodService
definedClassName
	^definedClassName
%

category: 'accessing'
method: RowanMethodService
definedClassName: object
	definedClassName := object
%

category: 'Accessing'
method: RowanMethodService
definedPackage

	^definedPackage
%

category: 'rowan'
method: RowanMethodService
definitionClass

	^RwMethodDefinition
%

category: 'client commands'
method: RowanMethodService
disableBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method disableBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
disableMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self disableBreakAt: breakPoint ]
%

category: 'client commands'
method: RowanMethodService
enableBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method setBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
enableMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self enableBreakAt: breakPoint ]
%

category: 'Updating'
method: RowanMethodService
failedCompile: boolean	
	
	failedCompile := boolean
%

category: 'client commands'
method: RowanMethodService
fileout
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: (self behavior fileOutMethod: selector asString environmentId: 0).
	answeringService answer: ws contents.
	RowanCommandResult addResult: answeringService.
%

category: 'Accessing'
method: RowanMethodService
firstReference: integer

	firstReference := integer
%

category: 'initialization'
method: RowanMethodService
forClass: theClass organizer: theOrganizer
  "assume meta and selector are set"

  | classOrMeta gsNMethod |
  organizer := theOrganizer.
  classOrMeta := meta
    ifTrue: [ theClass class ]
    ifFalse: [ theClass ].
  gsNMethod := classOrMeta compiledMethodAt: selector.
  definedPackage := gsNMethod rowanPackageName.
  oop := gsNMethod asOop.
  stepPoints := self stepPointsFor: gsNMethod.
  breakPoints := self breakPointsFor: gsNMethod.
  self updateSource: gsNMethod sourceString.
  category := (classOrMeta categoryOfSelector: selector) asString.
  className := theClass name asString.
  packageName := gsNMethod rowanPackageName.
  projectName := gsNMethod rowanProjectName.
  self setSupersAndSubsFor: classOrMeta.
  isExtension := self rowanIsExtension.
  self initializeTestMethodsFor: classOrMeta thisClass
%

category: 'Accessing'
method: RowanMethodService
gsNMethod

	^[self classOrMeta compiledMethodAt: selector ] on: Error do:[:ex | nil "removed method"]
%

category: 'comparing'
method: RowanMethodService
hash
  ^ (selector hash bitXor: className hash) bitXor: meta hash
%

category: 'Accessing'
method: RowanMethodService
hasSubs

	^hasSubs
%

category: 'Updating'
method: RowanMethodService
hasSubs: aBoolean

	hasSubs := aBoolean
%

category: 'Accessing'
method: RowanMethodService
hasSupers

	^hasSupers
%

category: 'Updating'
method: RowanMethodService
hasSupers: aBoolean

	hasSupers := aBoolean
%

category: 'initialization'
method: RowanMethodService
initialize

	super initialize. 
	hasSupers := false. 
	hasSubs := false.
	accessedInstVars := Array new.
	isTestMethod := false.
	failedCompile := false.
	isMethodForBlock := false.
	hasMethodHistory := true.
%

category: 'initialization'
method: RowanMethodService
initialize: aGsNMethod organizer: aClassOrganizer

	| inClass |
	oop := aGsNMethod asOop.
	definedPackage := aGsNMethod rowanPackageName. 
	selector := aGsNMethod selector.
	stepPoints := self stepPointsFor: aGsNMethod.
	breakPoints := self breakPointsFor: aGsNMethod.
	((inClass := aGsNMethod inClass) isNil or: [selector isNil]) ifTrue: [
		meta := false.
		self updateSource: aGsNMethod sourceString.
		hasSupers := false.
		hasSubs := false.
		organizer := aClassOrganizer. 
		inSelectedPackage := false.
		^self
	].
	meta := inClass isMeta.
	self 
		forClass: inClass thisClass 
		organizer: aClassOrganizer.
	self initializeTestMethodsFor: inClass thisClass.
	self setHasMethodHistory
%

category: 'initialization'
method: RowanMethodService
initializeTestMethodsFor: aClass
	| testSelectors |
	(aClass inheritsFrom: TestCase)
		ifTrue: [ 
			aClass isAbstract
				ifTrue: [ ^ self ].
			testSelectors := aClass thisClass allTestSelectors.
			isTestMethod := testSelectors includes: selector ]
%

category: 'testing'
method: RowanMethodService
isMethodService

	^true
%

category: 'testing'
method: RowanMethodService
isTestMethod

	^isTestMethod
%

category: 'Updating'
method: RowanMethodService
isTestMethod: boolean

	isTestMethod := boolean
%

category: 'testing'
method: RowanMethodService
isUnboundMethod

	(className notNil and: [selector notNil]) ifTrue:[^false].
	^(Object _objectForOop: oop) isKindOf: GsNMethod
%

category: 'Accessing'
method: RowanMethodService
meta
	^meta
%

category: 'Updating'
method: RowanMethodService
meta: aBoolean
	"allow nil parameter for now" 
	meta := aBoolean == true
%

category: 'Accessing'
method: RowanMethodService
method

	^self classFromName compiledMethodAt: selector otherwise: nil
%

category: 'Accessing'
method: RowanMethodService
methodDefinitions
	^methodDefinitions
%

category: 'Updating'
method: RowanMethodService
methodDefinitions: newValue
	methodDefinitions := newValue
%

category: 'Accessing'
method: RowanMethodService
name

	^selector
%

category: 'accessing'
method: RowanMethodService
oop
	^oop
%

category: 'accessing'
method: RowanMethodService
oop: object
	oop := object
%

category: 'Accessing'
method: RowanMethodService
packageName
	^packageName
%

category: 'Updating'
method: RowanMethodService
packageName: newValue
	packageName := newValue
%

category: 'printing'
method: RowanMethodService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $(;
				nextPutAll: (className ifNil:[nil printString]); 
				nextPutAll: '>>'; 
				nextPutAll: (selector ifNil:[nil printString]);
				nextPut: $)
%

category: 'client commands'
method: RowanMethodService
reformatSource
  source := (RBParser parseMethod: source) formattedCode.
  updateType := #'dontUpdateSystem'.	"let browser update the source"
  RowanCommandResult addResult: self
%

category: 'rowan'
method: RowanMethodService
removeSelector: sel class: clsName
	
	self rowanFixMe. "combine remove methods variants"
	self browserTool removeMethod: sel asSymbol forClassNamed: (clsName subStrings first) isMeta: (clsName subStrings size = 2)
%

category: 'Accessing'
method: RowanMethodService
renamedName
	^renamedName
%

category: 'Accessing'
method: RowanMethodService
renamedName: object
	renamedName := object
%

category: 'rowan'
method: RowanMethodService
rowanIsExtension

	^Rowan projectTools browser isExtensionMethod: selector asString forClassNamed: className asString isMeta: meta
%

category: 'rowan'
method: RowanMethodService
rowanProjectName

	^projectName
%

category: 'client commands'
method: RowanMethodService
runTest: testSelector inClassName: theClassName

	| sunitTestResult |
	sunitTestResult := (Rowan image objectNamed: theClassName) run: testSelector asSymbol.
	sunitTestResult errorCount > 0 ifTrue:[testResult := 'error']. 
	sunitTestResult failureCount > 0 ifTrue:[testResult := 'failure']. 
	sunitTestResult passedCount > 0 ifTrue:[testResult := 'passed']. 
	testRunClassName := theClassName. 
	RowanCommandResult addResult: self.
%

category: 'accessing'
method: RowanMethodService
searchString
	^searchString
%

category: 'accessing'
method: RowanMethodService
searchString: object
	searchString := object
%

category: 'Accessing'
method: RowanMethodService
selectedPackageServices

	^selectedPackageServices
%

category: 'Updating'
method: RowanMethodService
selectedPackageServices: collection

	selectedPackageServices := collection
%

category: 'Accessing'
method: RowanMethodService
selector

	^selector
%

category: 'Updating'
method: RowanMethodService
selector: aSymbol

	selector := aSymbol
%

category: 'perform'
method: RowanMethodService
servicePerform: symbol withArguments: collection
  | theCommand |
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  theCommand := command.
  super servicePerform: symbol withArguments: collection.
  theCommand = #'reformatSource'
    ifFalse: [ self update ]
%

category: 'client commands'
method: RowanMethodService
setBreakAt: stepPoint
  | method |
  method := self isUnboundMethod
    ifTrue: [ (Object _objectForOop: oop) homeMethod ]
    ifFalse: [ self gsNMethod ].
  method setBreakAtStepPoint: stepPoint.
  self class breakPointsAreEnabled
    ifFalse: [ self disableBreakAt: stepPoint ].
  self update.
  RowanCommandResult addResult: self
%

category: 'method history'
method: RowanMethodService
setHasMethodHistory
  | answeringService |
  answeringService := RowanAnsweringService new.
  hasMethodHistory := (answeringService basicMethodHistoryFor: self) answer size
    > 1.
%

category: 'initialization'
method: RowanMethodService
setSupersAndSubsFor: theClass

	| theSuper |
	theSuper := theClass superClass. 
	hasSupers := false. 
	[theSuper notNil and:[hasSupers not]] whileTrue:[
		hasSupers := theSuper selectors includes: selector.
		hasSupers ifTrue:[
			comparisonSource := theSuper sourceCodeAt: selector.
			superDisplayString := theSuper name, '>>', selector].
		theSuper := theSuper superClass].
	(organizer allSubclassesOf: theClass thisClass) do:[:cls |
		| aClass |
		aClass := theClass isMeta ifTrue:[cls class] ifFalse:[cls]. 
		(hasSubs := aClass includesSelector: selector) ifTrue:[
		^self]].
%

category: 'Accessing'
method: RowanMethodService
source

	^source
%

category: 'Updating'
method: RowanMethodService
source: string
	
	self updateSource: string
%

category: 'Accessing'
method: RowanMethodService
stepPoints

	"for testing"
	
	^stepPoints
%

category: 'Updating'
method: RowanMethodService
stepPoints: collection

	stepPoints := collection
%

category: 'initialization'
method: RowanMethodService
stepPointsFor: aGsNMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aGsNMethod _sourceOffsetsOfSends) ifNil: [^#()].
	list := aGsNMethod homeMethod  _sourceOffsets.
	list := list collect: [:each |
		| index eachSelector |
		eachSelector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [eachSelector := selectors at: index + 1].
		each -> eachSelector.
	].
	^list
%

category: 'Accessing'
method: RowanMethodService
testResult

	^testResult
%

category: 'updates'
method: RowanMethodService
update
	self isUnboundMethod ifFalse:[
		self wasRecycled ifTrue:[oop := self gsNMethod asOop].
		self wasDeleted ifTrue:[
			self updateType: #methodsRemoved:. 
			^RowanCommandResult addResult: self. ].  "removed method"
		oop ifNil: [oop := self gsNMethod asOop]].
	self 
		initialize: (Object _objectForOop: oop) 
		organizer: organizer.
	RowanCommandResult addResult: self.
%

category: 'updates'
method: RowanMethodService
updateLatest
  | theClass compiledMethod |
  theClass := (RowanClassService new name: className) theClass.
  theClass ifNil: [ ^ self ].
  compiledMethod := theClass compiledMethodAt: selector otherwise: nil.
  compiledMethod ifNil: [ ^ self ].
  oop := compiledMethod asOop.
  super updateLatest
%

category: 'private'
method: RowanMethodService
updateSource: string

	source := self class removeUnicodeFromSource: string
%

category: 'testing'
method: RowanMethodService
wasDeleted
	selector isNil ifTrue:[^false].
	^self gsNMethod isNil
%

category: 'testing'
method: RowanMethodService
wasRecycled
	(oop notNil and:[self gsNMethod asOop ~= oop]) ifTrue:[^true].
	^false
%

! Class implementation for 'RowanPackageService'

!		Class methods for 'RowanPackageService'

category: 'instance creation'
classmethod: RowanPackageService
forPackageNamed: aName

	| inst |
	inst := self new.  
	inst name: aName.
	aName isNil ifFalse:[
		inst isDirty. "lazy initialize"].
	inst setDefaultTemplate. 
	inst updateProjectName.
	^inst
%

!		Instance methods for 'RowanPackageService'

category: 'comparing'
method: RowanPackageService
= packageService
	(packageService isKindOf: RowanPackageService) ifFalse:[^false].
	^name = packageService name
%

category: 'testing'
method: RowanPackageService
arePackageAndProjectClean
  ^ self rowanDirty not and: [ self projectIsDirty not ]
%

category: 'rowan'
method: RowanPackageService
changes
   "diffForPackageName: not implemented yet"
 
 self error: 'this message is no longer supported'
%

category: 'other'
method: RowanPackageService
classes: collection

	classes := collection
%

category: 'client commands'
method: RowanPackageService
classHierarchy
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
%

category: 'commands support'
method: RowanPackageService
classHierarchy: theClasses
  hierarchyServices := super classHierarchy: theClasses.
  ^ hierarchyServices
%

category: 'rowan'
method: RowanPackageService
createPackageNamed: aString inProject: projName
	| projectService | 
	name := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projName.  
	projectDefinition addPackageNamed: name.
	self projectTools load loadProjectDefinition: projectDefinition.
%

category: 'Updating'
method: RowanPackageService
defaultTemplate: newValue
	defaultTemplate := newValue
%

category: 'rowan'
method: RowanPackageService
definition

	^(Rowan image loadedPackageNamed: name) asDefinition
%

category: 'rowan'
method: RowanPackageService
deletePackage

	self browserTool removePackageNamed: name.
%

category: 'rowan'
method: RowanPackageService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil
%

category: 'comparing'
method: RowanPackageService
hash
	^self name hash
%

category: 'Accessing'
method: RowanPackageService
hierarchyServices

	^hierarchyServices
%

category: 'rowan'
method: RowanPackageService
isDirty

	^isDirty := self rowanDirty
%

category: 'Updating'
method: RowanPackageService
isDirty: boolean

	isDirty := boolean
%

category: 'testing'
method: RowanPackageService
isPackageService

	^true
%

category: 'Accessing'
method: RowanPackageService
jadeite_testClasses

	"for testing" 
	^testClasses
%

category: 'rowan'
method: RowanPackageService
loadedClasses

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClasses
%

category: 'rowan'
method: RowanPackageService
loadedClassExtensions

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClassExtensions
%

category: 'rowan'
method: RowanPackageService
loadedClassHandles
	| loadedClasses |
	loadedClasses := self loadedClasses collect:[:loadedClass | loadedClass handle].
	loadedClasses addAll: (self loadedClassExtensions collect:[:extension | extension handle]).
	^loadedClasses
%

category: 'rowan'
method: RowanPackageService
loadedClassNames

	^self loadedClasses collect:[:loadedClass | loadedClass name]
%

category: 'Accessing'
method: RowanPackageService
name
	^name
%

category: 'Updating'
method: RowanPackageService
name: newValue
	name := newValue
%

category: 'Accessing'
method: RowanPackageService
packageName
	^name
%

category: 'Updating'
method: RowanPackageService
packageName: newValue
	name := newValue
%

category: 'printing'
method: RowanPackageService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'Accessing'
method: RowanPackageService
projectDefinition
	^projectDefinition
%

category: 'Updating'
method: RowanPackageService
projectDefinition: newValue
	projectDefinition := newValue
%

category: 'testing'
method: RowanPackageService
projectIsDirty
  projectName ifNil: [ self updateProjectName ].
  ^ (RowanProjectService new name: projectName) rowanDirty
%

category: 'Accessing'
method: RowanPackageService
projectName
	^projectName
%

category: 'Updating'
method: RowanPackageService
projectName: newValue
	projectName := newValue
%

category: 'client commands'
method: RowanPackageService
removeClass: classService
  self removeClassNamed: classService name.
  self setDefaultTemplate.
  classService updateType: #'removedClass:'.
  classService wasRemoved: true.
  RowanCommandResult addResult: classService.
  RowanBrowserService new packagesWithTests	"sunit browser might need updated"
%

category: 'commands support'
method: RowanPackageService
removeClassNamed: className

	self browserTool removeClassNamed: className.
%

category: 'rowan'
method: RowanPackageService
rowanDirty

	^(RwPackage newNamed: name) isDirty
%

category: 'rowan'
method: RowanPackageService
rowanProjectName

	^projectName
%

category: 'other'
method: RowanPackageService
selectedClass
	
	^selectedClass
%

category: 'Accessing'
method: RowanPackageService
selectedClass: classService
	selectedClass := classService.
	classService selectedPackageServices: (Array with: self)
%

category: 'perform'
method: RowanPackageService
servicePerform: symbol withArguments: collection
  | wasClean |
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  wasClean := self arePackageAndProjectClean.
  super servicePerform: symbol withArguments: collection.
  self update.
  wasClean
    ifTrue: [ self updateProject ]
%

category: 'commands support'
method: RowanPackageService
services: services from: levels expand: toExpand
  ^ self
    services: services
    from: levels
    expand: toExpand
    classes: (classes collect: [ :classService | classService theClass ])
%

category: 'client commands'
method: RowanPackageService
setDefaultTemplate

	defaultTemplate := self genericClassCreationTemplate.
%

category: 'client commands'
method: RowanPackageService
testClasses
  organizer := ClassOrganizer new.
  testClasses := Set new.
  self loadedClasses
    valuesDo: [ :loadedClass | 
      | cls |
      cls := loadedClass handle.
      (cls inheritsFrom: TestCase)
        ifTrue: [ 
          cls isAbstract
            ifFalse: [ 
              | classService |
              classService := RowanClassService basicForClassNamed: cls name.
              testClasses add: classService ] ] ].
  self loadedClassExtensions
    valuesDo: [ :loadedClass | 
      | cls |
      cls := loadedClass handle.
      (cls inheritsFrom: TestCase)
        ifTrue: [ 
          cls isAbstract
            ifFalse: [ 
              | classService |
              classService := RowanClassService basicForClassNamed: cls name.
              testClasses add: classService ] ] ].
  updateType := #'testClasses:browser:'.
  testClasses := testClasses asArray.
  testClasses do: [ :classService | classService update ].
  RowanCommandResult addResult: self
%

category: 'updates'
method: RowanPackageService
update
	Rowan image loadedPackageNamed: name ifAbsent: [^self]. 
	classes := (self loadedClassNames keys collect:[:string | RowanClassService minimalForClassNamed: string]) asArray.
	classes addAll: (self loadedClassExtensions keys collect:[:string | 
		| classService | 
		classService := (RowanClassService minimalForClassNamed: string) 
			isExtension: true.
		(Rowan image loadedClassForClass: classService theClass ifAbsent:[]) 
			ifNotNil:[:cls | classService definedPackageName: cls packageName].
		classService]).
	classes do: [:clsService | clsService packageName: self name]. 
	self isDirty. 
	projectName := (Rowan image loadedPackageNamed: name) projectName.
	RowanCommandResult addResult: self
%

category: 'updates'
method: RowanPackageService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: classes size do:[:index |
		| classesService |
		classesService := classes at: index. 
		classesService = updatedService ifTrue:[
			classes at: index put: updatedService
		]].
%

category: 'updates'
method: RowanPackageService
updateProject
	| projectService |

	projectService := RowanProjectService new name: projectName. 
	projectService update.
	RowanCommandResult addResult: projectService.
%

category: 'updates'
method: RowanPackageService
updateProjectName

	projectName := (Rowan image loadedPackageNamed: name) projectName.
%

category: 'testing'
method: RowanPackageService
wasDeleted

	^(Rowan image loadedPackageNamed: name
			ifAbsent: []) isNil
%

! Class implementation for 'RowanProcessService'

!		Class methods for 'RowanProcessService'

category: 'instance creation'
classmethod: RowanProcessService
new

	"in general use one of the other constructor methods.
	Needed for STON replication"
	^super new
%

category: 'instance creation'
classmethod: RowanProcessService
onActiveProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'active';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onReadyProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'ready';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onSuspendedProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'suspended';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onWaitingProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'waiting';
		yourself
%

!		Instance methods for 'RowanProcessService'

category: 'initialize'
method: RowanProcessService
initialize

	frames := Array new
%

category: 'initialize'
method: RowanProcessService
initialize: aGsProcess status: aString

	| theOrganizer |
	theOrganizer := ClassOrganizer new. 
	frames := Array new: aGsProcess stackDepth.
	1 to: aGsProcess stackDepth do: [:i | 
		frames at: i put: (RowanFrameService process: aGsProcess level: i organizer: theOrganizer).
	].
	oop := aGsProcess asOop.  
	status := aString.
%

category: 'accessing'
method: RowanProcessService
oop
	^oop
%

category: 'accessing'
method: RowanProcessService
oop: object
	oop := object
%

category: 'perform'
method: RowanProcessService
servicePerform: symbol withArguments: collection
	^self perform: symbol withArguments: collection.
%

category: 'accessing'
method: RowanProcessService
status
	^status
%

category: 'accessing'
method: RowanProcessService
status: object
	status := object
%

category: 'updating'
method: RowanProcessService
update

	self initialize: (Object _objectForOop: oop) status: 'suspended'.
	RowanCommandResult addResult: self.
%

! Class implementation for 'RowanProjectService'

!		Class methods for 'RowanProjectService'

category: 'instance creation'
classmethod: RowanProjectService
newNamed: aString

	| inst |
	inst := self new.
	inst name: aString.
	inst refresh.
	^inst
%

!		Instance methods for 'RowanProjectService'

category: 'comparing'
method: RowanProjectService
= projectService
	^projectService isProjectService ifTrue: [name = projectService name] ifFalse: [^false]
%

category: 'initialization'
method: RowanProjectService
basicRefresh
	name = Rowan unpackagedName ifTrue:[
		isLoaded := false.
		RowanBrowserService new updateDictionaries. ^self]. 
	(isLoaded := self projectIsLoaded) ifFalse:[
		existsOnDisk := false. 
		updateType := #removedProject:. 
		^RowanCommandResult addResult: self]. 
	isDirty := self isDirty. 
	self setExistsOnDisk.
	isSkew := self isSkew.
	sha := self rowanSha.
	branch := self rowanBranch.
	projectUrl := self rowanProjectUrl. 
	rowanProjectsHome := System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME' .
	isDiskDirty := self isGitDirty.
	RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanProjectService
branch

	name isNil ifTrue:[^String new].
	^self rwProject currentBranchName
%

category: 'accessing'
method: RowanProjectService
branch: anObject

	branch := anObject
%

category: 'client commands'
method: RowanProjectService
changes

	| jadeServer projectNames |
	jadeServer := Rowan platform jadeServerClassNamed: #JadeServer. 
	changes := Array new. 
	projectNames := name ifNil: [ Rowan projectNames ] ifNotNil: [ { name } ].
	projectNames do: [:aProjectName | 
		(Rowan projectTools diff
			patchesForProjectNamed: aProjectName) do: [:assoc | 
				"key is packageName, value is a CypressPatch"
				| patch |
				patch := assoc value.
				changes add:(jadeServer new
					_mcDescriptionOfPatch: patch
					baseName: 'closest ancestor'
					alternateName: nil) ] ].
	self refresh.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanProjectService
checkout: branchName

	| project branches |

	project := self rwProject. 
	branches := Rowan gitTools gitcheckoutIn: project repositoryRootPath asFileReference with: branchName.
	^branches
%

category: 'client commands'
method: RowanProjectService
checkoutTag: tagName
  Rowan gitTools gitcheckoutIn: self repositoryRootPath with: tagName
%

category: 'client commands'
method: RowanProjectService
commitWithMessage: message
	
	Rowan projectTools write writeProjectNamed: name.
	Rowan projectTools commit
		commitProjectNamed: name
		message: message.
%

category: 'examples'
method: RowanProjectService
createProjectNamed: projectName 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.
%

category: 'examples'
method: RowanProjectService
createProjectNamed: projectName in: symbolDictionaryName

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].
%

category: 'replication'
method: RowanProjectService
excludedInstVars

	^ super excludedInstVars, #( #rwProject)
%

category: 'accessing'
method: RowanProjectService
existsOnDisk

	^existsOnDisk
%

category: 'comparing'
method: RowanProjectService
hash
	^self name hash
%

category: 'initialization'
method: RowanProjectService
initialize

	super initialize. 
	packages := Array new.
	isDiskDirty := false.
%

category: 'rowan'
method: RowanProjectService
isDirty
	name isNil
		ifTrue: [ ^ false ].
	^ self rwProject isDirty
%

category: 'accessing'
method: RowanProjectService
isDirty: aBoolean

	isDirty := aBoolean.
%

category: 'client commands'
method: RowanProjectService
isGitDirty
  "From https://github.com/GemTalk/Jadeite/issues/323#issuecomment-442545934"

  [ 
  ^ (Rowan gitTools gitstatusIn: self repositoryRootPath with: '--porcelain')
    isEmpty not ]
    on: Error
    do: [ :ignored | ^ false ]
%

category: 'rowan'
method: RowanProjectService
isSkew
	| repositorySha |
	name isNil ifTrue:[^false].
	self existsOnDisk ifFalse:[^false]. 
	repositorySha := [self repositorySha] on: Error do:[:ex | repositorySha := 'not on disk'].
	^self sha ~= repositorySha
%

category: 'client commands'
method: RowanProjectService
loadProjectNamed: aName

	[Rowan projectTools load loadProjectNamed: aName] 
		on: Warning
		do: [ :ex | Transcript cr; show: ex description. ex resume ].
	RowanBrowserService new updateProjects.
%

category: 'rowan'
method: RowanProjectService
log

	^self rwProject commitLog: 25
%

category: 'accessing'
method: RowanProjectService
name

	^name
%

category: 'accessing'
method: RowanProjectService
name: anObject

	name := anObject
%

category: 'client commands'
method: RowanProjectService
newGitProject: url root: rootPath useSsh: useSsh
	"set useSsh to false to clone using https:"

	Rowan projectTools clone
		cloneSpecUrl: url
		gitRootPath: rootPath
		useSsh: useSsh.
	(RowanBrowserService new organizer: organizer) updateProjects.
%

category: 'rowan'
method: RowanProjectService
packageNames
	"if no project is selected, return all package names"
	^name isNil ifTrue:[
		"list of visible packageNames for current user"
		Rowan image packageNames ]
	ifFalse:[
		"list of visible packageNames for current user and named project"
		Rowan image packageNamesForLoadedProjectNamed: name ]
%

category: 'rowan'
method: RowanProjectService
packageNameString
		"return a string showing the package names for a project"

	| ws packageNames |
	ws := WriteStream on: String new.
	ws
		nextPutAll: 'Packages for project: ' , name;
		cr.
	packageNames := Set new.
	packageNames addAll: (Rowan image loadedProjectNamed: name) packageNames.
	packageNames asSortedCollection do: 
			[:packageName |
			ws
				cr; 
				tab;
				nextPutAll: packageName
			].
	^ws contents
%

category: 'accessing'
method: RowanProjectService
packageServices

	^self packageNames collect:[:packageName | RowanPackageService forPackageNamed: packageName]
%

category: 'client commands'
method: RowanProjectService
performGitCommand: gitCommand with: argsString
  | project |
  project := RwProject newNamed: name.
  Rowan gitTools
    performGitCommand: gitCommand
    in: project repositoryRootPath
    with: argsString.
  RowanCommandResult addResult: self
%

category: 'printing'
method: RowanProjectService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'testing'
method: RowanProjectService
projectIsLoaded

	^(Rowan image
		loadedProjectNamed: name
		ifAbsent: []) notNil
%

category: 'rowan'
method: RowanProjectService
projects

		^Rowan projectNames collect: 
			[:string |
			| service |
			service := self class new name: string. 
			service 
				sha: service rowanSha;
				branch: service rowanBranch;
				isDirty: service rowanDirty]
%

category: 'accessing'
method: RowanProjectService
projectUrl

	^projectUrl
%

category: 'accessing'
method: RowanProjectService
projectUrl: anObject

	projectUrl := anObject
%

category: 'client commands'
method: RowanProjectService
pullFromGit

	| project |
	project := self rwProject. 
	Rowan gitTools
		gitpullIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName
%

category: 'client commands'
method: RowanProjectService
pushToGit

	| project |
	project := self rwProject. 
	Rowan gitTools
		gitpushIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName
%

category: 'initialization'
method: RowanProjectService
refresh
	self basicRefresh. 
	isLoaded ifTrue:[
		packages := self packageServices].
%

category: 'client commands'
method: RowanProjectService
reloadProject
  [ Rowan projectTools load loadProjectNamed: name ]
    on: Warning
    do: [ :ex | 
      Transcript
        cr;
        show: ex description.
      ex resume ].
  self update.
  RowanCommandResult addResult: self.
  RowanBrowserService new packagesWithTests
%

category: 'rowan'
method: RowanProjectService
removeProjectNamed: projectName
   "remove project"
 
    (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
      ifNotNil: [ :project | Rowan image _removeLoadedProject: project ]
%

category: 'rowan'
method: RowanProjectService
repositoryRootPath

	^(RwProject newNamed: name) repositoryRootPath
%

category: 'rowan'
method: RowanProjectService
repositorySha
	^ self rwProject repositoryCommitId
%

category: 'rowan'
method: RowanProjectService
rowanBranch
	
	name isNil ifTrue:[^String new].
	^ [  self rwProject currentBranchName ] on: Error do: [:ex | ^'ERROR getting repository branch' ]
%

category: 'rowan'
method: RowanProjectService
rowanDirty

	^(RwProject newNamed: name) isDirty
%

category: 'rowan'
method: RowanProjectService
rowanProjectName

	^name
%

category: 'rowan'
method: RowanProjectService
rowanProjectUrl

	^(RwProject newNamed: name) projectUrl
%

category: 'rowan'
method: RowanProjectService
rowanSha

	name isNil ifTrue:[^0].
	^(RwProject newNamed: name) loadedCommitId
%

category: 'rowan'
method: RowanProjectService
rowanSkew

	^self sha ~= self repositorySha
%

category: 'accessing'
method: RowanProjectService
rwProject
	^ rwProject ifNil: [ rwProject := RwProject newNamed: name ]
%

category: 'perform'
method: RowanProjectService
servicePerform: symbol withArguments: collection
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
  self update
%

category: 'rowan'
method: RowanProjectService
setExistsOnDisk
	"might be a better test than #repositorySha for
	determining if a project exists on disk." 

	existsOnDisk := (RwProject newNamed: name) existsOnDisk.
%

category: 'accessing'
method: RowanProjectService
sha

	name isNil ifTrue:[^0].
	^self rwProject loadedCommitId
%

category: 'accessing'
method: RowanProjectService
sha: anObject

	"because skew is intimately associated with sha
	set it here" 
	sha := anObject.
	isSkew := self rowanSkew
%

category: 'rowan'
method: RowanProjectService
unload

	| loadedProject |
	loadedProject := Rowan image loadedProjects select:[:proj | proj name = name].
%

category: 'update'
method: RowanProjectService
update
	self refresh.
%

category: 'update'
method: RowanProjectService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: packages size do:[:index |
		| packageService |
		packageService := packages at: index. 
		packageService = updatedService ifTrue:[
			packages at: index put: updatedService
		]].
%

category: 'testing'
method: RowanProjectService
wasDeleted
  ^ self projectIsLoaded not
%

category: 'client commands'
method: RowanProjectService
write
	Rowan projectTools write writeProjectNamed: name
%

category: 'accessing'
method: RowanProjectService
_isSkew

	^isSkew
%

! Class implementation for 'RowanQueryService'

!		Instance methods for 'RowanQueryService'

category: 'private'
method: RowanQueryService
basicBreakpointMethods
  | bpMethods bpArray |
  bpMethods := Array new.
  bpArray := (GsNMethod _breakReport: true) at: 2.
  bpArray do: [ :array | bpMethods add: (array at: 5) ].
  ^ (self methodServicesFrom: bpMethods) asSet asArray
%

category: 'queries'
method: RowanQueryService
breakPointMethods
  queryResults := self basicBreakpointMethods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
browseClassReferences: className

	| methods |
	methods := organizer referencesTo: className asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
browseReferencesTo: symbol

	| methods |
	methods := organizer referencesTo: symbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
%

category: 'private'
method: RowanQueryService
defaultProjectLogSize

	^100
%

category: 'queries'
method: RowanQueryService
gitTagListUsing: projectService
  | answerString readStream |
  Rowan gitTools
    performGitCommand: 'fetch'
    in: projectService repositoryRootPath
    with: '--tags'.
  answerString := Rowan gitTools
    performGitCommand: 'tag'
    in: projectService repositoryRootPath
    with: '--sort=-taggerdate'.
  queryResults := Array new.
  readStream := ReadStream on: answerString.
  [ readStream atEnd ]
    whileFalse: [ queryResults add: (readStream upTo: Character lf) ].
  RowanCommandResult addResult: self
%

category: 'queries'
method: RowanQueryService
hierarchyImplementorsOf: selector inClass: className

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (organizer allSubclassesOf: behavior). 
	methods := organizer implementorsOf: selector in: classes.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
hierarchySendersOf: selector inClass: className

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (organizer allSubclassesOf: behavior). 
	methods := organizer sendersOf: selector in: classes.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
implementorsOf: selector

	| methods |
	methods := organizer implementorsOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
instVarReaders: instVarName in: className
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsRead includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
instVarWriters: instVarName in: className
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsWritten includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
literalReferences: string

	| methods compilationResult |
	compilationResult := string evaluate.  
	methods := organizer referencesToLiteral: compilationResult.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
methodsContaining: string
  | methods sorted |
  methods := organizer substringSearch: string.
  sorted := SortedCollection
    sortBlock: [ :x :y | 
      x className = y className
        ifTrue: [ x selector < y selector ]
        ifFalse: [ x className < y className ] ].
  1 to: methods first size do: [ :index | 
    | methodService |
    methodService := self methodServiceFrom: (methods first at: index).
    methodService
      firstReference: ((methods at: 2) at: index);   "<<<< FIX HERE"
      searchString: string.
    sorted add: methodService ].
  queryResults := sorted asArray.
  self returnQueryToClient
%

category: 'query support'
method: RowanQueryService
methodServiceFrom: method
  ^ (self methodServicesFrom: (Array with: method)) first
%

category: 'query support'
method: RowanQueryService
methodServicesFrom: methods

	| sorted | 
	sorted := SortedCollection sortBlock: [:x :y | x className = y className ifTrue:[x selector < y selector] ifFalse:[x className < y className]].
	sorted addAll: (methods collect:[:gsNMethod | 
			RowanMethodService forSelector: gsNMethod selector class: gsNMethod inClass thisClass meta: gsNMethod inClass isMeta organizer: organizer]).
	^sorted asArray
%

category: 'queries'
method: RowanQueryService
projectBranches: projectName

	| project  |
	project := (RwProject newNamed: projectName). 
	queryResults := Rowan gitTools gitbranchIn: project repositoryRootPath with: ''.
	RowanCommandResult addResult: self
%

category: 'queries'
method: RowanQueryService
projectLog: projectName

	queryResults := (Rowan projectNamed: projectName) commitLog: self defaultProjectLogSize.
	RowanCommandResult addResult: self.
%

category: 'queryResults'
method: RowanQueryService
queryResults

	"for tests. So far, not needed on server." 

	^queryResults
%

category: 'private'
method: RowanQueryService
returnQueryToClient

	queryResults do:[:service |
		RowanCommandResult addResult: service].
	RowanCommandResult addResult: self.
%

category: 'queries'
method: RowanQueryService
sendersOf: selector

	| methods |
	methods := organizer sendersOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
%

category: 'queryResults'
method: RowanQueryService
setFirstReferenceUsing: results and: methods
  results
    do: [ :methodService | 
      | gsMethod index |
      gsMethod := methods first
        detect: [ :meth | 
          meth selector = methodService selector
            and: [ meth inClass thisClass name asString = methodService className ] ]
        ifNone: [ 
          methodService firstReference: nil.
          nil ].
      gsMethod
        ifNotNil: [ 
          index := methods first indexOf: gsMethod.
          methodService firstReference: (methods last at: index) ] ]
%

category: 'ston'
method: RowanQueryService
stonOn: stonWriter   
	| instanceVariableNames |
	instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
	stonWriter writeObject: self
		streamMap: 
			[:dictionary |
			instanceVariableNames do: 
					[:each |
					(self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
						ifNotNil: [:value | dictionary at: each asSymbol put: value]
						ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
%

category: 'update'
method: RowanQueryService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 
	1 to: queryResults size do:[:index |
		| service |
		service := queryResults at: index. 
		service = updatedService ifTrue:[ 
			queryResults at: index put: updatedService
		]].
%

! Class implementation for 'RowanVariableService'

!		Class methods for 'RowanVariableService'

category: 'other'
classmethod: RowanVariableService
oop: anInteger key: nameString value: valueString className: classNameString

	^self basicNew
		oop: anInteger key: nameString value: valueString className: classNameString;
		yourself
%

!		Instance methods for 'RowanVariableService'

category: 'other'
method: RowanVariableService
oop: anInteger key: nameString value: valueString className: classNameString

	oop := anInteger.
	key := nameString.
	value := valueString.
	className := classNameString.
%

! Class extensions for 'RowanMethodService'

!		Instance methods for 'RowanMethodService'

category: '*rowan-services-core-37x'
method: RowanMethodService
_initializeBreakPointsFor: theMethod
  "Answers an Array stepPoints - _allBreakpoints array size changed in 3.7.0"
  | list |
  list := OrderedCollection new.
  theMethod _allBreakpoints
    ifNil: [ ^ OrderedCollection new ]
    ifNotNil: [ :anArray | 
      1 to: anArray size by: 4 do: [ :i | 
        list
          add:
            (theMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)) ] ].
  ^ list asOrderedCollection
%

! Class extensions for 'RowanProjectService'

!		Instance methods for 'RowanProjectService'

category: '*rowan-services-corev2'
method: RowanProjectService
addPackageNamed: packageName toComponentNamed: componentName

	Rowan image loadedPackageNamed: packageName ifAbsent: [
		self browserTool addPackageNamed: packageName toComponentNamed: componentName andProjectNamed: name. 
		self update.
		^self answer: #added.].
	self answer: #duplicatePackage
%

! Class extensions for 'RwGsPlatform'

!		Instance methods for 'RwGsPlatform'

category: '*rowan-services-extensions'
method: RwGsPlatform
answeringServiceClass
  ^ RowanAnsweringService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
browserServiceClass

	^ RowanBrowserService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
classServiceClass

	^ RowanClassService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
commandResultClass

	^ RowanCommandResult
%

category: '*rowan-services-extensions'
method: RwGsPlatform
jadeServerClassNamed: className

	| jadeClasses |
	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
%

category: '*rowan-services-extensions'
method: RwGsPlatform
loggingServiceClass

	^ RowanLoggingService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
methodServiceClass

	^ RowanMethodService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
packageServiceClass

	^ RowanPackageService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
projectServiceClass

	^ RowanProjectService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClass

	^ RowanService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClasses
	"Explicitly add each class rather than sending #allSubclasses so
	that users other than SystemUser have visibility. Visibility in Rowan
	is determined at compile time. See STONReader>>lookupClass:"
	
	| array |
	array := Array with: RowanService. 
	array add: RowanAnsweringService; 
		add: RowanAutoCommitService; 
		add: RowanBrowserService;
		add: RowanClassService; 
		add: RowanDebuggerService; 
		add: RowanDictionaryService; 
		add: RowanFrameService;
		add: RowanInspectorService;
		add: RowanLoggingService;
		add: RowanMethodService;
		add: RowanPackageService; 
		add: RowanProcessService;
		add: RowanProjectService;
		add: RowanQueryService;
		add: RowanTestService;
		add: RowanVariableService.
	^array
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClassFor: className

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone:[]
%


		add: RowanTestServiceServer;
		add: RowanVariableServiceServer.
	^ array
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClassFor: className

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone:[]
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClassFor: className ifAbsent: absentBlock

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone: absentBlock
%

category: '*rowan-services-extensions'
method: RwGsPlatform
specificationServiceClass
  ^ RowanLoadSpecService
%

