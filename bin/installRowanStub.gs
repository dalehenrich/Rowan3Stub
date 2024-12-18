#!/usr/bin/env superdoit_topaz
#
# to run as superdoit script with $GEMSTONE set :
#		./installRowanStub.gs -I <path-to-topazini> -L
# 
# as SystemUser
login

set INPUTPAUSEONERROR on

#
#	these 4 methods should be in GemStone-Interactions-Kernel package in Rowan 3 and should
#		be when we hit masterV3.3
#
method: CharacterCollection
withoutGemstoneLineEndings

	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Remove each occurence. "

	| cr lf crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf
		add: cr asciiValue;
		add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ 
	lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [ 0 ].
	lineEndPos ~= 0 ]
		whileTrue: [ 
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: self
				startingAt: inPos.
			outPos := newOutPos - 1.

			((self at: lineEndPos) = cr
				and: [ lineEndPos < self size and: [ (self at: lineEndPos + 1) = lf ] ])
				ifTrue: [ 
					"CRLF ending"
					inPos := lineEndPos + 2 ]
				ifFalse: [ 
					"CR or LF ending"
					inPos := lineEndPos + 1 ] ].	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: self
		startingAt: inPos.

	^ outString copyFrom: 1 to: newOutPos - 1
%
method: CharacterCollection
indexOfAnyOf: aByteArray startingAt: start ifAbsent: aBlock

	"returns the index of the first character in the given set, starting from start"

	| ans |
	ans := self class
		findFirstInString: self
		inSet: aByteArray asByteArray byteArrayMap
		startingAt: start.
	ans = 0
		ifTrue: [ ^ aBlock value ]
		ifFalse: [ ^ ans ]
%
method: ByteArray
byteArrayMap

	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	| map |
	map := ByteArray new: 256 withAll: 0.
	self do: [ :ascii | map at: ascii + 1 put: 1 ].
	^ map
%
classmethod: SequenceableCollection
new: size withAll: value

	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^ (self new: size)
		atAllPut: value;
		yourself
%

# install JadeiteForPharo support in a non-Rowan stone

run
(Published at: #Rowan ifAbsent: [])
	ifNotNil: [ self error: 'Rowan is already installed!!' ].
Published at: #Rowan put: nil.	"make the compiler happy"
%

run
| symbolList |
symbolList := GsCurrentSession currentSession symbolList.
#( #RowanKernel) "needed by GemStoneInteractions"
  do: [:symbolName | 
    (symbolList resolveSymbol: symbolName) ifNotNil: [:val | System waitForDebug ] ifNil: [
      | newDict size |
      newDict := SymbolDictionary new
        name: symbolName;
        objectSecurityPolicy: symbolList objectSecurityPolicy;
        yourself.
      size := System myUserProfile symbolList size.
      System myUserProfile insertDictionary: newDict at: size + 1 .
      GsFile gciLogServer:'created ', symbolName . 
] ]. 
%

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Announcements.gs
input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RemoteServiceReplication.gs

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/GemStoneInteractions.gs

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs

# install Monticello package support for Rowan3Stub
run
| filePath |
(System gemEnvironmentVariable: 'ROWAN_STUB_EXTENT_TYPE') = 'base'
	ifTrue: [
 		filePath := '$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubBase.gs' asFileReference pathString.
		GsFileIn fromServerPath: filePath ].
((System gemEnvironmentVariable: 'ROWAN_STUB_EXTENT_TYPE') = 'seaside' or: [(System gemEnvironmentVariable: 'ROWAN_STUB_EXTENT_TYPE') = 'tode'])
	ifTrue: [
		filePath := '$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubMonticello.gs' asFileReference pathString.
		GsFileIn fromServerPath: filePath ].
Published at: #Rowan put: Rowan3Stub new.
Published at: #STON put: (RowanKernel_tonel at: #STON).
%

# the following 4 methods cannot be packaged, since they conflict with the Rowan implementation
method: Behavior
rowanPackageName
	^  '(NONE)'
%
method: Behavior
rowanProjectName
	^  '(NONE)'
%
method: GsNMethod
rowanPackageName
	^  '(NONE)'
%
method: GsNMethod
rowanProjectName
	^  '(NONE)'
%

run
(TestCase
	subclass: 'TestCase1'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-Stubs';
		immediateInvariant.
%
method: TestCase1
testError

	self foo
%
method: TestCase1
testFail

	self assert: false
%
method: TestCase1
testPassing

	self assert: true
%


run
(Object
	subclass: 'TestClass1'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-Stubs';
		immediateInvariant.
%

run
(TestClass1
	subclass: 'TestClass2'
	instVarNames: #(name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-Stubs';
		immediateInvariant.
%
method: TestClass2
name
	^name
%

run
(Object
	subclass: 'RwGsDummy'
	instVarNames: #(name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-Stubs';
		immediateInvariant.
%
classmethod: RwGsDummy
named: aSymbol
^self new name: aSymbol; yourself
%

method: RwGsDummy
name: aSymbol
name := aSymbol
%

method: RwGsDummy
handles: arg
System waitForDebug.
%

run
(Object
	subclass: 'RwGsPlatform'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-Stubs';
		immediateInvariant.
%

run
  | session symbolList symbolName | 
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.

	symbolName := #RowanClientServices.
    (symbolList resolveSymbol: symbolName) ifNil: [
      | newDict size |
      newDict := SymbolDictionary new
        name: symbolName;
        objectSecurityPolicy: symbolList objectSecurityPolicy;
        yourself.
      size := System myUserProfile symbolList size.
      System myUserProfile insertDictionary: newDict at: size + 1 .
      GsFile gciLogServer:'created ', symbolName .  ].  
%

run
	#(RwExecuteClassInitializeMethodsAfterLoadNotification RwPerformingUnpackagedEditNotification RwPackage RBParser RwMethodDefinition RwProject RwSemanticVersionNumber RwPlatformSubcomponent RwSubcomponent RwSpecification RwClassDefinition) 
		do: [:symbolName |
			Globals at: symbolName put: (RwGsDummy named: symbolName) ].
%

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RowanClientServicesV3.gs
input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubServices.gs

#
# PATCHES to RowanClientServices methods to enable the use of `System waitForDebug`
#

category: 'rsr'
method: RowanService
executeCommand
	"RSR -> RowanServices primary api."

	self checkForDeadProcesses.
	self setDebugActionBlock.	"<===== patch here ====="
	[ 
	Rowan commandResultClass initializeResults.
	[ 
	updateType := nil.	"Update type is only for returned commands"
	command ifNil: [^self]. 
	self servicePerform: command withArguments: commandArgs ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	updates := Rowan commandResultClass results.
	self postCommandExecution ]
		on: Exception
		do: [ :ex | 
			GsFile
				gciLogServer:
					DateTime now asStringMs , ' {'
						, Processor activeProcess identityHash printString , '}  - got error: '
						, ex printString.
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ].
	^ self
%

#
# overwrites of RowanClassService methods that will need to change for JfPwoR
#
category: 'Rowan3 stub'
method: RowanClassService
classComment: string
	| theClass |
	theClass := self theClass. 
	theClass comment: string.
%

category: 'Rowan3 stub'
method: RowanClassService
classCreationTemplateUsing: packageNames
	"copying RwPrjBrowserToolV2>>classCreationTemplateForClass:hybridBrowser: with one change for performance"

	| result anArray lfsp newByteSubclass civs superClass className |
	result := String new.
	superClass := self theClass superclass.
	className := self theClass name asString.
	superClass
		ifNil: [ result addAll: 'nil' ]
		ifNotNil: [ result addAll: superClass name asString ].
	lfsp := Character lf asString tab.
	newByteSubclass := false.
	(self theClass isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			result 
				addAll: ' byteSubclass: ''';
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			(self theClass isIndexable and: [ superClass isIndexable not ])
				ifTrue: [ 
					result 
						addAll: ' indexableSubclass: ''';
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					result 
						addAll: ' subclass: ''';
						addAll: className;
						addLast: $' ] ].
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll:
						(self theClass _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	self theClass _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := self theClass class allInstVarNames.
	civs removeFrom: 1 to: self theClass class superClass instSize.
	civs
		do: [ :civName | 
			result addLast: $ .
			(civName includesValue: $')
				ifTrue: [ result addAll: civName _asSource ]
				ifFalse: [ result addAll: civName ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: '.
	result addAll: '#()'.	"ignored for now"
 
			"if the class is unpackaged, then we need to provide for the specification of symbol dictionary into which the class will be installed"
			result
				addAll: lfsp;
				addAll: 'inDictionary: '.
			anArray := Rowan image symbolList dictionariesAndSymbolsOf: self theClass.
			anArray isEmpty
				ifTrue: [ result addAll: '''''' ]
				ifFalse: [ result addAll: ((anArray at: 1) at: 1) name asString ].
	result
		add: lfsp;
		add: self theClass _optionsArrayForDefinition.
	result add: Character lf.
	^ result
%

category: 'Rowan3 stub'
method: RowanClassService
removeCategories: theCategories
	| theClass  | 
	self refreshFrom: self theClass. 
	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theCategories do: [:category |
		theClass removeCategory: category.
		].
	shouldUpdate := true.
%

#
# overwrites of RowanPackageService methods that will need to change for JfPwoR
#
category: 'Rowan3 stub'
method: RowanPackageService
updateProjectName

	projectName := 'Monticello'.
%
category: 'Rowan3 stub'
classmethod: RowanPackageService
forPackageNamed: aName
	| inst |
	inst := self new.
	inst name: aName.
	aName isNil
		ifFalse: [ inst updateIsDirty ].
	inst setDefaultTemplate.
	inst updateProjectName.
	^ inst
%

#
# overwrites of RowanProjectService methods that will need to change for JfPwoR
#
category: 'Rowan3 stub'
method: RowanProjectService
rowanDirty
	"placeholder..."
	^ false
%
category: 'Rowan3 stub'
method: RowanProjectService
rwProject
	^ rwProject
		ifNil: [ 
			rwProject := Rowan image loadedProjectNamed: name. 
			projectOop := rwProject asOop.
			rwProject ]
%

#
#	PATCHES extracted from Rowan-GemStone-Kernel package 
#		(trying to avoid bringing in the whole kit and kaboodle if possible)
#

# SequenceableCollection>>copyUpTo:
category: 'Rowan3Stub' 
method: SequenceableCollection
copyUpTo: anObject

	"Answer all elements up to but not including anObject. If there
  is no such object, answer a copy of the receiver."

	| idx |
	idx := self indexOf: anObject startingAt: 1.
	idx == 0
		ifTrue: [ ^ self copy ]
		ifFalse: [ ^ self copyFrom: 1 to: idx - 1 ]
%

#
# overwrites of JadeServer methods that will need to change for JfPwoR
#

category: 'Rowan3 stub'
method: JadeServer
_describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream packageName: packageName
	aStream
		nextPut: $O;
		tab;
		nextPutAll: packageName;
		tab;
		lf
%

commit
