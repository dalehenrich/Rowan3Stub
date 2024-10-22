# as SystemUser
login

set INPUTPAUSEONERROR on

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
input $ROWAN_PROJECTS_HOME/RemoteServiceReplication/src-gs/bootstrapRSR.gs

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/GemStoneInteractions.gs

input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs

run
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
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	[ [ method := aBehavior
		compileMethod: methodString 
		dictionaries: aSymbolList 
		category: categorySymbol 
		environmentId: 0 ]
			on: CompileError
			do: [:ex | ^ nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume ].
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] 
		on: Error
		do: [:ex | ex return: method -> warnings]
%

#
# overwrites of RowanBrowserService methods that will need to change for JfPwoR
#
category: 'Rowan3 stub'
method: RowanBrowserService
recompileMethodsAfterClassCompilation
	"compileClass: must be run first"

	| theClass classService packageService projectService |
	theClass := [ 
	[ (SessionTemps current at: #'jadeiteCompileClassMethod') _executeInContext: nil ]
		on: CompileWarning , CompileError
		do: [ :ex | 
			(ex isKindOf: CompileError)
				ifTrue: [ 
					| compileErrorService |
					self postCommandExecutionWithoutAutoCommit.
					compileErrorService := RowanCompileErrorServiceServer new.
					compileErrorService gsArguments: ex errorDetails.
					updates := Array with: compileErrorService.
					^ RowanCommandResult addResult: compileErrorService ]
				ifFalse: [ ex resume ] ] ]
		ensure: [ SessionTemps current at: #'jadeiteCompileClassMethod' put: nil ].
	classService := RowanClassService new name: theClass name.
	classService update.
	classService updateSubclasses.
	classService isNewClass: true.	"if nothing else, the dirty state of the package/project services
	should be updated. Would like a less heavy weight solution than this, though."
	RowanCommandResult addResult: classService.
	selectedClass := classService.
	updateType := #'none'.
	self updateSymbols: (Array with: theClass name asString).
	RowanCommandResult addResult: self.
	^classService
%

#
# overwrites of RowanPackageService methods that will need to change for JfPwoR
#
category: 'Rowan3 stub'
method: RowanPackageService
updateProjectName

	projectName := nil.
%
category: 'Rowan3 stub'
classmethod: RowanPackageService
forPackageNamed: ignored
	| inst aName |
	inst := self new.
	inst name: aName.
	aName isNil
		ifFalse: [ inst updateIsDirty ].
	inst setDefaultTemplate.
	inst updateProjectName.
	^ inst
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

commit
