! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'Rowan3BrowserToolsStub'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3ImageStub'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3LoadedClassExtensionStub'
	instVarNames: #(theClass)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3LoadedClassStub'
	instVarNames: #(theClass)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3LoadedPackageStub'
	instVarNames: #(name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3LoadedProjectStub'
	instVarNames: #(name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3PlatformStub'
	instVarNames: #(image)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3ProjectToolsStub'
	instVarNames: #(browserTool)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'Rowan3Stub'
	instVarNames: #(platform projectTools)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

! Class implementation for 'Rowan3BrowserToolsStub'

!		Instance methods for 'Rowan3BrowserToolsStub'

category: 'accessing'
method: Rowan3BrowserToolsStub
classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser
	^ aClass definition
%

category: 'accessing'
method: Rowan3BrowserToolsStub
classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName
	"Returns a description of the receiver using object names taken from the given UserProfile."

	^ Class
		templateForSubclassOf: (GsSession currentSession objectNamed: superclassName)
		category: (category ifNil: [ 'Kernel' ])
%

category: 'accessing'
method: Rowan3BrowserToolsStub
isExtensionMethod: selector forClassNamed: className isMeta: meta
	^ false
%

category: 'accessing'
method: Rowan3BrowserToolsStub
removeMethod: selector forClassNamed: name  isMeta: meta
	| beh |
	beh := Rowan globalNamed: name.
	meta ifTrue: [ beh := beh class ].
	beh removeSelector: selector
%

! Class implementation for 'Rowan3ImageStub'

!		Instance methods for 'Rowan3ImageStub'

category: 'accessing'
method: Rowan3ImageStub
globalNamed: aString
	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self objectNamed: aString
%

category: 'accessing'
method: Rowan3ImageStub
loadedClassExtensionsForClass: class
	"lookup the loadedClassExtensions for the given class"

	^ IdentitySet new
%

category: 'accessing'
method: Rowan3ImageStub
loadedClassForClass: aClass ifAbsent: absentBlock
	^ absentBlock value
%

category: 'accessing'
method: Rowan3ImageStub
loadedPackageNamed: aString
	^ self
		loadedPackageNamed: aString
		ifAbsent: [ self error: 'No package named ' , aString printString , ' found' ]
%

category: 'accessing'
method: Rowan3ImageStub
loadedPackageNamed: aName ifAbsent: absentBlock
	"scan the symbol list a RwLoadedPackage instance of the given name"

	self loadedProjects
		do: [ :loadedProject | 
			(loadedProject loadedPackageNamed: aName ifAbsent: [  ])
				ifNotNil: [ :loadedPackage | ^ loadedPackage ] ].
	^ absentBlock value
%

category: 'accessing'
method: Rowan3ImageStub
loadedProjectNamed: aString

	^ self
		loadedProjectNamed: aString
		ifPresent: [:loadedProject | loadedProject ]
		ifAbsent: [ self error: 'No loaded project named ' , aString printString , ' found' ]
%

category: 'accessing'
method: Rowan3ImageStub
loadedProjectNamed: projectName ifAbsent: absentBlock
	^ self loadedProjects
		detect: [ :each | each name = projectName ]
		ifNone: absentBlock
%

category: 'accessing'
method: Rowan3ImageStub
loadedProjectNamed: aString ifPresent: presentBlock ifAbsent: absentBlock
	"Look up a loaded project in the loaded project registry"

	| loadedProject |
	loadedProject := self loadedProjectNamed: aString ifAbsent: absentBlock.
	^ presentBlock cull: loadedProject
%

category: 'querying'
method: Rowan3ImageStub
objectNamed: aSymbol
	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ self symbolList objectNamed: aSymbol
%

category: 'accessing'
method: Rowan3ImageStub
packageNames
	| packageNames |
	packageNames := Set new.
	self loadedProjects
		do: [ :loadedProject | packageNames addAll: loadedProject packageNames ].
	^ packageNames asArray
%

category: 'querying'
method: Rowan3ImageStub
resolveClassNamed: aName

	"If the given name is bound to a class in the environment of the current session, 
	answer that class. Otherwise, answer nil."

	| resolved |
	resolved := self objectNamed: aName.
	^ (resolved isBehavior and: [ resolved isMeta not ])
		ifTrue: [ resolved ]
		ifFalse: [ nil ]
%

category: 'accessing'
method: Rowan3ImageStub
symbolList
	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
%

! Class implementation for 'Rowan3LoadedClassExtensionStub'

!		Instance methods for 'Rowan3LoadedClassExtensionStub'

category: 'accessing'
method: Rowan3LoadedClassExtensionStub
handle
	^ self theClass
%

category: 'accessing'
method: Rowan3LoadedClassExtensionStub
theClass
	^theClass
%

category: 'accessing'
method: Rowan3LoadedClassExtensionStub
theClass: object
	theClass := object
%

! Class implementation for 'Rowan3LoadedClassStub'

!		Instance methods for 'Rowan3LoadedClassStub'

category: 'accessing'
method: Rowan3LoadedClassStub
handle
	^ self theClass
%

category: 'accessing'
method: Rowan3LoadedClassStub
theClass
	^theClass
%

category: 'accessing'
method: Rowan3LoadedClassStub
theClass: object
	theClass := object
%

! Class implementation for 'Rowan3LoadedPackageStub'

!		Instance methods for 'Rowan3LoadedPackageStub'

category: 'accessing'
method: Rowan3LoadedPackageStub
name
	^name
%

category: 'accessing'
method: Rowan3LoadedPackageStub
name: object
	name := object
%

! Class implementation for 'Rowan3LoadedProjectStub'

!		Instance methods for 'Rowan3LoadedProjectStub'

category: 'accessing'
method: Rowan3LoadedProjectStub
commitId
	^ 0
%

category: 'accessing'
method: Rowan3LoadedProjectStub
componentNames
	^ #()
%

category: 'testing'
method: Rowan3LoadedProjectStub
existsOnDisk
	^ true
%

category: 'accessing'
method: Rowan3LoadedProjectStub
loadedClasses
	| theLoadedClasses |
	self halt: 'not expecting this to be called'.
	theLoadedClasses := KeyValueDictionary new.
	(ClassOrganizer new categories at: self name ifAbsent: [ ^ theLoadedClasses ])
		do: [ :aBehavior | 
			| aClass |
			aClass := aBehavior theNonMetaClass.
			theLoadedClasses
				at: aClass name
				put: (Rowan3LoadedClassStub new theClass: aClass) ].
	^ theLoadedClasses
%

category: 'accessing'
method: Rowan3LoadedProjectStub
loadedCommitId
	^ 0
%

category: 'accessing'
method: Rowan3LoadedProjectStub
loadedPackageNamed: aString
	^ self
		loadedPackageNamed: aString
		ifAbsent: [ self error: 'No package named ' , aString printString , ' found' ]
%

category: 'accessing'
method: Rowan3LoadedProjectStub
loadSpecification
	^ nil
%

category: 'accessing'
method: Rowan3LoadedProjectStub
name
	^name
%

category: 'accessing'
method: Rowan3LoadedProjectStub
name: object
	name := object
%

category: 'accessing'
method: Rowan3LoadedProjectStub
projectUrl
	^ 'file://fake'
%

! Class implementation for 'Rowan3PlatformStub'

!		Class methods for 'Rowan3PlatformStub'

category: 'instance creation'
classmethod: Rowan3PlatformStub
new
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
%

!		Instance methods for 'Rowan3PlatformStub'

category: 'accessing'
method: Rowan3PlatformStub
image
	^ image ifNil: [ image := Rowan3ImageStub new ]
%

category: 'initialization'
method: Rowan3PlatformStub
initialize
	self image
%

category: 'accessing'
method: Rowan3PlatformStub
parseSelectorFrom: methodString
	^ self parseSelectorFrom: methodString passCompileError: false
%

category: 'accessing'
method: Rowan3PlatformStub
parseSelectorFrom: methodString passCompileError: passCompileError
	| meth |
	^ [ 
	meth := self
		_parseMethod: methodString
		category: #'xyzzy'
		using: self image symbolList
		environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: [ 
			"if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil
				ifFalse: [ ^ nil ].
			meth := meth at: 1 ].
	meth selector asString ]
		on: CompileError
		do: [ :ex | 
			passCompileError
				ifTrue: [ ex pass ]
				ifFalse: [ ex return: '_____could_not_parse_selector_from_method_source_____' ] ]
%

category: 'accessing'
method: Rowan3PlatformStub
serviceClassFor: className ifAbsent: absentBlock

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone: absentBlock
%

category: '_private'
method: Rowan3PlatformStub
_parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError.
   Only used to parse a method to determine the selector.  "

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #'UndefinedSymbols'.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^ [ 
	UndefinedObject
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
		intoMethodDict: GsMethodDictionary new
		intoCategories: GsMethodDictionary new
		environmentId: anEnvironmentId ]
		onSynchronous: (Array with: CompileError with: CompileWarning)
		do:
			(Array
				with: [ :ex | 
					| undefSymbol symbols |
					undefSymbol := true.
					symbols := Array new.
					ex errorDetails
						do: [ :errArray | 
							(errArray atOrNil: 1) == 1031
								ifTrue: [ symbols add: (errArray atOrNil: 5) asSymbol ]
								ifFalse: [ undefSymbol := false ] ].
					undefSymbol
						ifTrue: [ 
							"attempt auto-define of undefined symbols"
							symbols do: [ :sym | undefinedSymbols at: sym put: nil ].

							[ 
							^ UndefinedObject
								compileMethod: source
								dictionaries: aSymbolList , undefinedSymbolList
								category: cat
								intoMethodDict: GsMethodDictionary new
								intoCategories: GsMethodDictionary new
								environmentId: anEnvironmentId ]
								onSynchronous: (Array with: CompileError with: CompileWarning)
								do:
									(Array with: [ :exb | undefSymbol := false ] with: [ :exc | exc resume ]) ].
					undefSymbol
						ifFalse: [ ex outer ] ]
				with: [ :ex | ex resume ])
%

! Class implementation for 'Rowan3ProjectToolsStub'

!		Class methods for 'Rowan3ProjectToolsStub'

category: 'instance creation'
classmethod: Rowan3ProjectToolsStub
new
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
%

!		Instance methods for 'Rowan3ProjectToolsStub'

category: 'accessing'
method: Rowan3ProjectToolsStub
browser
	^ browserTool ifNil: [ browserTool := Rowan3BrowserToolsStub new ]
%

category: 'initialization'
method: Rowan3ProjectToolsStub
initialize
	self browser
%

! Class implementation for 'Rowan3Stub'

!		Class methods for 'Rowan3Stub'

category: 'instance creation'
classmethod: Rowan3Stub
new
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
%

!		Instance methods for 'Rowan3Stub'

category: 'accessing'
method: Rowan3Stub
commandResultClass

	^ self platform commandResultClass
%

category: 'accessing'
method: Rowan3Stub
globalNamed: aString
	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self image objectNamed: aString
%

category: 'accessing'
method: Rowan3Stub
image
	^ self platform image
%

category: 'initialization'
method: Rowan3Stub
initialize
	self platform.
	self projectTools
%

category: 'accessing'
method: Rowan3Stub
jadeServerClassNamed: className

	| jadeClasses |

	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
%

category: 'accessing'
method: Rowan3Stub
loggingServiceClass

	^ self platform loggingServiceClass
%

category: 'accessing'
method: Rowan3Stub
platform
	^ platform ifNil: [ platform := Rowan3PlatformStub new ]
%

category: 'accessing'
method: Rowan3Stub
projectTools
	^ projectTools ifNil: [ projectTools := Rowan3ProjectToolsStub new ]
%

category: 'accessing'
method: Rowan3Stub
unpackagedName
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
%

