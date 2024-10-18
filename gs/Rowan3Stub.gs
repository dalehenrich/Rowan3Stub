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
	instVarNames: #(image projectTools)
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
classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName
	"Returns a description of the receiver using object names taken from the given
 UserProfile."

	| result lfsp |
	result := String new.
	result addAll: superclassName.
	(lfsp := Character lf asString) addAll: '  '.
	result
		addAll: ' subclass: ''';
		addAll: 'ClassName';
		addLast: $'.
	result
		addAll: lfsp;
		addAll: 'instVarNames: #( )';
		add: $).	" classVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classVars: #()'.	" classInstVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classInstVars: #()'.

	result
		addAll: lfsp;
		addAll: 'poolDictionaries: #('.
	result add: '#()'.	" inDictionary: <name of containing dictionary> "
	result
		addAll: lfsp;
		addAll: 'inDictionary: ';
		addAll: 'UserGlobals'.
	^ result
%

! Class implementation for 'Rowan3ImageStub'

!		Instance methods for 'Rowan3ImageStub'

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
loadedProjects
	^ IdentitySet new
%

category: 'accessing'
method: Rowan3ImageStub
packageNames
	^ #()
%

category: 'accessing'
method: Rowan3ImageStub
symbolList
	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
%

! Class implementation for 'Rowan3ProjectToolsStub'

!		Instance methods for 'Rowan3ProjectToolsStub'

category: 'accessing'
method: Rowan3ProjectToolsStub
browser
	^ browserTool ifNil: [ browserTool := Rowan3BrowserToolsStub new ]
%

! Class implementation for 'Rowan3Stub'

!		Instance methods for 'Rowan3Stub'

category: 'accessing'
method: Rowan3Stub
commandResultClass

	^ GsSession currentSession objectNamed: #RowanCommandResult
%

category: 'accessing'
method: Rowan3Stub
image
	^ image ifNil: [ image := Rowan3ImageStub new ]
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
projectTools
	^ projectTools ifNil: [ projectTools := Rowan3ProjectToolsStub new ]
%

category: 'accessing'
method: Rowan3Stub
unpackagedName
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
%

