! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'Rowan3Stub'
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

! Class implementation for 'Rowan3Stub'

!		Instance methods for 'Rowan3Stub'

category: 'accessing'
method: Rowan3Stub
jadeServerClassNamed: className

	| jadeClasses |

System waitForDebug.
	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
%
