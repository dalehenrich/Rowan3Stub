Class {
	#name : 'Rowan3Stub',
	#superclass : 'Object',
	#instVars : [
		'platform',
		'projectTools'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'instance creation' }
Rowan3Stub class >> new [
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
]

{ #category : 'accessing' }
Rowan3Stub >> commandResultClass [

	^ self platform commandResultClass
]

{ #category : 'accessing' }
Rowan3Stub >> globalNamed: aString [
	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self image objectNamed: aString
]

{ #category : 'accessing' }
Rowan3Stub >> image [
	^ self platform image
]

{ #category : 'initialization' }
Rowan3Stub >> initialize [
	self platform.
	self projectTools
]

{ #category : 'accessing' }
Rowan3Stub >> jadeServerClassNamed: className [

	| jadeClasses |

	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
]

{ #category : 'accessing' }
Rowan3Stub >> loggingServiceClass [

	^ self platform loggingServiceClass
]

{ #category : 'accessing' }
Rowan3Stub >> platform [
	^ platform ifNil: [ platform := Rowan3PlatformStub new ]
]

{ #category : 'accessing' }
Rowan3Stub >> projectTools [
	^ projectTools ifNil: [ projectTools := Rowan3ProjectToolsStub new ]
]

{ #category : 'accessing' }
Rowan3Stub >> stubbed [
	^ true
]

{ #category : 'accessing' }
Rowan3Stub >> unpackagedName [
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
]
