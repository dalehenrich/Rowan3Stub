Class {
	#name : 'RowanStubForJadeite',
	#superclass : 'Object',
	#instVars : [
		'platform',
		'projectTools'
	],
	#category : 'RowanStubForJadeite-Core'
}

{ #category : 'instance creation' }
RowanStubForJadeite class >> new [
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
]

{ #category : 'accessing' }
RowanStubForJadeite >> commandResultClass [

	^ self platform commandResultClass
]

{ #category : 'accessing' }
RowanStubForJadeite >> gemstoneTools [
	^ UserGlobals 
		at: #RowanStubForJadeite_gemstoneTools
		ifAbsentPut: [ Rowan3GemStoneRowanToolStub new ]
]

{ #category : 'accessing' }
RowanStubForJadeite >> globalNamed: aString [
	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self image objectNamed: aString
]

{ #category : 'accessing' }
RowanStubForJadeite >> image [
	^ self platform image
]

{ #category : 'initialization' }
RowanStubForJadeite >> initialize [
	self platform.
	self projectTools
]

{ #category : 'accessing' }
RowanStubForJadeite >> jadeServerClassNamed: className [

	| jadeClasses |

	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
]

{ #category : 'accessing' }
RowanStubForJadeite >> loggingServiceClass [

	^ self platform loggingServiceClass
]

{ #category : 'accessing' }
RowanStubForJadeite >> platform [
	^ platform ifNil: [ platform := Rowan3PlatformStub new ]
]

{ #category : 'accessing' }
RowanStubForJadeite >> projectTools [
	^ projectTools ifNil: [ projectTools := Rowan3ProjectToolsStub new ]
]

{ #category : 'accessing' }
RowanStubForJadeite >> unpackagedName [
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
]
