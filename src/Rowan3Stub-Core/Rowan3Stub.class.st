Class {
	#name : 'Rowan3Stub',
	#superclass : 'Object',
	#instVars : [
		'image'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3Stub >> commandResultClass [

	^ GsSession currentSession objectNamed: #RowanCommandResult
]

{ #category : 'accessing' }
Rowan3Stub >> image [
	^ image ifNil: [ image := Rowan3ImageStub new ]
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
Rowan3Stub >> unpackagedName [
	"Answer the name used for projects and packages that are not in a package ... unpackaged projects and packages are where pacakge things go by default."

	^ '(NONE)'
]
