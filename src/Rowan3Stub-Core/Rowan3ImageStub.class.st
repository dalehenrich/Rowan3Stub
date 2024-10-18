Class {
	#name : 'Rowan3ImageStub',
	#superclass : 'Object',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3ImageStub >> globalNamed: aString [
	"Answer a global object with the given name.  If no object with the given name is found, returns nil."

	^ self objectNamed: aString
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedClassExtensionsForClass: class [
	"lookup the loadedClassExtensions for the given class"

	^ IdentitySet new
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedClassForClass: aClass ifAbsent: absentBlock [
	^ absentBlock value
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjects [
	^ IdentitySet new
]

{ #category : 'accessing' }
Rowan3ImageStub >> objectNamed: aSymbol [
	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ self symbolList objectNamed: aSymbol
]

{ #category : 'accessing' }
Rowan3ImageStub >> packageNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3ImageStub >> symbolList [
	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
]
