Class {
	#name : 'Rowan3ImageStub',
	#superclass : 'Object',
	#category : 'Rowan3Stub-Core'
}

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
Rowan3ImageStub >> packageNames [
	^ #()
]
