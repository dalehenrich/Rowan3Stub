Class {
	#name : 'Rowan3GemStoneTopazToolStub',
	#superclass : 'Object',
	#category : 'RowanStubForJadeite-Core'
}

{ #category : 'private' }
Rowan3GemStoneTopazToolStub >> _currentTopazPackageKey [
	^ #'RowanTopazCurrentPackageName'
]

{ #category : 'private' }
Rowan3GemStoneTopazToolStub >> _currentTopazProjectKey [
	^#'RowanTopazCurrentProjectName'
]

{ #category : 'accessing' }
Rowan3GemStoneTopazToolStub >> currentTopazPackageName [
	"Return the current topaz package name or nil"

	^ SessionTemps current at: self _currentTopazPackageKey otherwise: nil
]

{ #category : 'accessing' }
Rowan3GemStoneTopazToolStub >> currentTopazPackageName: packageNameOrNil [
	"Set the current topaz package name. New methods and classes created in the topaz session 
		will be added to the named package. If nil, new methods and classes will be unmanaged."

	SessionTemps current at: self _currentTopazPackageKey put: packageNameOrNil
]

{ #category : 'accessing' }
Rowan3GemStoneTopazToolStub >> currentTopazProjectName [
	"answer the name of the current project"

	^ SessionTemps current at: self _currentTopazProjectKey otherwise: nil
]

{ #category : 'accessing' }
Rowan3GemStoneTopazToolStub >> currentTopazProjectName: projectNameOrNil [
	"set the name of the current project"

	SessionTemps current at: self _currentTopazProjectKey put: projectNameOrNil
]
