Class {
	#name : 'Rowan3LoadedProjectStub',
	#superclass : 'Object',
	#instVars : [
		'name'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> commitId [
	^ 0
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> componentNames [
	^ #()
]

{ #category : 'testing' }
Rowan3LoadedProjectStub >> existsOnDisk [
	^ true
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> loadedClasses [
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
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> loadedCommitId [
	^ 0
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> loadedPackageNamed: aString [
	^ self
		loadedPackageNamed: aString
		ifAbsent: [ self error: 'No package named ' , aString printString , ' found' ]
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> loadSpecification [
	^ nil
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> name [
	^name
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> name: object [
	name := object
]

{ #category : 'accessing' }
Rowan3LoadedProjectStub >> projectUrl [
	^ 'file://fake'
]
