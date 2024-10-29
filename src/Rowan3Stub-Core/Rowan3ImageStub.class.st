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
Rowan3ImageStub >> loadedPackageNamed: aName ifAbsent: absentBlock [
	"scan the symbol list a RwLoadedPackage instance of the given name"

	self loadedProjects
		do: [ :loadedProject | 
			(loadedProject loadedPackageNamed: aName ifAbsent: [  ])
				ifNotNil: [ :loadedPackage | ^ loadedPackage ] ].
	^ absentBlock value
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjectNamed: aString [

	^ self
		loadedProjectNamed: aString
		ifPresent: [:loadedProject | loadedProject ]
		ifAbsent: [ self error: 'No loaded project named ' , aString printString , ' found' ]
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjectNamed: projectName ifAbsent: absentBlock [
	^ self loadedProjects
		detect: [ :each | each name = projectName ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjectNamed: aString ifPresent: presentBlock ifAbsent: absentBlock [
	"Look up a loaded project in the loaded project registry"

	| loadedProject |
	loadedProject := self loadedProjectNamed: aString ifAbsent: absentBlock.
	^ presentBlock cull: loadedProject
]

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjects [
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:
					(IdentitySet
						with:
							(Rowan3MonticelloLoadedProjectStub new
								name: 'Monticello';
								yourself)) ]
]

{ #category : 'querying' }
Rowan3ImageStub >> objectNamed: aSymbol [
	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ self symbolList objectNamed: aSymbol
]

{ #category : 'accessing' }
Rowan3ImageStub >> packageNames [
	| packageNames |
	packageNames := Set new.
	self loadedProjects
		do: [ :loadedProject | packageNames addAll: loadedProject packageNames ].
	^ packageNames asArray
]

{ #category : 'querying' }
Rowan3ImageStub >> packageNamesForLoadedProjectNamed: projectName [
	projectName = 'Monticello'
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc ancestry ancestors first name ]) sort
]

{ #category : 'querying' }
Rowan3ImageStub >> resolveClassNamed: aName [

	"If the given name is bound to a class in the environment of the current session, 
	answer that class. Otherwise, answer nil."

	| resolved |
	resolved := self objectNamed: aName.
	^ (resolved isBehavior and: [ resolved isMeta not ])
		ifTrue: [ resolved ]
		ifFalse: [ nil ]
]

{ #category : 'accessing' }
Rowan3ImageStub >> symbolList [
	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
]
