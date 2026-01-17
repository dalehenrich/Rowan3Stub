Class {
	#name : 'Rowan3SeasideLoadedProjectStub',
	#superclass : 'Rowan3LoadedProjectStub',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3SeasideLoadedProjectStub class >> monticelloProjectName [
	^ 'LOOSE PACKAGES'
]

{ #category : 'accessing' }
Rowan3SeasideLoadedProjectStub >> loadedPackageNamed: aName ifAbsent: absentBlock [
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3SeasideLoadedPackageStub new
		name: aName;
		yourself
]

{ #category : 'accessing' }
Rowan3SeasideLoadedProjectStub >> packageConvention [
	^ 'Monticello'
]

{ #category : 'accessing' }
Rowan3SeasideLoadedProjectStub >> packageGroupNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3SeasideLoadedProjectStub >> packageNames [
	self name = self class monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' self name ].
	^ (Rowan globalNamed: 'MCWorkingCopy')
			ifNil: [ #() ]
			ifNotNil: [:wc |
				(wc allManagers collect: [ :wc | wc packageName ]) sort ]

]
