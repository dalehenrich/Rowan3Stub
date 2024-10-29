Class {
	#name : 'Rowan3MonticelloLoadedProjectStub',
	#superclass : 'Rowan3LoadedProjectStub',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3MonticelloLoadedProjectStub >> loadedPackageNamed: aName ifAbsent: absentBlock [
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3MonticelloLoadedPackageStub new
		name: aName;
		yourself
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedProjectStub >> packageConvention [
	^ 'Monticello'
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedProjectStub >> packageGroupNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedProjectStub >> packageNames [
	self name = 'Monticello'
		ifFalse: [ self error: 'unexpected projectName: ' self name ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc ancestry ancestors first name ]) sort
]
