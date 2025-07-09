Class {
	#name : 'Rowan3BaseLoadedProjectStub',
	#superclass : 'Rowan3LoadedProjectStub',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3BaseLoadedProjectStub class >> baseProjectName [
	^ 'Image'
]

{ #category : 'accessing' }
Rowan3BaseLoadedProjectStub >> loadedPackageNamed: aName ifAbsent: absentBlock [
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3BaseLoadedPackageStub new
		name: aName;
		yourself
]

{ #category : 'accessing' }
Rowan3BaseLoadedProjectStub >> packageConvention [
	^ 'Rowan'
]

{ #category : 'accessing' }
Rowan3BaseLoadedProjectStub >> packageGroupNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3BaseLoadedProjectStub >> packageNames [
	^#()
]
