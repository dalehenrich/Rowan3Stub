Extension { #name : 'RowanStubForJadeite' }

{ #category : 'accessing' }
 RowanStubForJadeite>> projectNamed: projectName [
	^ self image loadedProjectNamed: projectName
]

{ #category : 'accessing' }

 RowanStubForJadeite>> projectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock [
 
    ^self image loadedProjectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock.
]

