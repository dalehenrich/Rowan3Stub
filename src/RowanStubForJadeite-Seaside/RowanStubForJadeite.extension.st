Extension { #name : 'RowanStubForJadeite' }

{ #category : 'accessing' }
 RowanStubForJadeite>> projectNamed: projectName [
	^ self image loadedProjectNamed: projectName
]
