Extension { #name : 'RowanStubForJadeite' }

{ #category : 'accessing' }
RowanStubForJadeite >> extentType [
    "Create a new initialized instance of the receiver."
 
    ^ 'seaside'
]
 
{ #category : 'accessing' }
 RowanStubForJadeite>> projectNamed: projectName [
	^ self image loadedProjectNamed: projectName
]

{ #category : 'accessing' }

 RowanStubForJadeite>> projectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock [
 
    ^self image loadedProjectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock.
]

