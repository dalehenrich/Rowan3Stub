Class {
	#name : 'Rowan3MetacelloLoadedProjectStub',
	#superclass : 'Rowan3LoadedProjectStub',
	#instVars : [
		'projectRegistration'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub class >> metacelloProjectRegistrations [

	^((Rowan globalNamed: 'MetacelloProjectRegistration') registry projectSpecs
    collect: [ :projectSpec | 
      (Rowan globalNamed: 'MetacelloProjectRegistration')
        registrationForProjectSpec: projectSpec
        ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        ifPresent: [ :registration :ignored | (Rowan globalNamed: 'TDMetacelloRegistrationDefinition') registration: registration ] ])
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> loadedPackageNamed: aPackageName ifAbsent: absentBlock [
	| wc |
	(self packageNames includes: aPackageName)
		ifFalse: absentBlock.
	wc := self projectRegistration workingCopies
		detect: [ :each | each packageName = aPackageName ]
		ifNone: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aPackageName;
		loadedProject: self;
		workingCopy: wc;
		yourself
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> packageConvention [
	^ 'Monticello'
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> packageGroupNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> packageNames [
	^ self projectRegistration workingCopies collect: [:each | each packageName ]
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> projectRegistration [
	^ projectRegistration
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> projectRegistration: aTDMetacelloRegistrationDefinition [
	projectRegistration := aTDMetacelloRegistrationDefinition
]
