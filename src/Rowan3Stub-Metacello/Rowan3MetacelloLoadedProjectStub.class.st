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

	| projectRegistration |
	^((Rowan globalNamed: 'MetacelloProjectRegistration') registry projectSpecs
    collect: [ :projectSpec | 
      (Rowan globalNamed: 'MetacelloProjectRegistration')
        registrationForProjectSpec: projectSpec
        ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        ifPresent: [ :registration :ignored | (Rowan globalNamed: 'TDMetacelloRegistrationDefinition') registration: registration ] ])
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> loadedPackageNamed: aName ifAbsent: absentBlock [
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aName;
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
