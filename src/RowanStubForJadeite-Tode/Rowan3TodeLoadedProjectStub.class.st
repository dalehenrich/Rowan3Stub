Class {
	#name : 'Rowan3TodeLoadedProjectStub',
	#superclass : 'Rowan3LoadedProjectStub',
	#instVars : [
		'projectRegistration'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub class >> metacelloProjectRegistrations [

	(Rowan globalNamed: 'MetacelloProjectRegistration')
		ifNil: [ ^ #() ]
		ifNotNil: [:theMetacelloProjectRegistrationClass |
				"if logging in as SystemUser, then these classes aren't present"
				^(theMetacelloProjectRegistrationClass registry projectSpecs
    			collect: [ :projectSpec | 
      			(Rowan globalNamed: 'MetacelloProjectRegistration')
        			registrationForProjectSpec: projectSpec
        			ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        			ifPresent: [ :registration :ignored | (Rowan globalNamed: 'TDMetacelloRegistrationDefinition') registration: registration ] ]) ]
]

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub >> loadedPackageNamed: aPackageName ifAbsent: absentBlock [
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
Rowan3TodeLoadedProjectStub >> packageConvention [
	^ 'Monticello'
]

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub >> packageGroupNames [
	^ #()
]

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub >> packageNames [
	^ self projectRegistration workingCopies collect: [:each | each packageName ]
]

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub >> projectRegistration [
	^ projectRegistration
]

{ #category : 'accessing' }
Rowan3TodeLoadedProjectStub >> projectRegistration: aTDMetacelloRegistrationDefinition [
	projectRegistration := aTDMetacelloRegistrationDefinition
]
