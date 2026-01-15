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

	(Rowan globalNamed: 'MetacelloProjectRegistration')
		ifNil: [ ^ #() ]
		ifNotNil: [:theMetacelloProjectRegistrationClass |
				"if logging in as SystemUser, then these classes aren't present"
				^(theMetacelloProjectRegistrationClass registry projectSpecs
    			collect: [ :projectSpec | 
      			(Rowan globalNamed: 'MetacelloProjectRegistration')
        			registrationForProjectSpec: projectSpec
        			ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        			ifPresent: [ :registration :ignored |  registration ] ]) ]
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> loadedPackageNamed: aPackageName ifAbsent: absentBlock [
	| pr mcVersion wc |
	pr := self projectRegistration.
	mcVersion := pr configurationProjectSpec project map at: pr configurationProjectSpec versionString.
	wc := mcVersion packages detect: [:each | each name = aPackageName ]
		ifNone: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aPackageName;
		loadedProject: self;
		workingCopy: wc;
		yourself
"

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
"
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
	| pr mcVersion |
	pr := self projectRegistration.
	mcVersion := pr configurationProjectSpec project map at: pr configurationProjectSpec versionString.
	^ mcVersion packages collect: [:each | each name ]
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> projectRegistration [
	^ projectRegistration
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedProjectStub >> projectRegistration: aTDMetacelloRegistrationDefinition [
	projectRegistration := aTDMetacelloRegistrationDefinition
]
