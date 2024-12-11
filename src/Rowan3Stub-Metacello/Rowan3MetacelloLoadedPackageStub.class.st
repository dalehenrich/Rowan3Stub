Class {
	#name : 'Rowan3MetacelloLoadedPackageStub',
	#superclass : 'Rowan3LoadedPackageStub',
	#instVars : [
		'workingCopy',
		'loadedProject'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> gs_symbolDictionary [
	^ #'UserGlobals'
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> loadedClasses [
	| theLoadedClasses classes organizer |
	theLoadedClasses := KeyValueDictionary new.
	classes := IdentitySet new.
	organizer := ClassOrganizer new.
	((GsSession currentSession objectNamed: #'MCPackage') named: self name)
		packageInfo systemCategories
		do: [ :cat | 
			(organizer categories at: cat)
				do: [ :aClass | 
					theLoadedClasses
						at: aClass name
						put: (Rowan3LoadedClassStub new theClass: aClass) ] ].
	^ theLoadedClasses
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> loadedClassExtensions [
	| theExtendedClasses packageInfo extensionClasses packageInfoClass |
	theExtendedClasses := KeyValueDictionary new.
	extensionClasses := IdentitySet new.
	packageInfoClass := Rowan globalNamed: 'PackageInfo'.
	packageInfoClass ifNil: [ ^ theExtendedClasses ].
	packageInfo := packageInfoClass named: self name.
	packageInfo extensionClasses
		do: [ :aBehavior | extensionClasses add: aBehavior theNonMetaClass ].
	extensionClasses
		do: [ :aClass | 
			theExtendedClasses
				at: aClass name
				put: (Rowan3LoadedClassExtensionStub new theClass: aClass) ].
	^ theExtendedClasses
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> loadedProject [
	^loadedProject
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> loadedProject: object [
	loadedProject := object
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> projectName [
	^ self loadedProject name
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> workingCopy [
	^ workingCopy
]

{ #category : 'accessing' }
Rowan3MetacelloLoadedPackageStub >> workingCopy: aMCWorkingCopy [
	workingCopy := aMCWorkingCopy
]
