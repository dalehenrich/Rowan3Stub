Class {
	#name : 'Rowan3MonticelloLoadedPackageStub',
	#superclass : 'Rowan3LoadedPackageStub',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3MonticelloLoadedPackageStub >> gs_symbolDictionary [
	^ #'UserGlobals'
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedPackageStub >> loadedClasses [
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
Rowan3MonticelloLoadedPackageStub >> loadedClassExtensions [
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
Rowan3MonticelloLoadedPackageStub >> projectName [
	^ Rowan3MonticelloLoadedProjectStub monticelloProjectName
]
