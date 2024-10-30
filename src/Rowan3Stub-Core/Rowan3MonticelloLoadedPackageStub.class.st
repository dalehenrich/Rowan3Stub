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
	| theLoadedClasses |
	theLoadedClasses := KeyValueDictionary new.
	(ClassOrganizer new categories at: self name ifAbsent: [ ^ theLoadedClasses ])
		do: [ :aClass | 
			theLoadedClasses
				at: aClass name
				put: (Rowan3LoadedClassStub new theClass: aClass) ].
	^ theLoadedClasses
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedPackageStub >> loadedClassExtensions [
	| theExtendedClasses packageInfo extensionClasses |
	theExtendedClasses := KeyValueDictionary new.
	extensionClasses := IdentitySet new.
	packageInfo := (Rowan globalNamed: 'PackageInfo') named: self name.
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
