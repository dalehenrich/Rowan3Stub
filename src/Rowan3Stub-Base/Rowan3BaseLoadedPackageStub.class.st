Class {
	#name : 'Rowan3BaseLoadedPackageStub',
	#superclass : 'Rowan3LoadedPackageStub',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3BaseLoadedPackageStub >> gs_symbolDictionary [
	^ #'UserGlobals'
]

{ #category : 'accessing' }
Rowan3BaseLoadedPackageStub >> loadedClasses [
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
Rowan3BaseLoadedPackageStub >> loadedClassExtensions [
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
Rowan3BaseLoadedPackageStub >> projectName [
	^ Rowan3BaseLoadedProjectStub baseProjectName
]
