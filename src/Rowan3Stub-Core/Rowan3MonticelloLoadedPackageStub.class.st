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
	"do we need to distinguish extensions for Monticello packages?"

	^ Dictionary new
]

{ #category : 'accessing' }
Rowan3MonticelloLoadedPackageStub >> projectName [
	^ Rowan3MonticelloLoadedProjectStub monticelloProjectName
]
