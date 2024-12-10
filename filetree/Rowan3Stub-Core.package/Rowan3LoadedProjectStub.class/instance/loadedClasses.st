accessing
loadedClasses
	| theLoadedClasses |
	self halt: 'not expecting this to be called'.
	theLoadedClasses := KeyValueDictionary new.
	(ClassOrganizer new categories at: self name ifAbsent: [ ^ theLoadedClasses ])
		do: [ :aBehavior | 
			| aClass |
			aClass := aBehavior theNonMetaClass.
			theLoadedClasses
				at: aClass name
				put: (Rowan3LoadedClassStub new theClass: aClass) ].
	^ theLoadedClasses