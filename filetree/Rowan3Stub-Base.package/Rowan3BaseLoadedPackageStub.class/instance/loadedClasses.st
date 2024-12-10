accessing
loadedClasses
	| theLoadedClasses |
	theLoadedClasses := KeyValueDictionary new.
	(ClassOrganizer new categories at: self name ifAbsent: [ ^ theLoadedClasses ])
		do: [ :aClass | 
			theLoadedClasses
				at: aClass name
				put: (Rowan3LoadedClassStub new theClass: aClass) ].
	^ theLoadedClasses