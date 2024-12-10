accessing
loadedClasses
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