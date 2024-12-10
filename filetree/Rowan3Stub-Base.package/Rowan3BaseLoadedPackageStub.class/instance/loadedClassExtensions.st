accessing
loadedClassExtensions
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