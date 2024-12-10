accessing
packageNames
	| packageNames |
	packageNames := Set new.
	self loadedProjects
		do: [ :loadedProject | packageNames addAll: loadedProject packageNames ].
	^ packageNames asArray