accessing
loadedPackageNamed: aName ifAbsent: absentBlock
	"scan the symbol list a RwLoadedPackage instance of the given name"

	self loadedProjects
		do: [ :loadedProject | 
			(loadedProject loadedPackageNamed: aName ifAbsent: [  ])
				ifNotNil: [ :loadedPackage | ^ loadedPackage ] ].
	^ absentBlock value