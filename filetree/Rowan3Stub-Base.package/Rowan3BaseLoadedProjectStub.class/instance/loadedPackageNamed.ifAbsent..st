accessing
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3BaseLoadedPackageStub new
		name: aName;
		yourself