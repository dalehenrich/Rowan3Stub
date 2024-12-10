accessing
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3MonticelloLoadedPackageStub new
		name: aName;
		yourself