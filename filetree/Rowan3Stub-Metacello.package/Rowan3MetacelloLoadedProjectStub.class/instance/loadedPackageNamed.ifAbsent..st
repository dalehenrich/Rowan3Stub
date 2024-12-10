accessing
loadedPackageNamed: aPackageName ifAbsent: absentBlock
	| wc |
	(self packageNames includes: aPackageName)
		ifFalse: absentBlock.
	wc := self projectRegistration workingCopies
		detect: [ :each | each packageName = aPackageName ]
		ifNone: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aPackageName;
		loadedProject: self;
		workingCopy: wc;
		yourself