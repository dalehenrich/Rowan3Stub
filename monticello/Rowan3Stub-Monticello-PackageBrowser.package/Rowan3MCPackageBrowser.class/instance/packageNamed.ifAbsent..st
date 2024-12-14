accessing
packageNamed: packageName ifAbsent: absentBlock
	"answer the package (MCWorkingCopy instance) with the given name"

	^ self class _allWorkingCopies detect: [ :wc | wc packageName = packageName ] ifNone: absentBlock