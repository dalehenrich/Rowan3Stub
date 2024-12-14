accessing
packageNamed: packageName
	"answer the package (MCWorkingCopy instance) with the given name"

	^ self packageNamed: packageName ifAbsent: [ self error: 'No package name ', packageName printString, ' found.']