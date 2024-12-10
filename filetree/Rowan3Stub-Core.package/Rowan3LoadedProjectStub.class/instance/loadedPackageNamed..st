accessing
loadedPackageNamed: aString
	^ self
		loadedPackageNamed: aString
		ifAbsent: [ self error: 'No package named ' , aString printString , ' found' ]