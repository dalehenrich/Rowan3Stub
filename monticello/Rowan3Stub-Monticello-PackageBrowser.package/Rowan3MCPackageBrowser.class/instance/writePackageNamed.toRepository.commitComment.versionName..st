operations
writePackageNamed: packageName toRepository: repository commitComment: commitComment versionName: versionName
	|  version wc  theVersionName |
	wc := (self packageNamed: packageName) 
		ifNil: [ ^ self error: 'No package named ', packageName printString, ' found.' ].
	theVersionName := versionName 
		ifNil: [ 
			"calculate a unique version name"
			wc uniqueVersionName]
		ifNotNil: [ versionName ].
	version := wc 
		newVersionWithName: theVersionName
		message: commitComment.
	repository storeVersion: version.