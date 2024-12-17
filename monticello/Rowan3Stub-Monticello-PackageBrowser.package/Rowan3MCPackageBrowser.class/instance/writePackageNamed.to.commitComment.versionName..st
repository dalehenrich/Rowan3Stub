operations
writePackageNamed: packageName to: repositoryDescription commitComment: commitComment versionName: versionName
	|  version wc repository theVersionName |
	wc := (self packageNamed: packageName) 
		ifNil: [ ^ self error: 'No package named ', packageName printString, ' found.' ].
	repository := MCRepositoryGroup default repositories 
		detect: [:each | 
			(each description ~= 'cache' and: [ (each description endsWith: 'package-cache') not ]) and: [ each description = repositoryDescription ] ]
		ifNone: [ 
			self error: 'The repository ', repositoryDescription printString, ' is not present in the image' ].
	theVersionName := versionName 
		ifNil: [ 
			"calculate a unique version name"
			wc uniqueVersionName]
		ifNotNil: [ versionName ].
	version := wc 
		newVersionWithName: theVersionName
		message: commitComment.
	repository storeVersion: version.