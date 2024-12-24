operations
writePackageNamed: packageName to: repositoryDescription commitComment: commitComment versionName: versionName
	|   repository  |
	repository := MCRepositoryGroup default repositories 
		detect: [:each | 
			(each description ~= 'cache' and: [ (each description endsWith: 'package-cache') not ]) and: [ each description = repositoryDescription ] ]
		ifNone: [ 
			self error: 'The repository ', repositoryDescription printString, ' is not present in the image' ].
	self writePackageNamed: packageName toRepository: repository commitComment: commitComment versionName: versionName