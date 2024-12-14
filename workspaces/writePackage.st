"w1.st"
	| repositoryDescription versionName version commitComment wc repository packageName |
	packageName := 'Rowan3Stub-Monticello-PackageBrowser'.
	wc := (MCWorkingCopy forPackage: (MCPackage named: packageName)) 
		ifNil: [ ^ 'No package named ', packageName printString, ' found.' ].		
	repositoryDescription := 'filetree:///bosch1/users/dhenrich/_stones/37x/j_37x_externals_st//Rowan3Stub/monticello'.
	repository := MCRepositoryGroup default repositories 
		detect: [:each | 
			(each description ~= 'cache' and: [ (each description endsWith: 'package-cache') not ]) and: [ each description = repositoryDescription ] ]
		ifNone: [ 
			self error: 'The repository ', repositoryDescription printString, ' is not present in the image' ].
	commitComment := 'remove a method and category'.
	versionName :=  'Rowan3Stub-Monticello-PackageBrowser-dkh.1'.
	version := wc 
		newVersionWithName: versionName
		message: commitComment.
	repository storeVersion: version.
