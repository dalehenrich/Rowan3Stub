tests
testWritePackageVersion
	| packageBrowser  repositoryDescription repository packageName versionName |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	repositoryDescription := 'dictionary://Rowan3MC_test_repository'.
	repository := packageBrowser createRepositoryFromDescription: repositoryDescription.
	packageName := 'ATest-Core'.
	packageBrowser createPackageNamed: packageName.
	versionName := packageName, '-dkh.5'.
	packageBrowser
		writePackageNamed: packageName 
		to: repositoryDescription 
		commitComment: 'testing' 
		versionName: versionName.
	packageBrowser
		writePackageNamed: packageName 
		to: repositoryDescription 
		commitComment: 'testing part 2'.
	(Smalltalk at: #Rowan3MC_test_repository).
	packageBrowser unloadPackageNamed: packageName.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: [nil ]) equals: nil.
	packageBrowser removeRepository: repository.
	Smalltalk removeKey: #Rowan3MC_test_repository ifAbsent: []