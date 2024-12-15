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
		versionName: versionName