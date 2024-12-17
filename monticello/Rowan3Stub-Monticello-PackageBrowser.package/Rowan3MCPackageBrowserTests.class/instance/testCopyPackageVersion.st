tests
testCopyPackageVersion
"copyVersion: packageVersion from: fromDescription to: toDescription
"
	| packageBrowser  repositoryDescription1 repositoryDescription2 repository1 repository2 packageName versionName versions1 versions2  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	repositoryDescription1 := 'dictionary://Rowan3MC_test_repository_1'.
	repositoryDescription2 := 'dictionary://Rowan3MC_test_repository_2'.
	repository1 := packageBrowser createRepositoryFromDescription: repositoryDescription1.
	repository2 := packageBrowser createRepositoryFromDescription: repositoryDescription2.
[	packageName := 'ATest-Core'.
	packageBrowser createPackageNamed: packageName.
	versionName := packageName, '-dkh.5'.
	packageBrowser
		writePackageNamed: packageName 
		to: repositoryDescription1 
		commitComment: 'testing' 
		versionName: versionName.
	versions1 := packageBrowser versionsOfPackageNamed: packageName for: repositoryDescription1.
	versions2 := packageBrowser versionsOfPackageNamed: packageName for: repositoryDescription2.
	self assert: versions1 equals: { 'ATest-Core-dkh.5'}.
	self assert: versions2 equals: { }.
"do copy"
	packageBrowser copyVersion: versionName from: repositoryDescription1 to: repositoryDescription2.
	versions1 := packageBrowser versionsOfPackageNamed: packageName for: repositoryDescription1.
	versions2 := packageBrowser versionsOfPackageNamed: packageName for: repositoryDescription2.
	self assert: versions1 equals: versions2.	
	packageBrowser unloadPackageNamed: packageName.
	packageBrowser removeRepository: repository1.
	packageBrowser removeRepository: repository2.
	Smalltalk removeKey: #Rowan3MC_test_repository_1 ifAbsent: [].
	Smalltalk removeKey: #Rowan3MC_test_repository_2 ifAbsent: [].
] ensure: [	packageBrowser removeRepository: repository1.
		packageBrowser removeRepository: repository2.
	Smalltalk removeKey: #Rowan3MC_test_repository_1 ifAbsent: [].
	Smalltalk removeKey: #Rowan3MC_test_repository_2 ifAbsent: [].
		]