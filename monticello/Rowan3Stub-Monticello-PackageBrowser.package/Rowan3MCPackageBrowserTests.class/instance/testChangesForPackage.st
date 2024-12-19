tests
testChangesForPackage
	| packageBrowser  packageName  className repositoryDescription repoGlobalName changes repository |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageName := 'ATEST-PACKAGE'.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: []) equals: nil.
	repoGlobalName := #'Rowan3MC_test_repository'.
	repositoryDescription :=  'dictionary://', repoGlobalName asString .
	repository := packageBrowser  createRepositoryFromDescription: repositoryDescription.
	self 
		_createPackageNamed: packageName 
		in: repositoryDescription
		using: packageBrowser.
	self assert: (packageBrowser packageNamed: packageName)  packageName equals: packageName.
	className :=  'YYY'.
	Object subclass: className
		instVarNames: #( )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: ''
		category: packageName
		options: #().
	changes := packageBrowser
		changesForPackageNamed: packageName
		against: repository.
	packageBrowser unloadPackageNamed: packageName.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: [nil ]) equals: nil.
	packageBrowser removeRepository: repository.
	Smalltalk removeKey: #Rowan3MC_test_repository ifAbsent: []