tests
testCreatePackage
	| packageBrowser  packageName  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageName := 'ATEST-PACKAGE'.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: []) equals: nil.
	packageBrowser createPackageNamed: packageName.
	self assert: (packageBrowser packageNamed: packageName)  packageName equals: packageName.
	packageBrowser unloadPackageNamed: packageName