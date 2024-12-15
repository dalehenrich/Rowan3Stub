tests
testPackageNamed
	| packageBrowser packageName notFound |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	notFound := false.
	packageBrowser packageNamed: 'BogusPackageName' ifAbsent: [ notFound := true ].
	self assert: notFound description: 'Package ''BogusPackageName'' unexpectedly found'.
	packageName := packageBrowser packageNames asArray first.
	packageBrowser packageNamed: packageName ifAbsent: [ self assert: false description: 'Package ', packageName printString, ' was unexpectedly not found' ].
	packageBrowser unloadPackageNamed: packageName.