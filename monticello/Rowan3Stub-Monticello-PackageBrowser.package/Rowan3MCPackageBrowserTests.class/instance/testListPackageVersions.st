tests
testListPackageVersions
	| packageBrowser stream list |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listVersionsOfPackageNamed: 'ConfigurationOfGLASS' for: 'http://seaside.gemtalksystems.com/ss/MetacelloRepository' on: stream.
	self _assertListMatchesExpectedPackagesList: stream contents equals: self _expectedPackageVersionsList.
	
	list := packageBrowser versionsOfPackageNamed: 'ConfigurationOfGLASS' for: 'http://seaside.gemtalksystems.com/ss/MetacelloRepository'.
	self assert: list sorted  equals: (self _namesFromString: self _expectedPackageVersionsList) sorted.