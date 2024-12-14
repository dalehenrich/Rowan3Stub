tests
testListPackageVersions
	| packageBrowser stream |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listVersionsOfPackageNamed: 'ConfigurationOfGLASS' for: 'http://seaside.gemtalksystems.com/ss/MetacelloRepository' on: stream.
	self _assertListMatchesExpectedPackagesList: stream contents equals: self _expectedPackageVersionsList