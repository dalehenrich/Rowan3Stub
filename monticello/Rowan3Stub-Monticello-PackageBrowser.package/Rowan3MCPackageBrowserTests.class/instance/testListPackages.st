tests
testListPackages
	| packageBrowser stream |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listPackageNamesOn: stream.
	self _assertListMatchesExpectedPackagesList: stream contents equals: self _expectedPackageNamesList
