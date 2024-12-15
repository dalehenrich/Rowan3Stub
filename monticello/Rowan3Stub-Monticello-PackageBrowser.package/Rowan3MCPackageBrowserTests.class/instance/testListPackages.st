tests
testListPackages
	| packageBrowser stream list |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listPackageNamesOn: stream.
	self _assertListMatchesExpectedPackagesList: stream contents equals: self _expectedPackageNamesList.
	
	list := packageBrowser packageNames.
	self assert: list sorted equals: (self _namesFromString: self _expectedPackageNamesList) sorted