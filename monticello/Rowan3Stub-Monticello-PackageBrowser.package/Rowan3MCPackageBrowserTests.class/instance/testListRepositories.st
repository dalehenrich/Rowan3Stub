tests
testListRepositories
	| packageBrowser stream |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listRepositoriesOn: stream.
	self _assertListMatchesExpectedRepositoryList: stream contents equals: self _expectedHttpRepositoryList
