tests
testCreateRepository
	| packageBrowser  repoGlobalName  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	repoGlobalName := #'Rowan3MC_test_repository'.
	packageBrowser createRepositoryFromDescription: 'dictionary://', repoGlobalName asString.
	self assert: (Rowan globalNamed: repoGlobalName) class name equals: #'MCDictionaryRepository'