tests
testCreateRepository
	| packageBrowser  repoGlobalName repository  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	repoGlobalName := #'Rowan3MC_test_repository'.
	repository := packageBrowser createRepositoryFromDescription: 'dictionary://', repoGlobalName asString.
	self assert: (Rowan globalNamed: repoGlobalName) class name equals: #'MCDictionaryRepository'.
	packageBrowser removeRepository: repository.
	UserGlobals removeKey: repoGlobalName