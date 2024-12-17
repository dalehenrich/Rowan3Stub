tests
testCreateFiletreeRepository
	| packageBrowser  repository filetreeDir  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	filetreeDir := FileReference newTempFilePrefix: 'r3mc' suffix: '.filetree'.
	filetreeDir ensureCreateDirectory.
	repository := packageBrowser createRepositoryFromDescription: 'filetree://',  filetreeDir pathString.
	repository setNoMetadata.
	repository halt.
	packageBrowser removeRepository: repository.