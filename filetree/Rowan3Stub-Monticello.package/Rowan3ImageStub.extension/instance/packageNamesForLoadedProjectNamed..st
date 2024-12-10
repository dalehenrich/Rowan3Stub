*rowan3stub-monticello
packageNamesForLoadedProjectNamed: projectName
	projectName = Rowan3MonticelloLoadedProjectStub monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
