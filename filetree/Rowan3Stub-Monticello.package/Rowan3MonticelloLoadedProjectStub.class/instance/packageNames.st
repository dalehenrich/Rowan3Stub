accessing
packageNames
	self name = self class monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' self name ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort