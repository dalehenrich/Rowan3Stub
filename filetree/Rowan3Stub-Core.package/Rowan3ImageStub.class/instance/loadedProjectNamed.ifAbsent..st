accessing
loadedProjectNamed: projectName ifAbsent: absentBlock
	^ self loadedProjects
		detect: [ :each | each name = projectName ]
		ifNone: absentBlock