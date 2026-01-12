Extension { #name : 'Rowan3ImageStub' }

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjects [
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:
					(IdentitySet
						with:
							(Rowan3SeasideLoadedProjectStub new
								name: Rowan3SeasideLoadedProjectStub monticelloProjectName;
								yourself)) ]
]

{ #category : 'querying' }
Rowan3ImageStub >> packageNamesForLoadedProjectNamed: projectName [
	projectName = Rowan3SeasideLoadedProjectStub monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
]

{ #category : 'accessing' }
Rowan3ImageStub >> projectNamed: projectName [

	^ self loadedProjectNamed: projectName
]
