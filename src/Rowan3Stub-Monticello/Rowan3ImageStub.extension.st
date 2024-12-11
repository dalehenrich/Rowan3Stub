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
							(Rowan3MonticelloLoadedProjectStub new
								name: Rowan3MonticelloLoadedProjectStub monticelloProjectName;
								yourself)) ]
]

{ #category : 'querying' }
Rowan3ImageStub >> packageNamesForLoadedProjectNamed: projectName [
	projectName = Rowan3MonticelloLoadedProjectStub monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ (Rowan globalNamed: 'MCWorkingCopy')
		ifNil: [ #() ]
		ifNotNil: [ :workingCopyClass | (workingCopyClass allManagers collect: [ :wc | wc packageName ]) sort ]
]
