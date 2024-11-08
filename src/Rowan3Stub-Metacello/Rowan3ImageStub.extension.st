Extension { #name : 'Rowan3ImageStub' }

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjects [
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			| loadedProjects |
			loadedProjects := IdentitySet new.
			Rowan3MetacelloLoadedProjectStub metacelloProjectRegistrations do: [:projectRegistration |
				loadedProjects add: (Rowan3MetacelloLoadedProjectStub new
								name: projectRegistration projectName;
								projectRegistration: projectRegistration; 
								yourself) ].
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:loadedProjects ]
]

{ #category : 'querying' }
Rowan3ImageStub >> packageNamesForLoadedProjectNamed: projectName [
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
]
