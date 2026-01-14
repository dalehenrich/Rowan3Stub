Extension { #name : 'Rowan3ImageStub' }

{ #category : 'accessing' }
Rowan3ImageStub >> loadedProjects [
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			| loadedProjects |
			loadedProjects := IdentitySet new.
			Rowan3TodeLoadedProjectStub metacelloProjectRegistrations do: [:projectRegistration |
				loadedProjects add: (Rowan3TodeLoadedProjectStub new
								name: projectRegistration projectName;
								projectRegistration: projectRegistration; 
								yourself) ].
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:loadedProjects ]
]

{ #category : 'querying' }
Rowan3ImageStub >> packageNamesForLoadedProjectNamed: projectName [
	^ (self loadedProjectNamed: projectName) packageNames
]

{ #category : 'accessing' }
Rowan3ImageStub >> projectNamed: projectName [

	^ self loadedProjectNamed: projectName
]
