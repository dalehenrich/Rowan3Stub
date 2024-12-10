*rowan3stub-monticello
loadedProjects
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
