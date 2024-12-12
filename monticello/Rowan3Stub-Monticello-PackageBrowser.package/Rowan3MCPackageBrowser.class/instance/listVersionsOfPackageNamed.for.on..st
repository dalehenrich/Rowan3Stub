operations
listVersionsOfPackageNamed: packageName for: repositoryDescription on: stream
	| repository project repositorySpec versionNames metacelloMCProjectClass |
	metacelloMCProjectClass := (Rowan globalNamed: 'MetacelloMCProject')
		ifNil: [ ^ #() ].
	project := metacelloMCProjectClass new.
	repositorySpec := project repositorySpec.
	repositorySpec description: repositoryDescription.
	repository := repositorySpec createRepository.
	versionNames := {}.
	repository allVersionNames
		do: [ :versionName | 
			| reference aPackageName |
			reference := (Rowan globalNamed: 'GoferResolvedReference')
				name: versionName
				repository: repository.
			aPackageName := reference metacelloPackageNameWithBranch at: 2.
			aPackageName = packageName
				ifTrue: [ versionNames add: reference name ] ].
	versionNames sort
		do: [ :versionName | 
			stream
				nextPutAll: versionName;
				lf ]
