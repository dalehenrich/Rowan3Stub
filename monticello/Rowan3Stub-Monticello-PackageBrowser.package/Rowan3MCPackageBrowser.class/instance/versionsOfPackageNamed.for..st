operations
versionsOfPackageNamed: packageName for: repositoryDescription
	| repository versionNames metacelloMCProjectClass |
	metacelloMCProjectClass := (Rowan globalNamed: 'MetacelloMCProject')
		ifNil: [ ^ #() ].
	repository := self 
		repositoryFromDescription: repositoryDescription 
			ifAbsent: [ self error: 'The repository ', repositoryDescription printString, ' has not been created' ].
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
	^ versionNames sort