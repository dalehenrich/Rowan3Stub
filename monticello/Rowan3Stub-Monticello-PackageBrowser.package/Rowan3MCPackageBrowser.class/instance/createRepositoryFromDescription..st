operations
createRepositoryFromDescription: repositoryDescription
	|  repository |
	repository :=  (self
		repositoryFromDescription: repositoryDescription 
		ifAbsent: [  ]) ifNotNil: [:newRepository |  
			MCRepositoryGroup default addRepository: newRepository.
			newRepository ].
	repository isValid
		ifFalse: [ repository directory assureExistence ].
	(repository isKindOf: MCFileTreeRepository) 
		ifTrue: [ 
			(repository respondsTo: #setNoMetadata)
				ifTrue: [ repository setNoMetadata ] ].
	self noCommit ifFalse: [ System commitTransaction ].
	^ repository