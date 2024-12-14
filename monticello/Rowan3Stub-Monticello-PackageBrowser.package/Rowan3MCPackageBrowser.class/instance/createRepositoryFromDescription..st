operations
createRepositoryFromDescription: repositoryDescription
	|  repository |
	repository := self repositoryFromDescription: repositoryDescription.
	repository isValid
		ifFalse: [ repository directory assureExistence ].
	MCRepositoryGroup default addRepository: repository.
	self noCommit ifFalse: [ System commitTransaction ].
	^ repository