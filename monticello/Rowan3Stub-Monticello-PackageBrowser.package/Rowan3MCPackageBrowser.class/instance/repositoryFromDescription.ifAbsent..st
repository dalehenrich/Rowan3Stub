accessing
repositoryFromDescription: repositoryDescription ifAbsent: absentBlock
	| project repositorySpec repository |
	 project := MetacelloMCProject new.
	repositorySpec := project repositorySpec.
	repositorySpec description: repositoryDescription.
	repository := repositorySpec createRepository.
	(MCRepositoryGroup default includes:  repository) ifFalse: absentBlock.
	^ repository