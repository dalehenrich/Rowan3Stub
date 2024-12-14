accessing
repositoryFromDescription: repositoryDescription
	| project repositorySpec |
	 project := MetacelloMCProject new.
	repositorySpec := project repositorySpec.
	repositorySpec description: repositoryDescription.
	^ repositorySpec createRepository.