operations
loadPackageVersion: packageVersion from: repositoryDescription
	| repository |
	repository := self 
		repositoryFromDescription: repositoryDescription 
			ifAbsent: [ self error: 'The repository ', repositoryDescription printString, ' has not been created' ].
	Gofer new 
		version: packageVersion;
		repository: repository;
		load.
	self noCommit ifFalse: [ System commitTransaction ].