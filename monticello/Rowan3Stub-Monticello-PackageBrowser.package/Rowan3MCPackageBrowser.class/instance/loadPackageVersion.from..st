operations
loadPackageVersion: packageVersion from: repositoryDescription
	repository := self repositoryFromDescription: repositoryDescription.
	Gofer new 
		version: packageVersion;
		repository: repository;
		load.
	self noCommit ifFalse: [ System commitTransaction ].