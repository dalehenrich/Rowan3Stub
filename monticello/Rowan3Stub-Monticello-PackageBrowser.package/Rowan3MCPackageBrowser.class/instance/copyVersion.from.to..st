operations
copyVersion: packageVersion from: fromDescription to: toDescription
	| version toRepo fromRepo |
	fromRepo := self _repositoryFromDescription: fromDescription.
	version := (GoferResolvedReference name: packageVersion repository: fromRepo) version.
	toRepo :=  self _repositoryFromDescription: toDescription.
	toRepo storeVersion: version.
	^ self noResult