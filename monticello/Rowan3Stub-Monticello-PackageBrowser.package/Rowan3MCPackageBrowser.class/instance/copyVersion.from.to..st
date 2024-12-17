operations
copyVersion: packageVersion from: fromDescription to: toDescription
	| version toRepo fromRepo |
	fromRepo := self repositoryFromDescription: fromDescription.
	version := (GoferResolvedReference name: packageVersion repository: fromRepo) version.
	toRepo :=  self repositoryFromDescription: toDescription.
	toRepo storeVersion: version