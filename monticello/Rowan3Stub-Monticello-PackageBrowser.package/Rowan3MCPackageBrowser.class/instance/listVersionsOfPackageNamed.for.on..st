operations
listVersionsOfPackageNamed: packageName for: repositoryDescription on: stream
	| versionNames |
	versionNames := self versionsOfPackageNamed: packageName for: repositoryDescription.
	versionNames sort
		do: [ :versionName | 
			stream
				nextPutAll: versionName;
				lf ]