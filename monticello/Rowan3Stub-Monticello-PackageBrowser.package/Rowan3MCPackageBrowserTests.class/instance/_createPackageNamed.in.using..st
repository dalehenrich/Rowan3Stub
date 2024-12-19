private
_createPackageNamed: packageName in: repositoryDescription using: packageBrowser
	packageBrowser createPackageNamed: packageName.
	packageBrowser
		writePackageNamed: packageName 
		to: repositoryDescription 
		commitComment: 'testing'