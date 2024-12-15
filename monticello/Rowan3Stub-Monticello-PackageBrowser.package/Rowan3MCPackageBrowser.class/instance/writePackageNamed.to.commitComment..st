operations
writePackageNamed: packageName to: repositoryDescription commitComment: commitComment
	"calculate a unique versionName"

	self writePackageNamed: packageName to: repositoryDescription commitComment: commitComment versionName: nil