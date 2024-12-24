operations
writePackageNamed: packageName toRepository: repository  commitComment: commitComment
	"calculate a unique versionName"

	self writePackageNamed: packageName toRepository: repository commitComment: commitComment versionName: nil