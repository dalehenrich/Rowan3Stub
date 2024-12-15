operations
createPackageNamed: packageName
	| package |
	package := MCWorkingCopy forPackage: (MCPackage named: packageName).
	self noCommit ifFalse: [ System commitTransaction ].
	^ package