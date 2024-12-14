operations
unloadPackageNamed: packageName
	Gofer new 
		package: packageName;
		unload.
	self noCommit ifFalse: [ System commitTransaction ].