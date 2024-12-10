querying
objectNamed: aSymbol
	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ self symbolList objectNamed: aSymbol