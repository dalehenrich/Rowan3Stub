accessing
packageNames
	"return a list of Monticello package names in the image"

	^ self class _allWorkingCopies collect: [ :wc | wc packageName ]
