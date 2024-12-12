accessing
modifiedPackageNames
	"return a list of modified Monticello package names in the image"

	^ self modifiedPackages collect: [ :wc | wc packageName ]
