accessing
packageNames
	"return a list of Monticello package names in the image"

	^ self packages collect: [ :wc | wc packageName ]