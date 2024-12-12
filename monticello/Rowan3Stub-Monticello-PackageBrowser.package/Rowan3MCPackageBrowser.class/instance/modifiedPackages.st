accessing
modifiedPackages
	"return a list of modified Monticello packages (instances of MCWorkingCopy) in the image"

	^ self class _allWorkingCopies select: [ :wc | wc modified ]
