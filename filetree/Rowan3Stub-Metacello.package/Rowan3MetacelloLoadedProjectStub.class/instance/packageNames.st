accessing
packageNames
	^ self projectRegistration workingCopies collect: [:each | each packageName ]