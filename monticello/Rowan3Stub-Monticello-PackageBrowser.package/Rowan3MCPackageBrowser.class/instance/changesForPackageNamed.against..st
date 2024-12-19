operations
changesForPackageNamed: packageName against: repository
	| package |
	package := MCWorkingCopy forPackage: (MCPackage named: packageName).
	package modified 
		ifFalse: [  
			"package not modified"
			^ nil ].
	patch := package changesRelativeToRepository: repository.
	^ (Rowan jadeServerClassNamed: #'JadeServer') new
		_mcDescriptionOfPatch: patch
		baseName: 'closest ancestor'
		alternateName: nil
		packageName: packageName