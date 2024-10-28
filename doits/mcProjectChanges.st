"
Produce an array of patch descriptions (one per package) that can be fred into the changesBrowser.st script
"
	| jadeServer modifiedMCPackages changes |
	jadeServer := Rowan jadeServerClassNamed: #'JadeServer'.	
	modifiedMCPackages := MCWorkingCopy allManagers select: [:wc | wc modified ].
	changes := Array new.	
	modifiedMCPackages
		do: [ :wc | | patch packageName |
			patch := wc changesRelativeToRepository: wc repositoryGroup repositories first.
			packageName := wc ancestry ancestors first name.
			changes add: (jadeServer new
										_mcDescriptionOfPatch: patch
										baseName: 'closest ancestor'
										alternateName: nil
										packageName: packageName) ].
	changes
