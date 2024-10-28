"
	take a patch description as produce by mcProjectChanges.st and the changes in a JadeiteChangesBrowser
"
| changes changesBrowser application operations patch |
changes := '119140353	closest ancestor vs. loaded
M	119140097	M	avi 2/13/2004 01:05	Monticello.v3-dkh.460	MCWorkingCopy	false	accessing	description	75	description
	^ self packageNameWithStar, '' ('', ancestry ancestorString, '')''
M	DataCurator 10/28/2024 10:10	Monticello.v3-dkh.460	MCWorkingCopy	false	accessing	description	78	description
""
	^ self packageNameWithStar, '' ('', ancestry ancestorString, '')''
R	119139841	M	dkh 08/18/2020 19:07	Monticello.v3-dkh.460	MCPlatformSupport	true	*monticello	authorInitials	267	authorInitials
  ^ SessionTemps current
    at: GsPackagePolicy authorInitialsGlobalName
    ifAbsent: [ 
      (GsSession currentSession objectNamed: #''UserGlobals'')
        at: #''GS_tODE_AuthorInitials''
        ifAbsent: [ GsPackagePolicy current authorInitials ] ]
'.
	application := JadeiteApplication new.
	patch := RowanPatch fromString: changes session: GciSession current.
	changesBrowser := JadeiteChangesBrowser
						                  showOn: patch
						                  name: 'dale'
						                  application: application.
					changesBrowser owner application: application
