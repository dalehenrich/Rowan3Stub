tests
testCreateClassInPackage
	| packageBrowser  packageName  className wc |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageName := 'ATEST-PACKAGE'.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: []) equals: nil.
	packageBrowser createPackageNamed: packageName.
	self assert: (packageBrowser packageNamed: packageName)  packageName equals: packageName.
	className :=  'YYY'.
	Object subclass: className
		instVarNames: #( )
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: ''
		category: packageName
		options: #().
	self assert: (packageBrowser modifiedPackageNames includes: packageName).
	wc := packageBrowser packageNamed: packageName.
	(Rowan globalNamed: 'MCPlatformSupport') removeClassNamed: className.
	self assert: (Smalltalk at: className asSymbol ifAbsent: []) equals: nil.
	packageBrowser unloadPackageNamed: packageName.
	self assert: (packageBrowser packageNamed: packageName ifAbsent: [nil ]) equals: nil