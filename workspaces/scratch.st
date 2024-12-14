Rowan3MCPackageBrowser new
	versionsOfPackageNamed: 'ConfigurationOfGLASS' .
	
(Rowan3MCPackageBrowser new packageNamed: 'ConfigurationOfGLASS') currentVersionInfo name

(MCPackage named: 'ConfigurationOfGLASS') packageVersion

MCWorkingCopy allManagers collect: [:each | each ancestry ancestorString ]

Rowan3MCPackageBrowser new packageVersions asArray sort


	| packageBrowser packageVersion  |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageVersion := packageBrowser packageNamed: 'ConfigurationOfGLASS'
