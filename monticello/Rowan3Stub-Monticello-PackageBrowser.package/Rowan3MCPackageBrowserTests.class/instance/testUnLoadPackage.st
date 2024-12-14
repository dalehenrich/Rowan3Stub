tests
testUnLoadPackage
	"load a fresh copy of an already loaded package"
	
	| packageBrowser packageName |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageName := 'ConfigurationOfGLASS'.
	packageVersion := packageBrowser currentVersionOfPackageNamed: 'ConfigurationOfGLASS'.

	packageBrowser unloadPackageNamed: packageName.

	packageBrowser loadPackageVersion: packageVersion from: 'http://seaside.gemtalksystems.com/ss/MetacelloRepository'.