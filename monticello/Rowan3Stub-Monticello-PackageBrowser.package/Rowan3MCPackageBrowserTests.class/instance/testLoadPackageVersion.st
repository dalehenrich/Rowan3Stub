tests
testLoadPackageVersion
	"load a fresh copy of an already loaded package"
	
	| packageBrowser stream packageVersion |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	packageVersion := packageBrowser currentVersionOfPackageNamed: 'ConfigurationOfGLASS'.
	stream := WriteStream on: String new.
	packageBrowser loadPackageVersion: packageVersion from: 'http://seaside.gemtalksystems.com/ss/MetacelloRepository'.