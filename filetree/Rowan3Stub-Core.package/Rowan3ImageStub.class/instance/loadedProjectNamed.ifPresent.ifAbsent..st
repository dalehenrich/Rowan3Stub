accessing
loadedProjectNamed: aString ifPresent: presentBlock ifAbsent: absentBlock
	"Look up a loaded project in the loaded project registry"

	| loadedProject |
	loadedProject := self loadedProjectNamed: aString ifAbsent: absentBlock.
	^ presentBlock cull: loadedProject