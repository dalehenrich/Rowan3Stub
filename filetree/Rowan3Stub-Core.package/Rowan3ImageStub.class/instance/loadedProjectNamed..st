accessing
loadedProjectNamed: aString

	^ self
		loadedProjectNamed: aString
		ifPresent: [:loadedProject | loadedProject ]
		ifAbsent: [ self error: 'No loaded project named ' , aString printString , ' found' ]