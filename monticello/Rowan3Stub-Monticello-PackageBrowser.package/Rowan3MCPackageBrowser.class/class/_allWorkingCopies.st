private
_allWorkingCopies
	"if MCWorkingCopy is not available, we are running in an evironment where Monticello has not been installed"

	^ (Rowan globalNamed: 'MCWorkingCopy')
		ifNil: [ #() ]
		ifNotNil: [ :mcWorkingCopy | mcWorkingCopy allManagers ]
