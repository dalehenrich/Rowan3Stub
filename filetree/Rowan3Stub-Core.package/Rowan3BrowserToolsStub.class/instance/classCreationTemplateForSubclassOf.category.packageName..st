accessing
classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName
	"Returns a description of the receiver using object names taken from the given UserProfile."

	^ Class
		templateForSubclassOf: (GsSession currentSession objectNamed: superclassName)
		category: (category ifNil: [ 'Kernel' ])