private
_repositoryDescriptions
	"MCRepositoryGroup holds onto all registered repositores"

	| descriptions |
	descriptions := Set new.
	^ (Rowan globalNamed: 'MCRepositoryGroup')
		ifNil: [ descriptions ]
		ifNotNil: [ :mcRepositoryGroup | 
			mcRepositoryGroup default
				repositoriesDo: [ :repo | descriptions add: repo description ].
			descriptions ]
