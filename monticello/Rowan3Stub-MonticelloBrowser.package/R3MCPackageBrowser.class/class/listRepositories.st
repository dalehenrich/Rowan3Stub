commands
listRepositories
	"list only repositories, including ones without any loaded packages"
	| descriptions |
	descriptions := Set new.
	MCRepositoryGroup default repositoriesDo: [:repo |
		descriptions add: repo description ].
	^ descriptions