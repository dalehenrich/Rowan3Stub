private
_assertListMatchesExpectedRepositoryList: repositoryList equals: expectedRepositoryList
	"ignore non http: repositories ... error free functionality is the goal"

	| filteredStream repositoryListStream |
	filteredStream := String new writeStream.
	repositoryListStream := repositoryList readStream.
	[ repositoryListStream atEnd ]
		whileFalse: [ 
			| line |
			line := repositoryListStream nextLine.
			(line beginsWith: 'http:')
				ifTrue: [ 
					filteredStream
						nextPutAll: line;
						lf ] ].
	self assert: expectedRepositoryList equals: filteredStream contents
