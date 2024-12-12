operations
listRepositoriesOn: stream
	self class _repositoryDescriptions sort
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ]
