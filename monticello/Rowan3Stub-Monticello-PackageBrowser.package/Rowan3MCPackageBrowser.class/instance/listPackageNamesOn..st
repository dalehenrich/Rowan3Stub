operations
listPackageNamesOn:  stream
	self packageNames sort
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ]
