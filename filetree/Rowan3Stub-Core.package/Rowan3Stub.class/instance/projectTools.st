accessing
projectTools
	^ projectTools ifNil: [ projectTools := Rowan3ProjectToolsStub new ]