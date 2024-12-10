accessing
browser
	^ browserTool ifNil: [ browserTool := Rowan3BrowserToolsStub new ]