accessing
platform
	^ platform ifNil: [ platform := Rowan3PlatformStub new ]