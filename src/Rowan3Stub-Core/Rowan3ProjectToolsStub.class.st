Class {
	#name : 'Rowan3ProjectToolsStub',
	#superclass : 'Object',
	#instVars : [
		'browserTool'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3ProjectToolsStub >> browser [
	^ browserTool ifNil: [ browserTool := Rowan3BrowserToolsStub new ]
]