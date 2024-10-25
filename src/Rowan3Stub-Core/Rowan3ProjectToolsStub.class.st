Class {
	#name : 'Rowan3ProjectToolsStub',
	#superclass : 'Object',
	#instVars : [
		'browserTool'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : 'instance creation' }
Rowan3ProjectToolsStub class >> new [
	"Create a new initialized instance of the receiver."

	^ self basicNew initialize
]

{ #category : 'accessing' }
Rowan3ProjectToolsStub >> browser [
	^ browserTool ifNil: [ browserTool := Rowan3BrowserToolsStub new ]
]

{ #category : 'initialization' }
Rowan3ProjectToolsStub >> initialize [
	self browser
]
