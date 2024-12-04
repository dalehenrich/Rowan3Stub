Class {
	#name : 'Rowan3BrowserToolsStub',
	#superclass : 'Object',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser [
	^ aClass definition
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName [
	"Returns a description of the receiver using object names taken from the given UserProfile."

	^ Class
		templateForSubclassOf: (GsSession currentSession objectNamed: superclassName)
		category: (category ifNil: [ 'Kernel' ])
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> isExtensionMethod: selector forClassNamed: className isMeta: meta [
	^ false
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> removeMethod: selector forClassNamed: name  isMeta: meta [
	| beh |
	beh := Rowan globalNamed: name.
	meta ifTrue: [ beh := beh class ].
	beh removeSelector: selector
]
