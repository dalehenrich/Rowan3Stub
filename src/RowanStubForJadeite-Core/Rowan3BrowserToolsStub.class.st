Class {
	#name : 'Rowan3BrowserToolsStub',
	#superclass : 'Object',
	#category : 'RowanStubForJadeite-Core'
}

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForClass: aClass [
	| result def classCategory |
	classCategory := aClass category.
	def := aClass definition.
	def := def copyFrom: 1 to: def size - 1.	"definition has a trailing LF that we don't want"
	result := String new.
	result
		addAll: '(';
		addAll: def;
		lf;
		addAll: ') category: ', classCategory printString ;
		lf;
		yourself.
	^ result
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForClass: aClass hybridBrowser: ignored [
	^ self classCreationTemplateForClass: aClass
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName [
	"Returns a description of the receiver using object names taken from the given UserProfile."

	^ (Class respondsTo: #'templateForSubclassOf:category:')
		ifTrue: [ 
			Class
				templateForSubclassOf:
					(GsSession currentSession objectNamed: superclassName)
				category: (category ifNil: [ 'Kernel' ]) ]
		ifFalse: [ 
			| result lfsp |
			result := String new.
			result addAll: '(' , superclassName.
			result addAll: ' subclass: ''NameOfSubclass'''.
			lfsp := Character lf asString tab.
			result
				addAll: lfsp;
				addLast: 'instVarNames: #()';
				addAll: lfsp;
				addLast: 'classVars: #()';
				addAll: lfsp;
				addLast: 'classInstVars: #()';
				addAll: lfsp;
				addAll: 'poolDictionaries: #()';
				addAll: lfsp;
				addAll: 'inDictionary: Globals'.
			result
				add: lfsp;
				add: 'options: #()';
				add: ') category: ''Kernel''';
				lf.
			^ result ]
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
