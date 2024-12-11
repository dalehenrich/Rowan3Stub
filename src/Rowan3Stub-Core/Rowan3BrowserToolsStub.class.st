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

	(Class respondsTo: #'templateForSubclassOf:category:')
		ifTrue: [ 
			^ Class
				templateForSubclassOf:
					(GsSession currentSession objectNamed: superclassName)
				category: (category ifNil: [ 'Kernel' ]) ]
		ifFalse: [ 
			"logged into a non Monticello environment (SystemUser or ?)"
			| aClass  anArray dictionaryName str |
			aClass := System myUserProfile symbolList
				objectNamed: superclassName asString asSymbol.
			anArray := System myUserProfile dictionaryAndSymbolOf: aClass.
			dictionaryName := (anArray == nil
				or: [ (anArray at: 1) == GsPackagePolicy current homeSymbolDict ])
				ifTrue: [ '' ]
				ifFalse: [ 
					anArray := System myUserProfile dictionaryAndSymbolOf: (anArray at: 1).
					(anArray == nil or: [ (anArray at: 2) == #'Globals' ])
						ifTrue: [ '' ]
						ifFalse: [ (anArray at: 2) asString ] ].
			(str := superclassName asString)
				add: ' subclass: ''NameOfSubclass''';
				lf.
			aClass isBytes
				ifFalse: [ 
					str
						add: '    instVarNames: #()';
						lf ].
			str
				add:
						'    classVars: #()
    classInstVars: #()
    poolDictionaries: #()
    inDictionary: ';
				add: dictionaryName printString;
				add:
						'
    category: ''';
				add: 'Kernel';
				add: $';
				add:
						'
    options: #() '.
			^ str ]
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
