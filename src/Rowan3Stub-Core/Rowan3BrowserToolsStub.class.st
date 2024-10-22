Class {
	#name : 'Rowan3BrowserToolsStub',
	#superclass : 'Object',
	#category : 'Rowan3Stub-Core'
}

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForClass: aClass hybridBrowser: hybridBrowser [
	| result anArray lfsp newByteSubclass civs superClass className |
	result := String new.
	superClass := aClass superclass.
	className := aClass name asString.
	superClass
		ifNil: [ result addAll: 'nil' ]
		ifNotNil: [ result addAll: superClass name asString ].
	lfsp := Character lf asString tab.
	newByteSubclass := false.
	(aClass isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			result addAll: ' byteSubclass: '''.
			result
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			(aClass isIndexable and: [ superClass isIndexable not ])
				ifTrue: [ 
					result addAll: ' indexableSubclass: '''.
					result
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					result addAll: ' subclass: '''.
					result
						addAll: className;
						addLast: $' ] ].
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll: (aClass _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	aClass _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := aClass class allInstVarNames.
	civs removeFrom: 1 to: aClass class superClass instSize.
	civs
		do: [ :civName | 
			result addLast: $ .
			(civName includesValue: $')
				ifTrue: [ result addAll: civName _asSource ]
				ifFalse: [ result addAll: civName ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: '.
	result addAll: '#()'.	
	result
		addAll: lfsp;
		addAll: 'inDictionary: '.
	anArray := Rowan image symbolList dictionariesAndSymbolsOf: aClass.
	anArray isEmpty
		ifTrue: [ result addAll: '''''' ]
		ifFalse: [ result addAll: ((anArray at: 1) at: 1) name asString ].
	result
		add: lfsp;
		add: aClass _optionsStringForDefinition.
	result add: Character lf.
	^ result
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> classCreationTemplateForSubclassOf: superclassName category: category packageName: packageName [
	"Returns a description of the receiver using object names taken from the given
 UserProfile."

	| result lfsp |
	result := String new.
	result addAll: superclassName.
	(lfsp := Character lf asString) addAll: '  '.
	result
		addAll: ' subclass: ''';
		addAll: 'ClassName';
		addLast: $'.
	result
		addAll: lfsp;
		addAll: 'instVarNames: #( )'.
	result
		addAll: lfsp;
		addLast: 'classVars: #()'.
	result
		addAll: lfsp;
		addLast: 'classInstVars: #()'.
	result
		addAll: lfsp;
		addAll: 'poolDictionaries: #()'.
	result
		addAll: lfsp;
		addAll: 'inDictionary: ';
		addAll: 'UserGlobals'.
	^ result
]

{ #category : 'accessing' }
Rowan3BrowserToolsStub >> isExtensionMethod: selector forClassNamed: className isMeta: meta [
	^ false
]
