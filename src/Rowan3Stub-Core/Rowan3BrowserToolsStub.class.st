Class {
	#name : 'Rowan3BrowserToolsStub',
	#superclass : 'Object',
	#category : 'Rowan3Stub-Core'
}

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
		addAll: 'instVarNames: #( )';
		add: $).	" classVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classVars: #()'.	" classInstVars: #( <list of strings> ) "
	result
		addAll: lfsp;
		addLast: 'classInstVars: #()'.

	result
		addAll: lfsp;
		addAll: 'poolDictionaries: #('.
	result add: '#()'.	" inDictionary: <name of containing dictionary> "
	result
		addAll: lfsp;
		addAll: 'inDictionary: ';
		addAll: 'UserGlobals'.
	^ result
]
