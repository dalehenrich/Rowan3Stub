Class {
	#name : 'Rowan3PlatformStub',
	#superclass : 'Object',
	#instVars : [
		'image'
	],
	#category : 'Rowan3Stub-Core'
}

{ #category : '_private' }
Rowan3PlatformStub >> _parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId [
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError.
   Only used to parse a method to determine the selector.  "

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #'UndefinedSymbols'.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^ [ 
	UndefinedObject
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
		intoMethodDict: GsMethodDictionary new
		intoCategories: GsMethodDictionary new
		environmentId: anEnvironmentId ]
		onSynchronous: (Array with: CompileError with: CompileWarning)
		do:
			(Array
				with: [ :ex | 
					| undefSymbol symbols |
					undefSymbol := true.
					symbols := Array new.
					ex errorDetails
						do: [ :errArray | 
							(errArray atOrNil: 1) == 1031
								ifTrue: [ symbols add: (errArray atOrNil: 5) asSymbol ]
								ifFalse: [ undefSymbol := false ] ].
					undefSymbol
						ifTrue: [ 
							"attempt auto-define of undefined symbols"
							symbols do: [ :sym | undefinedSymbols at: sym put: nil ].

							[ 
							^ UndefinedObject
								compileMethod: source
								dictionaries: aSymbolList , undefinedSymbolList
								category: cat
								intoMethodDict: GsMethodDictionary new
								intoCategories: GsMethodDictionary new
								environmentId: anEnvironmentId ]
								onSynchronous: (Array with: CompileError with: CompileWarning)
								do:
									(Array with: [ :exb | undefSymbol := false ] with: [ :exc | exc resume ]) ].
					undefSymbol
						ifFalse: [ ex outer ] ]
				with: [ :ex | ex resume ])
]

{ #category : 'accessing' }
Rowan3PlatformStub >> image [
	^ image ifNil: [ image := Rowan3ImageStub new ]
]

{ #category : 'accessing' }
Rowan3PlatformStub >> loggingServiceClass [

	^ GsSession currentSession objectNamed: #RowanLoggingService
]

{ #category : 'accessing' }
Rowan3PlatformStub >> parseSelectorFrom: methodString [
	^ self parseSelectorFrom: methodString passCompileError: false
]

{ #category : 'accessing' }
Rowan3PlatformStub >> parseSelectorFrom: methodString passCompileError: passCompileError [
	| meth |
	^ [ 
	meth := self
		_parseMethod: methodString
		category: #'xyzzy'
		using: self image symbolList
		environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: [ 
			"if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil
				ifFalse: [ ^ nil ].
			meth := meth at: 1 ].
	meth selector asString ]
		on: CompileError
		do: [ :ex | 
			passCompileError
				ifTrue: [ ex pass ]
				ifFalse: [ ex return: '_____could_not_parse_selector_from_method_source_____' ] ]
]

{ #category : 'accessing' }
Rowan3PlatformStub >> serviceClassFor: className ifAbsent: absentBlock [

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone: absentBlock
]
