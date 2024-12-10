accessing
parseSelectorFrom: methodString passCompileError: passCompileError
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