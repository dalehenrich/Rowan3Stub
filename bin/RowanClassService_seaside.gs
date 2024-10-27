#!/usr/bin/env superdoit_topaz
#
# to run as superdoit script with $GEMSTONE set :
#		./installRowanClassService_seaside.gs -I <path-to-topazini> -L
# 
# as seaside user (DataCurator)
login

set INPUTPAUSEONERROR on

#
# overwrite of RowanClassService method that will need to change for JfPwoR
# when run against extent0.seaside.dbf
#

run
UserGlobals 
	at: #RowanClassService 
	put: (Rowan platform 
					serviceClassFor: 'RowanClassService' 
					ifAbsent: [ self error: 'Cannot find RowanClassService']).
%
category: 'Rowan3 stub'
method: RowanClassService
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| selector warnings |
	[ [ aBehavior
		compileMethod: methodString 
		category: categorySymbol 
		using: aSymbolList 
		environmentId: 0 ]
			on: CompileError
			do: [:ex | ^ nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume ].
	selector := Behavior parseSelector: methodString for: aBehavior.
	^[(aBehavior compiledMethodAt: selector asSymbol) -> warnings] 
		on: Error
		do: [:ex | ex return: nil -> warnings]
%

commit
