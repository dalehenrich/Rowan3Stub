#!/usr/bin/env superdoit_topaz
#
# to run as superdoit script with $GEMSTONE set :
#		./installRowanClassService_base.gs -I <path-to-topazini> -L
# 
# as SystemUser
login

set INPUTPAUSEONERROR on

#
# overwrite of RowanClassService method that will need to change for JfPwoR
# when run against extent0.dbf (non-Seaside)
#
category: 'Rowan3 stub'
method: RowanClassService
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	[ [ method := aBehavior
		compileMethod: methodString 
		dictionaries: aSymbolList 
		category: categorySymbol 
		environmentId: 0 ]
			on: CompileError
			do: [:ex | ^ nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume ].
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] 
		on: Error
		do: [:ex | ex return: method -> warnings]
%
commit
