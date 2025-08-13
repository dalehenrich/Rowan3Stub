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
## end of RowanClassService_base.gs
