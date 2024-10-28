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

#
# overwrite of RowanProjectService method that will need to change for JfPwoR
# when run against extent0.seaside.dbf
#

run
UserGlobals 
	at: #RowanProjectService 
	put: (Rowan platform 
					serviceClassFor: 'RowanProjectService' 
					ifAbsent: [ self error: 'Cannot find RowanProjectService']).
%

category: 'Rowan3 stub'
method: RowanProjectService
changes
	| jadeServer modifiedMCPackages |
	jadeServer := Rowan jadeServerClassNamed: #'JadeServer'.	
	modifiedMCPackages := MCWorkingCopy allManagers select: [:wc | wc modified ].
	changes := Array new.	
	modifiedMCPackages
		collect: [ :wc | | patch packageName |
			patch := wc changesRelativeToRepository: wc repositoryGroup repositories first.
			packageName := wc ancestry ancestors first name.
			changes add:
				(jadeServer new
					_mcDescriptionOfPatch: patch
					baseName: 'closest ancestor'
					alternateName: nil
					packageName: packageName)  ].
%

commit

## end of RowanClassService_seaside.gs
