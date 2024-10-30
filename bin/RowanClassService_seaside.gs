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
# overwrite of RowanPackageService method that will need to change for JfPwoR
# when run against extent0.seaside.dbf
#

run
UserGlobals 
	at: #RowanPackageService 
	put: (Rowan platform 
					serviceClassFor: 'RowanPackageService' 
					ifAbsent: [ self error: 'Cannot find RowanPackageService']).
%
category: 'Rowan3 stub'
method: RowanPackageService
rowanDirty

	^ (MCWorkingCopy allManagers 
		detect: [:wc | wc ancestry ancestors first name = name ] 
		ifNone: [ ^false ]) modified
%

#
# overwrite of RowanProjectService method that will need to change for JfPwoR
# when run against extent0.seaside.dbf
#

run
"Make some classes visible ... not sure if they need to stay visible or even need to be visible ... but 
	for now they are needed for compiles to work"
#( RowanProjectService RowanBrowserService RowanCommandResult ) 
	do: [:serviceName |
		UserGlobals 
			at: serviceName 
			put: (Rowan platform 
					serviceClassFor: serviceName 
					ifAbsent: [ self error: 'Cannot find ', serviceName])].
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
category: 'Rowan3 stub'
method: RowanProjectService
basicRefresh
	name = Rowan unpackagedName
		ifTrue: [ 
			isLoaded := false.
			RowanBrowserService new updateDictionaries.
			^ self ].
	(isLoaded := self projectIsLoaded)
		ifFalse: [ 
			existsOnDisk := false.
			self isDefinedProject
				ifFalse: [ 
					updateType := #'removedProject:'.
					^ RowanCommandResult addResult: self ] ].
	self updateIsDirty. 
	self setExistsOnDisk.
	isSkew := self isSkew.
	sha := self rowanSha.
	diskSha := self rowanDiskSha. 
	branch := self rowanBranch.
	projectUrl := self rowanProjectUrl.
	rowanProjectsHome := System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME'.
	isDiskDirty := self isGitDirty.
"
	componentServices := self componentServices.
	specService := RowanLoadSpecService new initialize: self rwProject loadSpecification asOop.
	packageConvention := self rwProject packageConvention.
"
	RowanCommandResult addResult: self
%
category: 'Rowan3 stub'
method: RowanProjectService
defaultSymbolDictionaryFromLoadSpec
	^ #UserGlobals
%


commit

## end of RowanClassService_seaside.gs
