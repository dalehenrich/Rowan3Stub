! Class Declarations
! Generated file, do not Edit

doit
(Object
	subclass: 'JadeServer'
	instVarNames: #(classList classOrganizers readStream writeStream selectedClass methodFilterType methodFilters selections methodCommandResult)
	classVars: #(ExternalInteger GciError GsObject OopType32 OopType64)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer
	subclass: 'JadeServer64bit'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit
	subclass: 'JadeServer64bit24'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit24
	subclass: 'JadeServer64bit3x'
	instVarNames: #(environment)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit3x
	subclass: 'JadeServer64bit32'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(JadeServer64bit32
	subclass: 'JadeServer64bit35'
	instVarNames: #(transcriptService)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #()
)
		category: 'Rowan-JadeServer';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RowanCommandResult'
	instVarNames: #(executionTime)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Simple object with the results of the Jadeite command.';
		immediateInvariant.
true.
%

doit
(RsrService
	subclass: 'RowanService'
	instVarNames: #(command commandArgs updateType organizer updates wasUpdated shouldUpdate)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Rowan Service is the abstract service class for classes that represent
first class entities in Rowan. They are transported to the client via 
ston. 

On the client, set the command & commandArgs inst vars, then tell
the browser to #issueCommand: with an array of services. A service
received without the the command inst var set will send #update to
the service. #issueCommand: should call JadeServer>>updateFromSton:
which will run the command and return a result. 

Any service that sends updates back to the client willl propogates to 
registered windows. Add a service for return to the client with:
RowanCommandResult addResult: <self>';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'Foobar'
	instVarNames: #(answer)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanAnsweringService'
	instVarNames: #(answer)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'A place to put miscellaneous commands that don''t fit 
well in other services. 

Also good asking the server questions as it gives back 
an answer whereas other services simply return updated
services.';
		immediateInvariant.
true.
%

doit
(RowanAnsweringService
	subclass: 'RowanAnsweringServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanAnsweringService
	subclass: 'RowanFileService'
	instVarNames: #(path)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Simple service that returns the contents of directories. 

A server file browser could be built on top of me.';
		immediateInvariant.
true.
%

doit
(RowanFileService
	subclass: 'RowanFileServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanAutoCommitService'
	instVarNames: #(autoCommit)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Simple service that provides updates to the client related to auto commit.';
		immediateInvariant.
true.
%

doit
(RowanAutoCommitService
	subclass: 'RowanAutoCommitServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanBrowserService'
	instVarNames: #(projects removedMethods allClasses hierarchyServices testPackages testCount dictionaries selectedClass newCachedSelectors newCachedClasses)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'RowanBrowserService handles services that fall outside
the scope of other services with an emphasis on browser-
type tasks.';
		immediateInvariant.
true.
%

doit
(RowanBrowserService
	subclass: 'RowanBrowserServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanClassService'
	instVarNames: #(name comment instVarNames classVarNames classInstVarNames superclassName subclassType poolDictionaryNames classType meta isExtension version versions oop template filters filterType methods selectedPackageServices packageName definedPackageName selectedMethods projectName hierarchyServices variables categories isTestCase expand visibleTests isNewClass updateAfterCommand isInSymbolList dictionaryName wasRemoved renamedName hasSubclasses classCategory)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Most class operations done here. 

selectedMethods - client side selection. Used after a method compile.';
		immediateInvariant.
true.
%

doit
(RowanClassService
	subclass: 'RowanClassServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanClassService
	subclass: 'RowanFillerClassService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'Used only on the client but may be passed to the server. 
Doesn''t really do much but help the hierarchy behind the scenes.';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanCompileErrorService'
	instVarNames: #(gsArguments)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanCompileErrorService
	subclass: 'RowanCompileErrorServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanComponentService'
	instVarNames: #(name componentServices packageServices projectService basename)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanComponentService
	subclass: 'RowanComponentServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanDebuggerService'
	instVarNames: #(initialProcessOop processes)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanDebuggerService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanDebuggerService( initialProcessOop processes)
';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanDefinitionService'
	instVarNames: #(name properties)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanDefinitionService
	subclass: 'RowanClassDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanDefinitionService
	subclass: 'RowanComponentDefinitionService'
	instVarNames: #(subcomponentDefinitions packageNames preloadDoitName projectDefinitionService postloadDoitName projectNames comment)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanComponentDefinitionService
	subclass: 'RowanComponentDefinitionServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanComponentDefinitionService
	subclass: 'RowanPackageGroupService'
	instVarNames: #(condition packageServices projectName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanPackageGroupService
	subclass: 'RowanPackageGroupDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'The packages in this class may or may not be loaded.';
		immediateInvariant.
true.
%

doit
(RowanComponentDefinitionService
	subclass: 'RowanSubcomponentDefinitionService'
	instVarNames: #(condition)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanSubcomponentDefinitionService
	subclass: 'RowanPlatformSubcomponentDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanDefinitionService
	subclass: 'RowanMethodDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanDefinitionService
	subclass: 'RowanPackageDefinitionService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanPackageDefinitionService
	subclass: 'RowanPackageDefinitionServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanDefinitionService
	subclass: 'RowanProjectDefinitionService'
	instVarNames: #(definitionOop componentDefinitions packageDefinitions specService comment conditionalAttributes platformProperties packageGroups showLoadedPackageGroupsOnly definition)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'RowanProjectDefinitionService presents a gui-ready presentation of a Rowan project definition and load specification.';
		immediateInvariant.
true.
%

doit
(RowanProjectDefinitionService
	subclass: 'RowanProjectDefinitionServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanDictionaryService'
	instVarNames: #(name classes hierarchyServices globals defaultTemplate testClasses classCategories)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanDictionaryService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanDictionaryService( name classes hierarchyServices globals defaultTemplate)
';
		immediateInvariant.
true.
%

doit
(RowanDictionaryService
	subclass: 'RowanDictionaryServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanFrameService'
	instVarNames: #(label method stepPoint vars oop homeMethodSelector homeMethodClassName classIsResolvable name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanFrameService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanFrameService( label method stepPoint vars oop homeMethodSelector homeMethodClassName classIsResolvable)
';
		immediateInvariant.
true.
%

doit
(RowanFrameService
	subclass: 'RowanFrameServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanInspectorService'
	instVarNames: #(oop objects myself className indexedSize visibleIndices nextIndices maxIndexedVars compileErrorArray isOop instVarNames instVarsAreRemovable isDictionary isVariable selectionOop isUnordered statusText isStringObject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanInspectorService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanInspectorService( oop objects myself className indexedSize visibleIndices nextIndices maxIndexedVars compileErrorArray isOop instVarNames instVarsAreRemovable isDictionary isVariable selectionOop isUnordered statusText isStringObject)';
		immediateInvariant.
true.
%

doit
(RowanInspectorService
	subclass: 'RowanInspectorServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanInspectorService
	subclass: 'RowanLoadSpecService'
	instVarNames: #(loadSpecOop loadSpecPath comment conditionalAttributes platformProperties componentNames loadSpec)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanLoadSpecService
	subclass: 'RowanLoadSpecServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanLoggingService'
	instVarNames: #(fileName id groupId date time comment services mode location isLogging)
	classVars: #(Current)
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanLoggingService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanLoggingService( fileName id groupId date time comment services mode location isLogging)
';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanMethodService'
	instVarNames: #(oop source selector methodDefinitions category packageName projectName className meta hasSupers hasSubs compilationWarnings isExtension inSelectedPackage references stepPoints selectedPackageServices superDisplayString accessedInstVars breakPoints testResult definedPackage isTestMethod testRunClassName failedCompile comparisonSource firstReference renamedName isMethodForBlock homeMethodOop hasMethodHistory searchString definedClassName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanMethodService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanMethodService( oop source selector methodDefinitions classService category packageName projectName className meta hasSupers hasSubs compilationWarnings isExtension inSelectedPackage references stepPoints selectedPackageServices superDisplayString accessedInstVars breakPoints testResult definedPackage isTestMethod testRunClassName failedCompile comparisonSource firstReference renamedName isMethodForBlock homeMethodOop hasMethodHistory searchString definedClassName)
';
		immediateInvariant.
true.
%

doit
(RowanMethodService
	subclass: 'RowanMethodServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanPackageService'
	instVarNames: #(projectDefinition packageName name isDirty classes defaultTemplate projectName testClasses hierarchyServices selectedClass dictionaryName)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanPackageService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanPackageService( projectDefinition packageName name isDirty classes defaultTemplate projectName testClasses hierarchyServices selectedClass)
';
		immediateInvariant.
true.
%

doit
(RowanPackageService
	subclass: 'RowanPackageServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanProcessService'
	instVarNames: #(frames oop status name errorMessage)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanProcessService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanProcessService( frames oop status)
';
		immediateInvariant.
true.
%

doit
(RowanProcessService
	subclass: 'RowanProcessServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanProjectService'
	instVarNames: #(rwProject name sha branch isSkew isDirty packages changes existsOnDisk isLoaded projectUrl rowanProjectsHome isDiskDirty projectOop specService componentServices packageGroups defaultSymbolDictionaryName packageConvention diskSha)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanProjectService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanProjectService( rwProject name sha branch isSkew isDirty packages changes existsOnDisk isLoaded projectUrl rowanProjectsHome isDiskDirty)
';
		immediateInvariant.
true.
%

doit
(RowanProjectService
	subclass: 'RowanDefinedProjectService'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanProjectService
	subclass: 'RowanProjectServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanQueryService'
	instVarNames: #(queryResults)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanQueryService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanQueryService( queryResults)
';
		immediateInvariant.
true.
%

doit
(RowanQueryService
	subclass: 'RowanQueryServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanTestService'
	instVarNames: #(name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanTestService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanTestService
';
		immediateInvariant.
true.
%

doit
(RowanTestService
	subclass: 'RowanTestServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanTranscriptService'
	instVarNames: #(string)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanTranscriptService
	subclass: 'RowanTranscriptServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RowanService
	subclass: 'RowanVariableService'
	instVarNames: #(oop key value className name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		comment: 'No class-specific documentation for RowanVariableService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanVariableService( oop key value className)
';
		immediateInvariant.
true.
%

doit
(RowanVariableService
	subclass: 'RowanVariableServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanClientServices
	options: #()
)
		category: 'Rowan-Services-Core';
		immediateInvariant.
true.
%

doit
(RsrTemplateResolver
	subclass: 'RowanClientServiceTemplateResolver'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Services-Extensions';
		immediateInvariant.
true.
%

! Class implementation for 'JadeServer'

!		Class methods for 'JadeServer'

category: 'jadeite server'
classmethod: JadeServer
theJadeiteServer
	^ SessionTemps current
		at: #'jadeiteServer'
		ifAbsentPut: [ 
			(Rowan jadeServerClassNamed: #'JadeServer64bit35') new
				initialize;
				yourself ]
%

!		Instance methods for 'JadeServer'

category: 'category'
method: JadeServer
addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler 

	| x |
	aStream lf
"1"	nextPutAll: aString; tab;
"2"	nextPutAll: aProcess asOop printString; tab;
"3"	nextPutAll: aProcess priority printString; tab;
"4"	nextPutAll: (aProcess createdByApplication ifTrue: ['Y'] ifFalse: ['']); tab; 
"5"	nextPutAll: ((x := aProcess stackId) == -1 	ifTrue: [''] ifFalse: [x printString]); tab;
"6"	nextPutAll: ((x := aProcess waitingOn) 	isNil ifTrue: [''] ifFalse: [x asOop printString]); tab;
"7"	nextPutAll: ((x := aProcess _signalTime) 	isNil ifTrue: [''] ifFalse: [(x - aScheduler _now) printString]); tab;
"8"	nextPutAll: (aProcess isPartialContinuation	ifTrue: ['partial'] ifFalse: [aProcess isContinuation ifTrue: ['full'] ifFalse: ['']]); tab;
"9"	"type: forked or main"
"10"	"live or terminated"
	yourself.
%

category: 'category'
method: JadeServer
addUser: aUserProfile toStream: aStream 

	(self oopOf: aUserProfile) printOn: aStream.
	aStream tab; nextPutAll: aUserProfile userId.
	aStream tab; nextPutAll: (aUserProfile lastLoginTime asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile loginsAllowedBeforeExpiration printOn: aStream.
	aStream tab. aUserProfile isDisabled printOn: aStream.
	aStream tab. aUserProfile activeUserIdLimit printOn: aStream.
	aStream tab.	"; nextPutAll: aUserProfile nativeLanguage asString."
	aStream tab. aUserProfile reasonForDisabledAccount printOn: aStream.
	aStream tab; nextPutAll: (aUserProfile lastPasswordChange asStringUsingFormat: #(1 2 3 $  2 1 $: true true true false)).
	aStream tab. aUserProfile passwordNeverExpires printOn: aStream.
	aStream lf.
%

category: 'category'
method: JadeServer
allSessions
	| list stream |
	stream := WriteStream on: String new.
	stream nextPutAll: '<?xml version=''1.0'' ?><sessions>'.
	list := System currentSessionNames subStrings: Character lf.
	list := list reject: [ :each | each isEmpty ].
	list := list collect: [ :each | (each subStrings at: 3) asNumber ].
	list do: [ :each | self addSessionWithId: each toStream: stream ].
	^ stream
		nextPutAll: '</sessions>';
		contents
%

category: 'category'
method: JadeServer
allUsersPasswordLimits

	| stream |
	stream := WriteStream on: String new.
	AllUsers disallowUsedPasswords printOn: stream. stream tab.
	AllUsers minPasswordSize printOn: stream. stream tab.
	AllUsers maxPasswordSize printOn: stream. stream tab.
	AllUsers maxRepeatingChars printOn: stream. stream tab.
	AllUsers maxConsecutiveChars printOn: stream. stream tab.
	AllUsers maxCharsOfSameType printOn: stream. stream tab.
	AllUsers staleAccountAgeLimit printOn: stream. stream tab.
	AllUsers passwordAgeLimit printOn: stream. stream lf.
	AllUsers disallowedPasswords do: [:each | 
		stream nextPutAll: each; tab.
	].
	stream lf. AllUsers passwordAgeWarning printOn: stream. stream lf.
	^stream contents.
%

category: 'jadeite'
method: JadeServer
autoCommitIfRequired
	| commitResult |
	Rowan serviceClass autoCommit == true ifTrue:[
		commitResult := System commitTransaction.
		RowanAutoCommitService new autoCommit:  
			(commitResult 
				ifTrue:[true] 
				ifFalse:[#failed])].
%

category: 'jadeite'
method: JadeServer
checkForDeadProcesses
	"Rowan Client Services holds onto processes while Jadeite is debugging them. 
	Sometimes Jadeite won't know when a process is terminated so we check on
	every round trip for extinguished processes"

	(SessionTemps current at: #'jadeiteProcesses' ifAbsent: [ ^ self ]) copy
		do: [ :process | 
			process _isTerminated
				ifTrue: [ (SessionTemps current at: #'jadeiteProcesses') remove: process ifAbsent:[] ] ]
%

category: 'category'
method: JadeServer
commit
	classOrganizers := Array new: 4.
	^ System commitTransaction
%

category: 'category'
method: JadeServer
contents 
	"WriteStream method to identify things that have not yet been flushed to the output. We have flushed everything!"

	^''.
%

category: 'category'
method: JadeServer
debugString: aString fromContext: anObject environment: anInteger
	anInteger == 0
		ifFalse: [ self error: 'Only environment 0 is supported in this version!' ].
	^ (RowanDebuggerService new debugStringFrom: aString)
		evaluateInContext: anObject
		symbolList: GsSession currentSession symbolList
%

category: 'category'
method: JadeServer
descriptionOfConfigOption: aString
	| dict key string |
	dict := self systemConfigAsDictionary.
	(string := dict at: aString ifAbsent: [ nil ]) notNil
		ifTrue: [ ^ string ].
	string := aString asUppercase.
	dict keys
		do: [ :each1 | 
			key := (each1 reject: [ :each2 | each2 = $_ ]) asUppercase.
			key = string
				ifTrue: [ ^ dict at: each1 ] ].
	^ ''
%

category: 'category'
method: JadeServer
dictionaryListFor: aUserProfile

	| symbolList list stream |
	symbolList := aUserProfile symbolList.
	list := symbolList namesReport subStrings: Character lf.
	list := list reject: [:each | each isEmpty].
	list := list collect: [:each | each subStrings].
	stream := WriteStream on: String new.
	list do: [:each | 
		(self oopOf: (symbolList at: (each at: 1) asNumber)) printOn: stream.
		stream tab; nextPutAll: (each at: 2); lf.
	].
	^stream contents.
%

category: 'jadeite'
method: JadeServer
dontDeleteMethods

	"sent from the Jadeite client" 

	true ifTrue:[^self]. 
	self addUser: nil toStream: nil. 
	self allUsersPasswordLimits. 
	self dictionaryListFor: nil.
	self groupListFor: nil.
	self privilegeListFor: nil.
	self userList. 
	self updateFromSton: nil. 
	self autoCommitIfRequired. 
	self gsInteractionInformFailureHandler. 
	self interactionHandlerActive.
%

category: 'category'
method: JadeServer
errorListFor: aCollection 

	| stream |
	aCollection class name == #'ErrorDescription' ifTrue: [^''].
	stream := WriteStream on: String new.
	aCollection do: [:each | 
		stream
			nextPutAll: (each at: 1) printString; tab;
			nextPutAll: (each at: 2) printString; tab;
			nextPutAll: ((2 < each size and: [(each at: 3) notNil]) ifTrue: [(each at: 3)] ifFalse: [(GemStoneError at: #English) at: (each at: 1)]); tab;
			lf.
	].
	^stream contents.
%

category: 'category'
method: JadeServer
gemLogPath 

	^''
%

category: 'category'
method: JadeServer
groupListFor: aUserProfile 

	| allGroups myGroups stream |
	allGroups := AllGroups keys asSortedCollection.
	myGroups := aUserProfile groups.
	stream := WriteStream on: String new.
	allGroups do: [:each | 
		stream nextPutAll: each; tab.
		(myGroups includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.
%

category: 'jadeite'
method: JadeServer
gsInteractionInformFailureHandler
  self interactionHandlerActive
   ifFalse: [ 
      ^ GsInteractionHandler new
        defaultBlock: [ :ignored | Error signal: 'expected a confirmation' ];
        confirmBlock: [ :interaction | interaction ok ];
        informBlock: [ :interaction |  ];
        inspectBlock: [ :interaction |  ];
        yourself ].
  ^ GsInteractionHandler new
    confirmBlock: [ :interaction | 
          | exception answer |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'confirmMessageBox:'
            args: (Array with: interaction prompt).
          answer := exception defaultAction.	"expect printString of answer back. Jadeite has limited ability to convert client objects to oops"
          answer evaluate ];
    informBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'informMessageBox:'
            args: (Array with: interaction message).
          exception defaultAction.
          nil ];
    inspectBlock: [ :interaction | 
          | exception |
          exception := ClientForwarderSend new
            receiver: self
            clientObj: 1
            selector: #'inspectServerObject:'
            args: (Array with: interaction theObject asOop).
          exception defaultAction.
          interaction theObject ]
%

category: 'jadeite'
method: JadeServer
interactionHandlerActive
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
%

category: 'category'
method: JadeServer
mcInitials: aString
        "Do initial setup and return useful information"

        | mcPlatformSupport packagePolicyEnabledFlag string x |
        string := 'Jade-' , GsSession currentSession serialNumber printString , '-' , System myUserProfile userId.
        [
                self mcInitialsA: string.
        ] whileFalse: [ "Keep shortening it till it fits!"
                string := string copyFrom: 1 to: string size - 1.
        ].
        mcPlatformSupport := self objectInBaseNamed: #'MCPlatformSupport'.
        mcPlatformSupport notNil ifTrue: [mcPlatformSupport setAuthorInitials: aString].
        packagePolicyEnabledFlag := (x := self objectInBaseNamed: #'GsPackagePolicy') isNil ifTrue: ['0'] ifFalse: [x current enabled ifTrue: ['1'] ifFalse: ['0']].
        ^System session printString , Character space asString ,
                (GsSession serialOfSession: System session) printString , Character space asString ,
                packagePolicyEnabledFlag
%

category: 'category'
method: JadeServer
mcInitialsA: aString
        "Subclasses provide error handling, typically means string is too long"

        System _cacheName: aString.
%

category: 'category'
method: JadeServer
mySessionInfo 

	| dict stream |
	stream := WriteStream on: String new.
	stream nextPutAll: self gemLogPath; cr.
	dict := System gemVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System gemConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
%

category: 'category'
method: JadeServer
nextPut: aCharacter 

	self nextPutAll: aCharacter asString.
%

category: 'category'
method: JadeServer
nextPutAll: anObject 

	| string args |
	string := self asString: anObject.
	args := Array
		with: self
		with: 1
		with: #'nextPutAll:'
		with: (Array with: string).
	System
		signal: 2336
		args: args
		signalDictionary: GemStoneError.
%

category: 'category'
method: JadeServer
objectInBaseNamed: aString

        ^[(SymbolList withAll: self class sharedPools) objectNamed: aString asSymbol] on: Error do: [:ex | ex return: nil].
%

category: 'category'
method: JadeServer
oopOf: anObject
	^ anObject asOop
%

category: 'category'
method: JadeServer
privilegeListFor: aUserProfile 

	| allPrivileges myPrivileges stream |
	allPrivileges := (aUserProfile class instVarAt: 6) at: #'PrivilegeNames'.
	myPrivileges := aUserProfile privileges.
	stream := WriteStream on: String new.
	allPrivileges do: [:each | 
		stream nextPutAll: each; tab.
		(myPrivileges includes: each) printOn: stream.
		stream lf.
	].
	^stream contents.
%

category: 'category'
method: JadeServer
processes 

	| scheduler stream |
	scheduler := ProcessorScheduler scheduler.
	stream := (WriteStream on: String new)
		nextPutAll: 'highestPriority'; 			space; nextPutAll: scheduler highestPriority 			printString; tab;
		nextPutAll: 'highIOPriority'; 			space; nextPutAll: scheduler highIOPriority 			printString; tab;
		nextPutAll: 'lowestPriority'; 			space; nextPutAll: scheduler lowestPriority 				printString; tab;
		nextPutAll: 'lowIOPriority'; 				space; nextPutAll: scheduler lowIOPriority 				printString; tab;
		nextPutAll: 'systemBackgroundPriority'; 	space; nextPutAll: scheduler systemBackgroundPriority 	printString; tab;
		nextPutAll: 'timingPriority'; 			space; nextPutAll: scheduler timingPriority 			printString; tab;
		nextPutAll: 'userBackgroundPriority'; 		space; nextPutAll: scheduler userBackgroundPriority 		printString; tab;
		nextPutAll: 'userInterruptPriority'; 		space; nextPutAll: scheduler userInterruptPriority 		printString; tab;
		nextPutAll: 'userSchedulingPriority'; 		space; nextPutAll: scheduler userSchedulingPriority 		printString; tab;
		yourself.
	scheduler readyProcesses 		do: [:each | self addProcess: each to: stream withStatus: 'ready'		scheduler: scheduler].
	scheduler suspendedProcesses 	do: [:each | self addProcess: each to: stream withStatus: 'suspended'	scheduler: scheduler].
	self waitingProcesses			do: [:each | self addProcess: each to: stream withStatus: 'waiting'	scheduler: scheduler].
	^stream contents.
%

category: 'category'
method: JadeServer
reset 
	"WriteStream protocol"
%

category: 'category'
method: JadeServer
stackForProcess: aGsProcess
	| array stream |
	Exception category: nil number: nil do: [ :ex :cat :num :args | nil ].
	array := aGsProcess _reportOfSize: 5000.
	stream := WriteStream on: String new.
	array
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ].
	^ stream contents
%

category: 'category'
method: JadeServer
step: aGsProcess inFrame: anInteger
	aGsProcess _stepOverInFrame: anInteger
%

category: 'category'
method: JadeServer
stoneInfo 

	| dict stream |
	stream := (WriteStream on: String new)
		nextPutAll: self streamType; tab;
		nextPutAll: self stringType; tab;
		cr;
		yourself.
	dict := System stoneVersionReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	dict := System stoneConfigurationReport.
	dict keys asSortedCollection do: [:each | 
		stream nextPutAll: each; tab; nextPutAll: (dict at: each) asString; cr.
	].
	stream nextPut: $%; cr.
	^stream contents
%

category: 'category'
method: JadeServer
systemConfigAsDictionary
	| char dict i line list stream |
	list := Array new.
	stream := GsFile openReadOnServer: '$GEMSTONE/bin/initial.config'.
	[ 
	[ 
	line := stream nextLine
		reject: [ :each | each == Character cr or: [ each == Character lf ] ].
	(2 < line size and: [ (line copyFrom: 1 to: 2) = '#=' ])
		ifTrue: [ list add: (WriteStream on: String new) ]
		ifFalse: [ 
			list last
				nextPutAll: line;
				cr ].
	stream atEnd not ] whileTrue: [  ] ]
		ensure: [ stream close ].
	list := list copyFrom: 3 to: list size.
	list := list collect: [ :each | each contents ].
	dict := Dictionary new.
	list
		do: [ :each | 
			line := (ReadStream on: each) nextLine.
			line = '# End of Default GemStone Configuration Options'
				ifTrue: [ ^ dict ].
			(2 < line size and: [ (line copyFrom: 1 to: 2) = '# ' ])
				ifTrue: [ 
					i := 3.
					[ i <= line size and: [ (char := line at: i) == $_ or: [ char isAlphaNumeric ] ] ]
						whileTrue: [ i := i + 1 ].
					dict at: (line copyFrom: 3 to: i - 1) put: each ] ].
	self error: 'End of file not recognized!'
%

category: 'jadeite'
method: JadeServer
updateFromSton: stonString
	| services organizer resultString |
	self checkForDeadProcesses. 
	[ 
	Rowan commandResultClass initializeResults.
	services := ((STON reader allowComplexMapKeys: true) on: stonString readStream)
		next.
	organizer := ClassOrganizer new.
	[ 
	services
		do: [ :service | 
			service organizer: organizer.
			service updateType: nil.	"Update type is only for returned commands"
			service command ifNil: [ service command: #'update' ].
			service servicePerform: service command withArguments: service commandArgs ] ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	self autoCommitIfRequired.
	Rowan loggingServiceClass current logSentServices.
	resultString := STON toString: Rowan commandResultClass results.
	^ resultString ]
		on: Exception
		do: [ :ex | 
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ]
%

category: 'category'
method: JadeServer
userList 

	| list me stream |
	list := (AllUsers asSortedCollection: [:a :b | a userId <= b userId]) asOrderedCollection.
	me := System myUserProfile.
	list
		remove: me;
		addFirst: me;
		yourself.
	stream := WriteStream on: String new.
	list do: [:each | 
		self
			addUser: each 
			toStream: stream.
	].
	^stream contents.
%

category: 'category'
method: JadeServer
waitingProcesses 

	^ProcessorScheduler scheduler waitingProcesses
%

category: 'category'
method: JadeServer
_describeMCAddition: anMCAddition on: aStream packageName: packageName
	aStream
		nextPut: $A;
		tab;
		nextPutAll: (self oopOf: anMCAddition) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCAddition definition on: aStream packageName: packageName
%

category: 'category'
method: JadeServer
_describeMCClassDefinition: anMCClassDefinition on: aStream packageName: packageName
	| string |
	string := anMCClassDefinition definitionString
		collect: [ :char | 
			char = Character lf
				ifTrue: [ Character cr ]
				ifFalse: [ char ] ].
	aStream
		nextPut: $C;
		tab;
		nextPutAll: packageName;
		tab;
		nextPutAll: string;
		lf;
		yourself
%

category: 'category'
method: JadeServer
_describeMCDefinition: anMCDefinition on: aStream packageName: packageName
	anMCDefinition isMethodDefinition
		ifTrue: [ 
			self _describeMCMethodDefinition: anMCDefinition on: aStream packageName: packageName.
			^ self ].
	anMCDefinition isOrganizationDefinition
		ifTrue: [ 
			self _describeMCOrganizationDefinition: anMCDefinition on: aStream packageName: packageName.
			^ self ].
	anMCDefinition isClassDefinition
		ifTrue: [ 
			self _describeMCClassDefinition: anMCDefinition on: aStream packageName: packageName.
			^ self ].
	self halt
%

category: 'category'
method: JadeServer
_describeMCMethodDefinition: anMCMethodDefinition on: aStream packageName: packageName
	| source |
	source := anMCMethodDefinition source.
	aStream
		nextPut: $M;
		tab;
		nextPutAll: anMCMethodDefinition timeStamp;
		tab;
		nextPutAll: packageName;
		tab;
		nextPutAll: anMCMethodDefinition className;
		tab;
		nextPutAll: anMCMethodDefinition classIsMeta printString;
		tab;
		nextPutAll: anMCMethodDefinition category;
		tab;
		nextPutAll: anMCMethodDefinition selector;
		tab;
		nextPutAll: source size printString;
		tab;
		nextPutAll: source;
		lf
%

category: 'category'
method: JadeServer
_describeMCModification: anMCModification on: aStream packageName: packageName
	aStream
		nextPut: $M;
		tab;
		nextPutAll: (self oopOf: anMCModification) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCModification obsoletion on: aStream packageName: packageName.
	self _describeMCDefinition: anMCModification modification on: aStream packageName: packageName
%

category: 'category'
method: JadeServer
_describeMCOrganizationDefinition: anMCOrganizationDefinition on: aStream
	aStream
		nextPut: $O;
		tab;
		yourself.
	anMCOrganizationDefinition categories
		do: [ :each | 
			aStream
				nextPutAll: each;
				tab ].
	aStream lf
%

category: 'category'
method: JadeServer
_describeMCRemoval: anMCRemoval on: aStream packageName: packageName
	aStream
		nextPut: $R;
		tab;
		nextPutAll: (self oopOf: anMCRemoval) printString;
		tab;
		yourself.
	self _describeMCDefinition: anMCRemoval definition on: aStream packageName: packageName
%

category: 'category'
method: JadeServer
_mcDescriptionOfPatch: aPatch baseName: aString1 alternateName: aString2 packageName: packageName
	| stream |
	stream := WriteStream on: String new.
	(self oopOf: aPatch) printOn: stream.
	stream
		tab;
		nextPutAll:
				(aString1 isNil
						ifTrue: [ 'loaded' ]
						ifFalse: [ aString1 ]);
		nextPutAll: ' vs. ';
		nextPutAll:
				(aString2 isNil
						ifTrue: [ 'loaded' ]
						ifFalse: [ aString2 ]);
		lf.
	aPatch operations
		do: [ :each | 
			each isAddition
				ifTrue: [ self _describeMCAddition: each on: stream packageName: packageName ].
			each isModification
				ifTrue: [ self _describeMCModification: each on: stream packageName: packageName ].
			each isRemoval
				ifTrue: [ self _describeMCRemoval: each on: stream packageName: packageName ] ].
	^ stream contents
%

! Class implementation for 'JadeServer64bit'

!		Instance methods for 'JadeServer64bit'

category: 'category'
method: JadeServer64bit
addSessionWithId: anInteger toStream: aStream

	[
		super
			addSessionWithId: anInteger
			toStream: aStream.
	] on: Error do: [:ex | 
		ex resume: '?????'.
	].

%

category: 'category'
method: JadeServer64bit
addUser: aUserProfile toStream: aStream

	[
		super
			addUser: aUserProfile 
			toStream: aStream.
	] on: Error do: [:ex | 
		aStream lf.
		ex return.
	].

%

category: 'category'
method: JadeServer64bit
asString: anObject

	^[
		super asString: anObject.
	] on: Error do: [:ex | 
		ex return: '???'.
	].

%

category: 'category'
method: JadeServer64bit
homeMethodFor: aGsMethod

	^[aGsMethod homeMethod] on: Error do: [:ex | ex return: aGsMethod]
%

category: 'category'
method: JadeServer64bit
installTranscript

	[
		super installTranscript.
	] on: Error do: [:ex | 
		ex return.
	].

%

category: 'category'
method: JadeServer64bit
mcInitialsA: aString

	^[
		super mcInitialsA: aString.
		true.
	] on: Error do: [:ex | 
		ex return: false.
	].

%

category: 'category'
method: JadeServer64bit
objectForOop: anInteger

	^Object _objectForOop: anInteger.

%

category: 'category'
method: JadeServer64bit
recompile: aMethod withSource: aString
	| behavior |
	behavior := aMethod inClass.
	[[ behavior rwCompileMethod: aString
				category: (self _behavior: behavior categoryOfSelector: aMethod selector) ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
								on: RwPerformingUnpackagedEditNotification
								do: [:ex | ex resume ].
			Rowan serviceClass rowanFixMe.	"need to handle compile errors"
			^true
%

category: 'category'
method: JadeServer64bit
sbRemoveKey: aSymbol fromDictionary: aDictionary

	| aClass array |
	aClass := aDictionary at: aSymbol.
	array := self dictionaryAndSymbolOf: aClass.
	((array at: 1) == aDictionary and: [
		(array at: 2) == aSymbol and: [
		(Class canUnderstand: #'removeFromSystem') and: [	"mark package as modified"
		aClass removeFromSystem]]]) ifFalse: [
			aDictionary removeKey: aSymbol.
		].
%

category: 'category'
method: JadeServer64bit
sourceForProcess: gsProcess frame: level

	^[
		self
			_sourceForProcess: gsProcess 
			frame: level.
	] on: Error do: [:ex | 
			ex return: (self asAsciiString: ('?????' , ex description , Character cr asString , (GsProcess stackReportToLevel: 50))).
	].

%

category: 'category'
method: JadeServer64bit
streamType

	| isLegacy type |
	type := Globals at: #'PositionableStream_position' ifAbsent: [#'Legacy'].
	(Globals includesKey: #'PositionableStreamLegacy') ifFalse: [^type].
	isLegacy := PositionableStream isLegacyStreamImplementation.
	(type = #'Legacy') == isLegacy ifTrue: [^type].
	self error: 'Inconsistent PositionableStream configuration'.

%

category: 'category'
method: JadeServer64bit
stringType

	^(Globals at: #StringConfiguration ifAbsent: [String]) name

%

category: 'category'
method: JadeServer64bit
systemBrowser: aString

	[
		^super systemBrowser: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].

%

category: 'category'
method: JadeServer64bit
systemBrowserSTON: aString

	[
		^super systemBrowserSTON: aString.
	] on: Error do: [:ex |
		readStream := nil.
		ex pass.
	].

%

category: 'category'
method: JadeServer64bit
_oopAndStringFor: anObject

	^[
		super _oopAndStringFor: anObject.
	] on: Error do: [:ex | 
		ex return: 0 -> ('<ERROR IN #printString for ' , anObject class name , '>').
	].

%

category: 'category'
method: JadeServer64bit
_stepPointFromProcess: gsProcess frame: aFrame method: gsMethod level: anInteger

	^gsProcess _stepPointAt: anInteger

%

category: 'category'
method: JadeServer64bit
_trimStackOf: aGsProcess toLevel: anInteger

	^[
		super
			_trimStackOf: aGsProcess 
			toLevel: anInteger.
	] on: Error do: [:ex | 
		self 
			_trimStackOf: aGsProcess 
			toLevel: anInteger - 1.
		ex return.
	].

%

! Class implementation for 'JadeServer64bit24'

!		Instance methods for 'JadeServer64bit24'

category: 'category'
method: JadeServer64bit24
inspect: anObject

	^(self isClientForwarder: anObject)
		ifTrue: [self inspectClientForwarder: anObject]
		ifFalse: [super inspect: anObject].

%

category: 'category'
method: JadeServer64bit24
inspectClientForwarder: anObject

	| stream |
	(stream := WriteStream on: String new)
		nextPutAll: 'ClientForwarder'; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf;
		nextPut: $1; lf;
		nextPutAll: 'clientObject'; tab;
		yourself.
	self print: (self oopOf: anObject clientObject) on: stream.
	stream lf; nextPutAll: (self printStringOf: anObject).
	^stream contents.

%

category: 'category'
method: JadeServer64bit24
isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder' 

%

category: 'category'
method: JadeServer64bit24
oopOf: anObject

	^Reflection oopOf: anObject.

%

category: 'category'
method: JadeServer64bit24
printStringOf: anObject

	^(self isClientForwarder: anObject)
		ifFalse: [anObject printString asUnicodeString]
		ifTrue: ['aClientForwarder(' , anObject clientObject printString , ')'].
%

category: 'category'
method: JadeServer64bit24
registerOBNotificationsForPlatform: platform clientForwarder: clientForwarder

	super
		registerOBNotificationsForPlatform: platform 
		clientForwarder: clientForwarder.
	platform 
		registerMultipleChoiceClientForwarder: clientForwarder;
		yourself.

%

! Class implementation for 'JadeServer64bit3x'

!		Instance methods for 'JadeServer64bit3x'

category: 'category'
method: JadeServer64bit3x
addMethodCategoryNamesToMethodFilters

	classList do: [:each | 
		each 
			env: environment 
			categorysDo:[ :categName :selectors | methodFilters add: categName ].
	].

%

category: 'category'
method: JadeServer64bit3x
addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler

	| instVarNumber modeInfo modeInfo_forked modeInfo_terminated |
	super addProcess: aProcess to: aStream withStatus: aString scheduler: aScheduler.
	(instVarNumber := GsProcess instVarNames indexOf: #'modeInfo') == 0 ifTrue: [^self].
	modeInfo := aProcess instVarAt: instVarNumber.
	(modeInfo_forked := GsProcess _classVars at: #'ModeInfo_forked' ifAbsent: [nil]) ifNil: [^self].
	(modeInfo_terminated := GsProcess _classVars at: #'ModeInfo_terminated' ifAbsent: [nil]) ifNil: [^self].
	aStream
"9"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_forked) ifTrue: ['forked'] ifFalse: ['main']); tab;
"10"		nextPutAll: (0 < (modeInfo bitAnd: modeInfo_terminated) ifTrue: ['terminated'] ifFalse: ['']); tab;
		yourself.

%

category: 'category'
method: JadeServer64bit3x
asString: anObject

	(anObject isKindOf: String) ifTrue: [^anObject asUnicodeString].
	(anObject _class name == #'ClientForwarder') ifTrue: [^'aClientForwarder(' , (self asString: anObject clientObject) , ')'].
	^[
		anObject printString asUnicodeString.
	] on: Error , Admonition do: [:ex | 
		ex return: '<<printString error: ' , ex description , '>>'.
	].
%

category: 'category'
method: JadeServer64bit3x
categoryOfMethod: aMethod

	| category selector |
	(selector := aMethod selector) isNil ifTrue: [^''].
	category := self _behavior: aMethod inClass categoryOfSelector: aMethod selector.
	category ifNil: [category := #'other'].
	^category.

%

category: 'category'
method: JadeServer64bit3x
class: aClass includesSelector: aSelector

	^aClass includesSelector: aSelector asSymbol environmentId: environment.

%

category: 'category'
method: JadeServer64bit3x
compiledMethodAt: aSymbol inClass: aClass

	| method | 
	method := aClass compiledMethodAt: aSymbol environmentId: environment.
	method ifNil: [self error: 'Lookup failed for selector ' , aSymbol , ' inClass ' , aClass name , ' in environment ' , environment printString].
	^method.
%

category: 'category'
method: JadeServer64bit3x
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings | 

	[[ 
			[[ method := aBehavior rwCompileMethod: methodString category: categorySymbol ]
					on: RwExecuteClassInitializeMethodsAfterLoadNotification
					do: [:ex | ex resume: false ]]
						on: RwPerformingUnpackagedEditNotification
						do: [:ex | ex resume ].
		] on: CompileError do: [:ex |
		^nil -> (ex gsArguments at: 1)
	]] on: CompileWarning do: [:ex |
		warnings := ex gsArguments at: 1.
		ex resume.
	].
	^[	
		(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings.
	] on: Error do: [:ex | 
		ex return: method -> warnings.
	].
%

category: 'category'
method: JadeServer64bit3x
debugString: aString fromContext: anObject environment: anInteger

	[
		^super debugString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].

%

category: 'category'
method: JadeServer64bit3x
describeMethod: aMethod
	"Provide info needed to create a GsMethod in Jade client"

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super describeMethod: aMethod
%

category: 'category'
method: JadeServer64bit3x
environment

	^environment
%

category: 'category'
method: JadeServer64bit3x
environment: anInteger

	environment := anInteger.

%

category: 'category'
method: JadeServer64bit3x
environmentForMethod: aGsNMethod

	^aGsNMethod environmentId
%

category: 'category'
method: JadeServer64bit3x
executeString: aString fromContext: anObject environment: anInteger

	[
		^super executeString: aString fromContext: anObject environment: anInteger.
	] on: CompileWarning do: [:ex | 
		ex resume.
	].

%

category: 'category'
method: JadeServer64bit3x
homeMethodFor: aGsMethod

	| result |
	result := super homeMethodFor: aGsMethod.
	(result class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , result class name].
	^result
%

category: 'category'
method: JadeServer64bit3x
initialize

	super initialize.
	environment := 0.

%

category: 'category'
method: JadeServer64bit3x
inspect: anObject
	| dynamic dynamicSize indexedSize instVarNames namedSize stream string isRcBag |
	(self isClientForwarder: anObject) ifTrue: [^self inspectClientForwarder: anObject].
	(stream := WriteStream on: String new)
		nextPutAll: anObject class name; tab;
		yourself.
	(self oopOf: anObject) printOn: stream.
	stream lf.
	(anObject isKindOf: Dictionary superclass) ifTrue: [^self inspectDictionary: anObject on: stream].
	instVarNames := anObject class allInstVarNames.
	namedSize := instVarNames size.
	dynamic := anObject dynamicInstanceVariables.
	dynamicSize := dynamic size.
	isRcBag := anObject class name == #RcIdentityBag.
	indexedSize := (anObject class isNsc or: [anObject class isIndexable]) ifFalse: [
		0.
	] ifTrue: [
		isRcBag ifTrue: [ anObject size] ifFalse: [(anObject _primitiveSize - namedSize)].
	].
	
	namedSize + dynamicSize + indexedSize printOn: stream.
	stream lf.
	1 to: instVarNames size do: [:i | 
		stream nextPutAll: (instVarNames at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: stream.
		stream lf.
	].
	1 to: dynamicSize do: [:i | 
		stream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: stream.
		stream lf.
	].
	isRcBag
		ifTrue: [ |aBag |
			aBag := anObject _asIdentityBag.
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (aBag _at: i )) on: stream.
				stream lf]]
		ifFalse: [
			1 to: indexedSize do: [:i | 
				i printOn: stream.
				stream tab.
				self print: (self oopOf: (anObject _primitiveAt: i + namedSize)) on: stream.
				stream lf] ].

	(string := anObject printString) size > 100000 ifTrue: [string := (string copyFrom: 1 to: 100000) , '...'].
	string class == String ifFalse: [
		string := String withAll: (string collect: [:each | (32 <= each codePoint and: [each codePoint <= 255]) ifTrue: [each] ifFalse: [$?]]).
	].
	^stream 
		nextPutAll: string; 
		contents.
%

category: 'category'
method: JadeServer64bit3x
inspectNamedInstanceVariablesOf: anObject on: aStream

	| list dynamic size |
	list := anObject class allInstVarNames.
	dynamic := anObject dynamicInstanceVariables.
	size := list size + dynamic size.
	anObject class format > 0 ifTrue: [
		size := size + (anObject _basicSize min: 200).
	].
	size printOn: aStream.
	aStream lf.
	1 to: list size do: [:i | 
		aStream nextPutAll: (list at: i); tab.
		self print: (self oopOf: (anObject instVarAt: i)) on: aStream.
		aStream lf.
	].
	1 to: dynamic size do: [:i | 
		aStream nextPutAll: (dynamic at: i); tab.
		self print: (self oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) on: aStream.
		aStream lf.
	].

%

category: 'transcript'
method: JadeServer64bit3x
installTranscript

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: self.
%

category: 'category'
method: JadeServer64bit3x
methodSignatureForSelector: aSymbol
	"Ruby bridge methods can have some strange selectors!"

	| class comma i j method source |
	environment ~~ 1 ifTrue: [^aSymbol].
	class := selectedClass whichClassIncludesSelector: aSymbol environmentId: environment.
	method := class compiledMethodAt: aSymbol environmentId: environment.
	source := (method sourceString subStrings: Character lf) first trimBlanks.
	(4 < source size and: [(source copyFrom: 1 to: 4) = 'def ']) ifTrue: [
		source := source copyFrom: 5 to: source size.
		(source includes: $#) ifTrue: [source := (source copyFrom: 1 to: (source indexOf: $#) - 1) trimBlanks].
		^source.
	].
	(i := aSymbol indexOf: $#) == 0 ifTrue: [^aSymbol].
	source := aSymbol copyFrom: 1 to: i - 1.
	(aSymbol copyFrom: i to: aSymbol size) = '#0__' ifTrue: [^source].
	comma := ''.
	source add: $(.
	j := (aSymbol at: i + 1) asString asNumber.
	1 to: j do: [:k | 
		source 
			add: comma;
			add: 'arg'.
		1 < j ifTrue: [source add: k printString].
		comma := $,.
	].
	(aSymbol at: i + 2) == $* ifTrue: [
		source 
			add: comma;
			add: (0 == j ifTrue: ['args'] ifFalse: ['rest']).
		comma := $,.
	].
	aSymbol last == $& ifTrue: [
		source
			add: comma;
			add: '&block'.
	].
	source add: $).
	^source.

%

category: 'category'
method: JadeServer64bit3x
nameOfFirstDictionaryReferencing: aGlobal

	| list |
	list := self symbolList dictionariesAndSymbolsOf: aGlobal.
	list isEmpty ifTrue: [^''].
	^list first first name
%

category: 'category'
method: JadeServer64bit3x
nextPutAll: anObject

	| exception |
	exception := ClientForwarderSend new 
		receiver: self 
		clientObj: 2
		selector:#'nextPutAll:'
		args: (Array with: (self asString: anObject)).
	exception defaultAction.  "return error direct to GCI"
%

category: 'category'
method: JadeServer64bit3x
objectSecurityPolicyFor: anObject

	^anObject objectSecurityPolicy.

%

category: 'category'
method: JadeServer64bit3x
packagePolicy: aPackagePolicy includesSelector: aSymbol forClass: aClass

	| dict |
	^aPackagePolicy notNil and: [
		(dict := aClass transientMethodDictForEnv: environment) notNil and: [
			dict keys includes: aSymbol.		"includesKey: requires protected mode!"
		].
	].

%

category: 'category'
method: JadeServer64bit3x
sbBrowseMethodsWithPragma: anOrderedCollection

	| methods symbol |
	symbol := anOrderedCollection removeFirst asSymbol.
	methods := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal class with: eachGlobal) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						(eachMethod pragmas anySatisfy: [:eachPragma | symbol == eachPragma keyword]) ifTrue: [methods add: eachMethod].
					].
				].
			].
		].
	].
	writeStream 
		nextPutAll: 'browseMethodsWithPragma'; lf;
		nextPutAll: (self streamOfMethods: methods);
		yourself.

%

category: 'category'
method: JadeServer64bit3x
sbClassComment: anOrderedCollection

	(self sbClassFrom: anOrderedCollection) thisClass rwComment: self sbNextParagraph trimSeparators.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer64bit3x
sbFileOutMethod: anOrderedCollection

	| aClass |
	aClass := self sbClassFrom: anOrderedCollection.
	writeStream nextPutAll: (aClass fileOutMethod: anOrderedCollection removeFirst asSymbol environmentId: environment).


%

category: 'category'
method: JadeServer64bit3x
sbListMethodPragmas: aList

	| pragmas |
	pragmas := IdentitySet new.
	self symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				(Array with: eachGlobal with: eachGlobal class) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						pragmas addAll: (eachMethod pragmas collect: [:eachPragma | eachPragma keyword]).
					].
				].
			].
		].
	].
	pragmas asSortedCollection do: [:each | writeStream nextPutAll: each; tab].
	^pragmas
%

category: 'category'
method: JadeServer64bit3x
sbMethod: anOrderedCollection

	| behavior category string gsMethod |
	behavior := self sbClassFrom: anOrderedCollection.
	category := anOrderedCollection notEmpty ifTrue: [anOrderedCollection removeFirst] ifFalse: ['other'].
	string := self sbNextParagraph.
	gsMethod := behavior
		compileMethod: string 
		dictionaries: self symbolList 
		category: category asSymbol 
		environmentId: environment.
	selections 
		at: #'methodCategory' 	put: (self _behavior: gsMethod inClass categoryOfSelector: gsMethod selector) asString;
		at: #'method'			put: gsMethod selector asString;
		yourself.
	self systemBrowserUpdate.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodBreakPointsFor: aMethod
	"Answers an Array of step points"

	| list array |
	(array := aMethod _allBreakpoints) isNil ifTrue: [^#()].      "{ breakpointNumber1 . method . ipOffset1 . ... }"
	list := Array new.
	1 to: array size by: 3 do:[:k |
		list add: (aMethod
			_stepPointForMeth: (array at: k + 1)
			ip: (array at: k + 2)).
	].
	^list.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodPragmas

	selectedClass notNil ifTrue:[
		methodCommandResult selections add: selectedClass asString].
	methodCommandResult updateMethodPragmas.
	methodCommandResult writeMethodFiltersTo: writeStream. 
	methodCommandResult updateMethodFilterSelections: self nextLineAsList 
		hasCategoryOverride: (selections at: #'methodCategory' ifAbsent: [nil]).
	methodFilters := methodCommandResult selectionNames.
	methodCommandResult writeSelectionsTo: writeStream. 


%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByCategories

	| selectors |
	selectors := IdentitySet new.
	classList do: [:eachClass |
		(eachClass selectorsForEnvironment: environment) do: [:eachSelector |
			(methodFilters isEmpty or: [
				| category |
				category := eachClass categoryOfSelector: eachSelector environmentId: environment.
				(category isNil and: [ methodFilters includes: #'other']) or: [ methodFilters includes: category asSymbol]]) ifTrue: [
				| method |
				method := eachClass compiledMethodAt: eachSelector environmentId: environment.
				(method respondsTo: #isRubyBridgeMethod)
					ifTrue: [ 
						method isRubyBridgeMethod ifFalse: [
							selectors add: eachSelector ] ]
					ifFalse: [ selectors add: eachSelector ]
			].
		].
	].
	^selectors.
%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByPragmas

	| selectors |
	selectors := IdentitySet new.
	methodFilters isEmpty ifTrue: [^#()].
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		gsMethod pragmas do: [:eachPragma | 
			( methodFilters includes: eachPragma keyword) ifTrue: [selectors add: eachSelector].
		].
	].
	^selectors.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodsByVariables

	| selectors filters |
	selectors := IdentitySet new.
	filters := IdentitySet withAll: (methodFilters select: [:each | each isSymbol]).
	(selectedClass selectorsForEnvironment: environment) do: [:eachSelector | 
		| gsMethod |
		gsMethod := selectedClass compiledMethodAt: eachSelector environmentId: environment.
		(gsMethod instVarsAccessed * filters) notEmpty ifTrue: [selectors add: eachSelector].
	].
	^selectors.

%

category: 'category'
method: JadeServer64bit3x
sbUpdateMethodStepPointsFor: aMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aMethod _allDebugInfo: 10) ifNil: [^#()].
	list := (self homeMethodFor: aMethod)  _sourceOffsets.
	list := list collect: [:each | 		"exists as far back as 32-bit 6.3.0"
		| index selector |
		selector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [selector := selectors at: index + 1].
		each -> selector.
	].
	^list.


%

category: 'category'
method: JadeServer64bit3x
selectedClassOverridesSelector: aSymbol

	^selectedClass superclass notNil and: [(selectedClass superclass whichClassIncludesSelector: aSymbol environmentId: environment) ~~ nil].

%

category: 'transcript'
method: JadeServer64bit3x
uninstallTranscript

	Transcript class name == #'TranscriptStreamPortable' ifFalse: [^self].
	SessionTemps current at: #'TranscriptStream_SessionStream' put: nil.
%

category: 'category'
method: JadeServer64bit3x
_allSelectors

	| allSelectors |
	allSelectors := IdentitySet new.
	self classOrganizer classes do: [:each | 
		allSelectors addAll: (each selectorsForEnvironment: environment); addAll: (each class selectorsForEnvironment: environment).
	].
	^allSelectors
%

category: 'category'
method: JadeServer64bit3x
_behavior: aBehavior categoryOfSelector: aSymbol

	^aBehavior categoryOfSelector: aSymbol environmentId: environment
%

category: 'category'
method: JadeServer64bit3x
_describeMethod: aMethod

	(aMethod class name == #'GsNMethod') ifFalse: [self error: 'Expected a GsNMethod but got ' , aMethod class name].
	^super _describeMethod: aMethod
%

category: 'category'
method: JadeServer64bit3x
_methodsFor: aClass categories: aList

	| methods |
	methods := IdentitySet new.
	aList do: [:eachCategory | 
		(aClass includesCategory: eachCategory) ifTrue: [
			(aClass selectorsIn: eachCategory) do: [:eachSelector |
				methods add: (self compiledMethodAt: eachSelector inClass: aClass).
			].
		].
	].
	^methods.


%

category: 'category'
method: JadeServer64bit3x
_stepPointsForBreaksInMethod: gsMethod
	"Presumably there is a way to do this, just not the same as 32-bit and 64-bit 2.x"

	^#()
%

! Class implementation for 'JadeServer64bit32'

!		Instance methods for 'JadeServer64bit32'

category: 'category'
method: JadeServer64bit32
dictionaryAndSymbolOf: aClass

	| array |
	array := self symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].

%

category: 'category'
method: JadeServer64bit32
dictionaryAndSymbolOf: aClass forUser: aUserProfile

	| array |
	array := aUserProfile symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].

%

category: 'category'
method: JadeServer64bit32
gsPackagePolicy

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	class enabled ifFalse: [^nil].
	^class current

%

category: 'category'
method: JadeServer64bit32
stepThrough: aGsProcess inFrame: anInteger
  aGsProcess _stepThrough
%

! Class implementation for 'JadeServer64bit35'

!		Instance methods for 'JadeServer64bit35'

category: 'other'
method: JadeServer64bit35
nextPutAll: string
	transcriptService ifNil:[^super nextPutAll: string].
	transcriptService nextPutAll: string.
%

category: 'other'
method: JadeServer64bit35
stepThrough: aGsProcess inFrame: anInteger
	aGsProcess setStepThroughBreaksAtLevel: anInteger breakpointLevel: nil
%

category: 'accessing'
method: JadeServer64bit35
transcriptService: object
	transcriptService := object
%

! Class implementation for 'RowanCommandResult'

!		Class methods for 'RowanCommandResult'

category: 'accessing'
classmethod: RowanCommandResult
addResult: service
	"service command: nil;
			commandArgs: nil."
	self updateClientBoundServices: service.
	^service
%

category: 'accessing'
classmethod: RowanCommandResult
basicAddResult: service
	self results add: service
%

category: 'initailize'
classmethod: RowanCommandResult
initializeResults

	SessionTemps current at: #rowanCommandResults put: Array new.
%

category: 'instance creation'
classmethod: RowanCommandResult
new

	| inst |
	inst := super new initialize.
	self addResult: inst.
	^inst
%

category: 'accessing'
classmethod: RowanCommandResult
removeResult: aResult

	self results remove: aResult
%

category: 'accessing'
classmethod: RowanCommandResult
results

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanCommandResults ifAbsentPut: [Array new]
%

category: 'private'
classmethod: RowanCommandResult
updateClientBoundServices: clientBoundService
	"We're about to add a service to the results collection. 
	That service will be sent to the client. Since services
	are not canonical, we need to do some housekeeping
	to ensure that we don't already have this service 
	somewhere in other client-bound services"
	(self results includes: clientBoundService) ifTrue:[
		self removeResult: clientBoundService].
	self basicAddResult: clientBoundService. 
	self results do:[:service |
		service updateInternalService: clientBoundService.
		clientBoundService updateInternalService: service].
%

!		Instance methods for 'RowanCommandResult'

category: 'accessing'
method: RowanCommandResult
command: anObject

	"results don't have commands"
%

category: 'accessing'
method: RowanCommandResult
commandArgs: anObject

	"results don't have commandArgs"
%

category: 'initialization'
method: RowanCommandResult
initialize
%

category: 'testing'
method: RowanCommandResult
isMethodService

	^false
%

category: 'private'
method: RowanCommandResult
rowanFixMe
%

category: 'accessing'
method: RowanCommandResult
updateInternalService: service
%

! Class implementation for 'RowanService'

!		Class methods for 'RowanService'

category: 'autocommit'
classmethod: RowanService
autoCommit

	^SessionTemps current at: #'Jadeite_AutoCommit' ifAbsentPut: [false]
%

category: 'autocommit'
classmethod: RowanService
breakPointsAreEnabled

	^SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' ifAbsentPut: [true]
%

category: 'autocommit'
classmethod: RowanService
flipAutoCommit
	| newValue |
	newValue := self autoCommit == #failed ifTrue:[false] ifFalse:[self autoCommit not].
	^self setAutoCommit: newValue
%

category: 'testing'
classmethod: RowanService
isRowanClientServicesVersionSupported: lowerLimit
	^ self
		isRowanClientServicesVersionSupported: RowanService version
		lowerLimit: lowerLimit
%

category: 'testing'
classmethod: RowanService
isRowanClientServicesVersionSupported: versionString lowerLimit: lowerLimit
	"return a two element arg showing if the version is supported and the version"

	| low high version |
	low := RwSemanticVersionNumber fromString: lowerLimit.
	high := low copy incrementMinorVersion.
	version := RwSemanticVersionNumber fromString: versionString.
	^ low <= version and: [ version < high ]
%

category: 'instance creation'
classmethod: RowanService
new

	^super new initialize
%

category: 'other'
classmethod: RowanService
rowanFixMe

	"send this message to see everywhere that GS_Jade should be fixed"
%

category: 'examples'
classmethod: RowanService
sampleService

	^self new sampleService
%

category: 'autocommit'
classmethod: RowanService
setAutoCommit: object

	^SessionTemps current at: #'Jadeite_AutoCommit' put: object
%

category: 'autocommit'
classmethod: RowanService
setBreakPointsAreEnabled: boolean
  ^ SessionTemps current at: #'Jadeite_BreakPointsAreEnabled' put: boolean
%

category: 'rsr'
classmethod: RowanService
templateClassName

	^#RowanService
%

category: 'accessing'
classmethod: RowanService
version
	^ '3.0.8'
%

category: 'accessing'
classmethod: RowanService
versionRangeHigh: lowRange
	| low high |
	low := RwSemanticVersionNumber fromString: lowRange.
	high := low incrementMinorVersion.
	^high printString
%

!		Instance methods for 'RowanService'

category: 'commands support'
method: RowanService
addAllSubclassHierarchiesOf: theClass to: hierarchies
	(self organizer subclassesOf: theClass)
		do: [ :subclass | 
			hierarchies
				add:
					(self organizer allSuperclassesOf: theClass) , (Array with: theClass with: subclass).
			self addAllSubclassHierarchiesOf: subclass to: hierarchies ]
%

category: 'other'
method: RowanService
answer: anObject

	| answeringService |
	answeringService := RowanAnsweringService new. 
	answeringService answer: anObject. 
	RowanCommandResult addResult: answeringService.
%

category: 'commands support'
method: RowanService
autoCommitIfRequired
	| commitResult |
	self class autoCommit == true
		ifTrue: [ 
			[ commitResult := RowanBrowserService new commitTransaction ]
				on: Error
				do: [ :e | 
					RowanAutoCommitService new autoCommit: #'failed'.
					^ self ].
			RowanAutoCommitService new
				autoCommit:
					(commitResult
						ifTrue: [ true ]
						ifFalse: [ #'failed' ]) ]
%

category: 'rowan'
method: RowanService
browserTool

	^self projectTools browser
%

category: 'rsr'
method: RowanService
checkForDeadProcesses
	"Rowan Client Services holds onto processes while Jadeite is debugging them. 
	Sometimes Jadeite won't know when a process is terminated so we check on
	every round trip for extinguished processes"

	RowanProcessServiceServer processServices copy
		keysAndValuesDo: [ :process :processService | 
			process _isTerminated
				ifTrue: [ RowanProcessServiceServer removeProcessServiceFor: process ] ]
%

category: 'commands support'
method: RowanService
classHierarchy: theClasses
	| superclassChains levels services hierarchies toExpand hierarchyServices |
	superclassChains := self superclassChainsFor: theClasses.
	hierarchies := self extendHierarchies: superclassChains forClasses: theClasses.
	levels := self hierarchiesByLevel: hierarchies.
	services := Dictionary new.
	toExpand := Set new.
	self
		services: services
		from: levels
		expand: toExpand
		classes: theClasses.
	hierarchyServices := services reject: [ :array | array isEmpty ].
	hierarchyServices copy
		keysAndValuesDo: [ :key :value | 
			hierarchyServices
				at: key
				put: (value asSet asSortedCollection: [ :x :y | x name < y name ]) asArray ].
	^ hierarchyServices
%

category: 'rsr'
method: RowanService
clearOrganizers
	self organizer: nil.
	updates do: [ :update | update organizer: nil ]
%

category: 'accessing'
method: RowanService
command

	^command
%

category: 'accessing'
method: RowanService
command: aSymbol

	command := aSymbol
%

category: 'accessing'
method: RowanService
commandArgs

	^commandArgs ifNil:[commandArgs := Array new]
%

category: 'accessing'
method: RowanService
commandArgs: anArray

	"for tests" 

	commandArgs := anArray
%

category: 'commands support'
method: RowanService
computePackageNameFor: theClass in: packageNames
	"Similar to Behavior>>rowanPackageNames but pass in a cached list of packageNames for performance"

	| loadedClass packageName |
	loadedClass := Rowan image
		loadedClassForClass: theClass thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	packageName := loadedClass loadedPackage name.
	^ (packageNames includes: packageName)
		ifTrue: [ packageName ]
		ifFalse: [ Rowan unpackagedName ]
%

category: 'symbol dictionaries'
method: RowanService
createDefaultSymbolDictionary

	^self createSymbolDictionaryNamed: self defaultSymbolDictionaryName
%

category: 'samples'
method: RowanService
createSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
	self createSymbolDictionaryNamed: self sampleSymbolDictionaryName
%

category: 'commands support'
method: RowanService
createServiceFor: aClass unlessExistingIn: newServices expand: toExpand packageNames: packageNames
	| service |
	service := newServices
		detect: [ :classService | classService name asString = aClass name asString ]
		ifNone: [ 
			RowanClassService new
				classServiceFromOop: aClass asOop
				packageNames: packageNames ].
	(toExpand includes: service theClass)
		ifTrue: [ service expand: true ]
		ifFalse: [ service expand: false ].

	(self organizer subclassesOf: aClass) size > 0
		ifTrue: [ service hasSubclasses: true ].

	^ service
%

category: 'symbol dictionaries'
method: RowanService
createSymbolDictionaryNamed: aName

	| dictionary size |
	dictionary := SymbolDictionary new.
	dictionary at: aName asSymbol put: dictionary.
	size := Rowan image symbolList size.
	System myUserProfile insertDictionary: dictionary at: size + 1.
	^ dictionary
%

category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionary
	"used?"

	^self symbolDictionaryNamed: self defaultSymbolDictionaryName
%

category: 'symbol dictionaries'
method: RowanService
defaultSymbolDictionaryName

	^'RowanProjects'
%

category: 'rowan'
method: RowanService
definitionClass

	^self subclassResponsibility
%

category: 'rowan'
method: RowanService
definitionClassName

	^self definitionClass name
%

category: 'replication'
method: RowanService
excludedInstVars

	^#( #organizer)
%

category: 'rsr'
method: RowanService
executeCommand
	"RSR -> RowanServices primary api."

	self checkForDeadProcesses.
	self setDebugActionBlock.
	[ 
	Rowan commandResultClass initializeResults.
	[ 
	updateType := nil.	"Update type is only for returned commands"
	command ifNil: [ ^ self ].
	self servicePerform: command withArguments: commandArgs ]
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ].
	updates := Rowan commandResultClass results.
	self postCommandExecution ]
		on: Exception
		do: [ :ex | 
			(ex isKindOf: Notification)
				ifFalse: [ 
					GsFile
						gciLogServer:
							DateTime now asStringMs , ' {'
								, Processor activeProcess identityHash printString
								, '}  - got exception: ' , ex printString ].
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
			ex pass ].
	^ self
%

category: 'commands support'
method: RowanService
extendHierarchies: hierarchies forClasses: theClasses
	"extend the hierarchies by one level
	of subclasses if it is a selected class"

	hierarchies
		do: [ :hierarchy | 
			| theClass |
			theClass := hierarchy last.
			(theClasses includes: theClass)
				ifTrue: [ self addAllSubclassHierarchiesOf: theClass to: hierarchies ] ].
	^ hierarchies
%

category: 'commands support'
method: RowanService
fileOut: ws on: path
	| file | 
	file := path asFileReference.
	file exists
		ifTrue: [ 
			(self confirm: 'File exists. File out anyway?')
				ifTrue: [ file delete ]
				ifFalse: [ ^ self ] ].
	file := file writeStreamDo: [ :stream | stream nextPutAll: ws contents ]
%

category: 'rsr'
method: RowanService
gsInteractionInformFailureHandler
	| promise |
	^ GsInteractionHandler new
		confirmBlock: [ :interaction | 
					self organizer: nil. 
					promise := remoteSelf jadeiteConfirm: interaction prompt.
					self organizer: ClassOrganizer new.  
					promise wait ];
		informBlock: [ :interaction | 
					self organizer: nil. 
					promise := remoteSelf jadeiteNotify: interaction message.
					self organizer: ClassOrganizer new.  
					promise wait  ];
		inspectBlock: [ :interaction | 
					self organizer: nil. 
					promise := remoteSelf jadeiteInspect: interaction theObject asOop.
					self organizer: ClassOrganizer new.  
					promise wait.
					interaction theObject]
%

category: 'perform'
method: RowanService
handleDeletedService
  self updateType: #'removed:'.
  RowanCommandResult addResult: self
%

category: 'commands support'
method: RowanService
hierarchiesByLevel: hierarchies

	"Return dictionary of classes by level. 
	Example: 
		hierarchies - #(#(Object Collection Array) #(Object AbstractException Exception))
	Return: 
		#(#nil->#(Object) Object->#(Collection AbstractException) Collection->#(Array) AbstractException->#(Exception)
	"
	| levels |
	levels := hierarchies inject: Dictionary new into:[:dict :chain | 
		1 to: chain size do: [:index | 
			| cls theSuper classSet |
			cls := chain at: index.
			classSet := dict at: cls ifAbsentPut: [Array new].
			index = 1 
		ifTrue:[
			classSet := dict at: #'nil' ifAbsentPut: [Array new]. 
			((dict at: #'nil') includes: cls) ifFalse:[(dict at: #'nil') add: cls].
		]
		ifFalse:[
				theSuper := chain at: index - 1.
				((dict at: theSuper) includes: cls) ifFalse:[(dict at: theSuper) add: cls]
				]].
			dict].
	^levels
%

category: 'initialization'
method: RowanService
initialize
	updates := Array new.
	wasUpdated := false
%

category: 'rsr'
method: RowanService
interactionHandlerActive
  ^ SessionTemps current at: #'rowanServiceInteractionActive' ifAbsent: [ true ]
%

category: 'testing'
method: RowanService
isClassService

	^false
%

category: 'testing'
method: RowanService
isDefinedProject

	^false
%

category: 'testing'
method: RowanService
isDictionaryService

	^false
%

category: 'testing'
method: RowanService
isMethodService

	^false
%

category: 'testing'
method: RowanService
isPackageService

	^false
%

category: 'testing'
method: RowanService
isProjectService

	^false
%

category: 'testing'
method: RowanService
isUpdating

	^command == #update
%

category: 'perform'
method: RowanService
isUpdatingButFoundToBeDeleted
  ^ self command == #'update' and: [ self wasDeleted ]
%

category: 'accessing'
method: RowanService
jadeiteServer

	^(Rowan jadeServerClassNamed: #JadeServer) theJadeiteServer
%

category: 'printing'
method: RowanService
logOn: aStream
	| instVarNames |
	super printOn: aStream.
	aStream lf.
	instVarNames := self class allInstVarNames.
	1 to: instVarNames size do: [ :index | 
		| instVarValue |
		instVarValue := self instVarAt: index.
		instVarValue
			ifNotNil: [ 
				aStream
					nextPutAll: (instVarNames at: index);
					nextPut: $=;
					nextPutAll: instVarValue printString;
					tab ] ].
	aStream
		lf
%

category: 'printing'
method: RowanService
logString
	| ws |
	ws := WriteStream on: String new.
	self logOn: ws.
	^ ws contents
%

category: 'rsr'
method: RowanService
massage: theValue inNonIndexableCollection: collection visitLog: visitLog
	theValue isSpecial
		ifTrue: [ ^ self ].
	theValue class = LargeInteger 
		ifTrue:[^self]. 
	(theValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ collection remove: theValue ].
	(theValue isKindOf: Association)
		ifTrue: [ 
			| replacement |
			replacement := Array with: theValue key with: theValue value.
			collection remove: theValue.
			collection add: replacement ].
	(theValue isKindOf: RowanService)
		ifTrue: [ ^ theValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	theValue class isVariable
		ifTrue: [ ^ self massageCollection: theValue visitLog: visitLog ]
%

category: 'rsr'
method: RowanService
massageCollection: collection visitLog: visitLog
	(visitLog includes: collection asOop)
		ifTrue: [ ^ self ].
	visitLog add: collection asOop.
	collection class isIndexable
		ifTrue: [ self massageIndexableCollection: collection visitLog: visitLog ]
		ifFalse: [ self massageNonIndexableCollection: collection visitLog: visitLog ]
%

category: 'rsr'
method: RowanService
massageIndexableCollection: collection visitLog: visitLog
	1 to: collection size do: [ :index | self massageObject: collection atIndex: index visitLog: visitLog ]
%

category: 'rsr'
method: RowanService
massageNonIndexableCollection: collection visitLog: visitLog
	collection
		ifNotNil: [  
			collection
				do: [ :theValue | self massage: theValue inNonIndexableCollection: collection visitLog: visitLog ] ]
%

category: 'rsr'
method: RowanService
massageObject: object atIndex: index visitLog: visitLog
	| theValue |
	(object isKindOf: Dictionary) ifTrue:[^self "for now"].
	theValue := object at: index.
	theValue isSpecial
		ifTrue: [ ^ self ].
	(theValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ object at: index put: nil ].
	(theValue isKindOf: Association)
		ifTrue: [ ^ object at: index put: (Array with: theValue key with: theValue value) ].
	(theValue isKindOf: RowanService)
		ifTrue: [ ^ theValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	theValue class = LargeInteger 
		ifTrue:[^self]. 
	theValue class isVariable
		ifTrue: [ ^self massageCollection: theValue visitLog: visitLog ]
%

category: 'rsr'
method: RowanService
massageObjectAtInstVarIndex: index visitLog: visitLog
	| instVarValue |
	instVarValue := self instVarAt: index.
	instVarValue isSpecial
		ifTrue: [ ^ self ].
	instVarValue class = LargeInteger 
		ifTrue:[^self]. 
	(instVarValue isKindOf: ClassOrganizer)
		ifTrue: [ ^ self instVarAt: index put: nil ].
	(instVarValue isKindOf: Association)
		ifTrue: [ 
			^ self
				instVarAt: index
				put: (Array with: instVarValue key with: instVarValue value) ].
	(instVarValue isKindOf: RowanService)
		ifTrue: [ ^ instVarValue massageServiceForRsrTransportWithVisitLog: visitLog ].
	instVarValue class isVariable
		ifTrue: [ self massageCollection: instVarValue visitLog: visitLog ]
%

category: 'rsr'
method: RowanService
massageServiceForRsrTransportWithVisitLog: visitLog
	"rsr won't handle associations"

	self isSpecial
		ifTrue: [ ^ self ].
	(visitLog includes: self asOop)
		ifTrue: [ ^ self ].
	visitLog add: self asOop.
	1 to: self class allInstVarNames size do: [ :index |
		self
			massageObjectAtInstVarIndex: index
			visitLog: visitLog ].
%

category: 'rsr'
method: RowanService
nilRsrVariables

	_id := nil. 
	_connection := nil. 
	remoteSelf := nil
%

category: 'accessing'
method: RowanService
organizer

	^organizer ifNil: [organizer := ClassOrganizer new]
%

category: 'accessing'
method: RowanService
organizer: anOrganizer

	organizer := anOrganizer.
%

category: 'commands support'
method: RowanService
peerHierarchies: theClasses
	" create hierarchies for each peer of the 
	classes of interest."

	| peerHierarchies |
	peerHierarchies := Array new.
	theClasses
		do: [ :theClass | 
			| superclass |
			superclass := theClass superclass.
			superclass
				ifNil: [ peerHierarchies add: (Array with: theClass) ]
				ifNotNil: [ 
					superclass subclasses
						do: [ :subclass | 
							peerHierarchies
								add:
									((self organizer allSuperclassesOf: subclass)
										add: subclass;
										yourself) ] ] ].
	^ peerHierarchies
%

category: 'other'
method: RowanService
postCommandExecution
	self postCommandExecutionWithoutAutoCommit.
	self autoCommitIfRequired.
%

category: 'other'
method: RowanService
postCommandExecutionWithoutAutoCommit
	| visitLog |
	self clearOrganizers.
	self removeDuplicateServices.
	visitLog := Set new.
	self massageServiceForRsrTransportWithVisitLog: visitLog.
%

category: 'rowan'
method: RowanService
projectTools

	^Rowan projectTools
%

category: 'release'
method: RowanService
releaseProcessOop: oop
  "not really releasing it. The client should have registered
	the process with the debugger window it opened before
	this is run"

  | jadeiteProcesses process |
  ((process := Object _objectForOop: oop) isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      jadeiteProcesses remove: process ifAbsent: [  ] ]
%

category: 'rsr'
method: RowanService
removeDuplicateServices
	updates copy ifNotNil: [:copy |
		copy do: [ :update | 
			update = self
				ifTrue: [ 
					self updateInternalService: update.
					updates remove: update ] ]]
%

category: 'samples'
method: RowanService
removeSampleSymbolDictionary

	self removeSymbolDictionaryNamed: self sampleSymbolDictionaryName.
%

category: 'symbol dictionaries'
method: RowanService
removeSymbolDictionaryNamed: aName

	| index |
	index := Rowan image symbolList names indexOf: aName asSymbol.
	index ~= 0 ifTrue:[
		System myUserProfile removeDictionaryAt: index]
%

category: 'rsr'
method: RowanService
resume: suspendedProcess orStep: debuggerResult
	debuggerResult = #'resume'
		ifTrue: [ 
			"open a new debugger if necessary"
			RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
			^ suspendedProcess resume ].
	debuggerResult = #'terminate'
		ifTrue: [ 
			RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
			suspendedProcess terminate.
			^ self ].
	suspendedProcess
		perform: debuggerResult first
		withArguments: (debuggerResult copyFrom: 2 to: debuggerResult size).
	debuggerResult first = #'trimStackToLevel:'
		ifTrue: [ 
			| result processService |
			processService := RowanProcessServiceServer
				existingProcessServiceFor: suspendedProcess.
			result := processService updateClient.
			^ self resume: suspendedProcess orStep: result ].
	(Delay forMilliseconds: 100) wait.
	suspendedProcess resume
%

category: 'other'
method: RowanService
rowanFixMe
		
	"marker for all things broken in Rowan"
%

category: 'accessing'
method: RowanService
rowanMethodHistory

	^UserGlobals at: #'RowanMethodHistory' ifAbsentPut: [Dictionary new]
%

category: 'accessing'
method: RowanService
rowanProjectName

	"all services should be able to return a project name
	even if they are not truly packaged" 

	^nil
%

category: 'samples'
method: RowanService
sampleSymbolDictionaryName

	^'SampleSymbolDictionaryName'
%

category: 'perform'
method: RowanService
servicePerform: symbol withArguments: collection
	"subclasses may not want to update after performing the command"

	self servicePerform: symbol withArguments: collection shouldUpdate: true
%

category: 'perform'
method: RowanService
servicePerform: commandSymbol withArguments: collection shouldUpdate: possiblyUpdate
	"each service updates itself after performing a command.
	Therefore, if the command is #update, don't run it here"

	shouldUpdate := possiblyUpdate.	"let the command decide if an update is actually needed"
	shouldUpdate ifNil: [^self]. 
	super perform: commandSymbol withArguments: collection.
	(self shouldUpdate and: [self organizer notNil])
		ifTrue: [ "a nil organizer implies that this process is in the process of terminating" self update ]
%

category: 'commands support'
method: RowanService
services: services from: levels expand: toExpand classes: theClasses
	"In order to avoid the expense of creating duplicate services, we cache
them in the newServices temporary for look up"

	| newServices allPackageNames |
	allPackageNames := Rowan image packageNames.
	newServices := Array new.
	theClasses
		do: [ :aClass | toExpand addAll: (self organizer allSuperclassesOf: aClass) ].
	levels
		keysAndValuesDo: [ :key :value | 
			| newKey |
			newKey := key = #'nil'
				ifTrue: [ #'nil' ]
				ifFalse: [ 
					self
						createServiceFor: key
						unlessExistingIn: newServices
						expand: toExpand
						packageNames: allPackageNames ].
			services
				at: newKey
				put:
					(value
						collect: [ :cls | 
							self
								createServiceFor: cls
								unlessExistingIn: newServices
								expand: toExpand
								packageNames: allPackageNames ]) ]
%

category: 'rsr'
method: RowanService
setDebugActionBlock
	Processor activeProcess
		breakpointLevel: 1;
		debugActionBlock: [ :ex | 
					| debuggerResult suspendedProcess |
					_connection isOpen
						ifFalse: [ 
							GsFile gciLogServer: ex printString.
							GsFile gciLogServer: 'connection not open'.	"can't return"
							Processor activeProcess terminate ].
					GsFile
						gciLogServer:
							DateTime now asStringMs , ' {'
								, Processor activeProcess identityHash printString , '} '
								, ex printString.
					suspendedProcess := Processor activeProcess.
					ex class = CompileError
						ifTrue: [ 
							| compileErrorService |
							self postCommandExecution.
							compileErrorService := RowanCompileErrorServiceServer new.
							compileErrorService gsArguments: ex errorDetails.
							updates := Array with: compileErrorService.
							^ ex return ].
					[ 
					RowanDebuggerService new saveProcessOop: suspendedProcess asOop.
					debuggerResult := (RowanProcessServiceServer
						existingProcessServiceFor: suspendedProcess)
						ifNil: [ 
							RowanProcessServiceServer
								openDebuggerOn: suspendedProcess
								exception: ex
								connection: _connection ]
						ifNotNil: [ :processService | processService updateClient ].
					ex isResumable
						ifFalse: [ debuggerResult := #'terminate' ].
					debuggerResult = #'terminate'
						ifTrue: [ 
							RowanProcessServiceServer removeProcessServiceFor: suspendedProcess.
							suspendedProcess resume	"let the exception handler block return nil" ]
						ifFalse: [ self resume: suspendedProcess orStep: debuggerResult ] ] fork.
					suspendedProcess ifNotNil: [ :proc | proc suspend ].
					debuggerResult = #'terminate'
						ifTrue: [ 
							self postCommandExecution.
							RowanBrowserService new unsetSecretBreakpoint. 
							ex tag: #'rsrProcessTerminated'.
							RsrUnhandledException signal: ex	"stop processing the exception but let rsr return" ].
					ex resume ]
%

category: 'accessing'
method: RowanService
shouldUpdate
	^shouldUpdate ifNil: [shouldUpdate := false].
%

category: 'replication'
method: RowanService
stonOn: stonWriter
    | instanceVariableNames |
    instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
    stonWriter writeObject: self
        streamMap: 
            [:dictionary |
            instanceVariableNames do: 
                    [:each |
                    (self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
                        ifNotNil: [:value | dictionary at: each asSymbol put: value]
                        ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
%

category: 'replication'
method: RowanService
stonStringFor: anObject

	"return a string representing a complete object structure
	suitable for replicating on the client."

	^STON toString: anObject
%

category: 'private'
method: RowanService
stripOutUnicode: string
  | asciiString |
  asciiString := string
    collect: [ :char | 
      ((self validLowRangeCharacters includes: char) not
        and: [ char asciiValue < 32 or: [ char asciiValue > 255 ] ])
        ifTrue: [ $? ]
        ifFalse: [ char ] ].
  ^ asciiString
%

category: 'commands support'
method: RowanService
superclassChainsFor: behaviors
	^ behaviors
		collect: [ :behavior | 
			| supers |
			supers := self organizer allSuperclassesOf: behavior.
			supers add: behavior.
			supers ]
%

category: 'symbol dictionaries'
method: RowanService
symbolDictionaryNamed: aName

	| symbolList  index |
	symbolList := Rowan image symbolList.
	index :=symbolList names indexOf: aName asSymbol.
	^index ~= 0
		ifTrue:[
			symbolList at: index]
		ifFalse:[
			self createSymbolDictionaryNamed: aName].
%

category: 'rsr'
method: RowanService
unregisteredCopy
	"copy of service without any reference to the rsr connection or organizer"

	| copy |
	copy := self copy. 
	copy nilRsrVariables.
	copy organizer: nil. 
	^copy
%

category: 'update'
method: RowanService
update
	wasUpdated := true
%

category: 'update'
method: RowanService
updateInternalService: updatedService

	"no internally held services to update"
%

category: 'update'
method: RowanService
updateLatest
  "subclasses may want to special behavior to update themselves
	to their loaded version"

  self update
%

category: 'other'
method: RowanService
updates: aCollection

	updates := aCollection
%

category: 'accessing'
method: RowanService
updateType: aSymbol

	updateType := aSymbol
%

category: 'accessing'
method: RowanService
userGlobals
	^ Rowan image symbolList objectNamed: #'UserGlobals'
%

category: 'private'
method: RowanService
validLowRangeCharacters
  ^ Array with: Character lf with: Character tab
%

category: 'testing'
method: RowanService
wasDeleted

	^false
%

category: 'fileout'
method: RowanService
writeFileOutHeaderOn: stream
	"This method will write a fileout header onto the given file.
	Adapted from GBS - GbxBrowser>>writeFileOutHeaderOn:"

	| rawVer beVer lf |
	stream
		nextPutAll: 'fileformat utf8';
		lf.
	rawVer := System _version.
	beVer := ''.
	lf := String with: Character lf.	"Comment each newline"
	(rawVer subStrings: (Array with: Character lf))
		do: [ :line | beVer := beVer , '! ' , line , lf ].
	stream
		nextPutAll: '!';
		lf;
		nextPutAll: '! From ';
		nextPutAll: beVer;
		lf;
		nextPutAll: '! On ';
		nextPutAll: Date today printString;
		nextPutAll: ', ';
		nextPutAll: Time now printString;
		lf;
		nextPutAll: '!';
		lf;
		flush
%

! Class implementation for 'RowanAnsweringService'

!		Class methods for 'RowanAnsweringService'

category: 'rsr'
classmethod: RowanAnsweringService
templateClassName

	^#RowanAnsweringService
%

!		Instance methods for 'RowanAnsweringService'

category: 'client command support'
method: RowanAnsweringService
addHaltToString: aString
	" temporary to make Debug It work for Jadeite in Pharo through RSR which 
	doesn't use the gci "

	| newString trimmedString | 
	trimmedString := aString trimSeparators. 
	trimmedString first = $|
		ifTrue: [ 
			| rs ws |
			rs := ReadStream on: trimmedString.
			ws := WriteStream on: String new.
			ws nextPut: rs next.
			ws nextPutAll: (rs upTo: $|).
			rs next.
			ws nextPutAll: ' | self halt. '.
			ws nextPutAll: rs upToEnd.
			newString := ws contents ]
		ifFalse: [ newString := 'self halt. ' , trimmedString ].
	^ newString
%

category: 'private'
method: RowanAnsweringService
addLowerCaseSymbolsIn: theClass To: array
	array addAll: theClass selectors.
	array addAll: theClass class selectors.
	array addAll: theClass instVarNames.
	array addAll: theClass class instVarNames.
	array addAll: theClass classVarNames
%

category: 'client commands'
method: RowanAnsweringService
allClassesStartingWith: string
  answer := SortedCollection new. 
  self organizer classes
    do: [ :cls | 
      (cls name beginsWith: string)
        ifTrue: [ answer add: cls name asString ] ].
  answer := answer asArray.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
allClassNames
  answer := SortedCollection new.
  answer addAll: (self organizer classes collect: [ :cls | cls name asString ]).
  answer := answer asArray.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
allPackageNames

	answer := Rowan image packageNames asSortedCollection asArray.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
allTestsIn: classServices
	answer := Array new. 
	classServices do:[:service | answer addAll: service allTests].
	RowanCommandResult addResult: self.
%

category: 'accessing'
method: RowanAnsweringService
answer

	^answer
%

category: 'Updating'
method: RowanAnsweringService
answer: anObject

	answer := anObject
%

category: 'client commands'
method: RowanAnsweringService
auditProjectNamed: projectName
	answer := (RowanProjectService new name: projectName) audit printString.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
autoCommit

	answer := RowanService autoCommit. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
autocompleteSymbols
  | newClassNames newLowerCaseSymbols |
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  self organizer classes
    do: [ :cls | 
      newClassNames add: cls name asString.
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  SessionTemps current
    at: #'autocompleteSymbolCache'
    put: (Array with: newClassNames with: newLowerCaseSymbols).
  answer := Array
    with: newClassNames asOrderedCollection
    with: newLowerCaseSymbols asOrderedCollection.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
%

category: 'client command support'
method: RowanAnsweringService
basicExec: aString context: oop
	^ self basicExec: aString context: oop shouldDebug: false
%

category: 'client command support'
method: RowanAnsweringService
basicExec: aString context: oop shouldDebug: shouldDebug
	| object symbolList tempMethod |
	object := Object _objectForOop: oop.
	symbolList := Rowan image symbolList.
	[ tempMethod := aString _compileInContext: object symbolList: symbolList ]
		on: CompileError
		do: [ :ex | 
			answer := false -> ex errorDetails.
			^ answer ].
	shouldDebug
		ifTrue: [ 
			tempMethod setBreakAtStepPoint: 1 breakpointLevel: 1.
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop ].
	[ answer := true -> (tempMethod _executeInContext: object) asOop ]
		ensure: [ 
			shouldDebug
				ifTrue: [ tempMethod clearBreakAtStepPoint: 1 ] ].
	RowanService autoCommit == true
		ifTrue: [ System commitTransaction ].
	^ answer
%

category: 'client command support'
method: RowanAnsweringService
basicExec: aString context: oop shouldDebug: shouldDebug returningString: returnStringBoolean
	| object symbolList tempMethod result return  |
	object := Object _objectForOop: oop.
	symbolList := Rowan image symbolList.
	[ tempMethod := aString _compileInContext: object symbolList: symbolList ]
		on: CompileError
		do: [ :ex | 
			answer := Array with: false with: ex errorDetails with: aString.
			^ answer ].
	shouldDebug
		ifTrue: [ 
			| theResult |
			theResult := tempMethod setBreakAtStepPoint: 1 breakpointLevel: 1. 
			RowanDebuggerService new saveProcessOop: GsProcess _current asOop ].
	[ 
	result := tempMethod _executeInContext: object.
	return := returnStringBoolean
		ifTrue: [ result printString ]
		ifFalse: [ result ].
	answer := Array with: true with: result asOop with: return ]
		ensure: [ 
			shouldDebug
				ifTrue: [ tempMethod clearBreakAtStepPoint: 1 ] ].
	RowanService autoCommit == true
		ifTrue: [ System commitTransaction ].
	^ answer
%

category: 'client command support'
method: RowanAnsweringService
basicExecReturningPrintString: aString context: oop shouldDebug: shouldDebug
	^ self
		basicExec: aString
		context: oop
		shouldDebug: shouldDebug
		returningString: true
%

category: 'client command support'
method: RowanAnsweringService
basicMethodHistoryFor: methodService
	answer := self rowanMethodHistory at: methodService ifAbsent: [ Array new ].
	answer isEmpty
		ifTrue: [ answer add: methodService ]
%

category: 'client command support'
method: RowanAnsweringService
basicPrintStringOf: oop toMaxSize: integer
  | object |
  object := Object _objectForOop: oop.
  ^ self basicPrintStringOfObject: object toMaxSize: integer
%

category: 'client command support'
method: RowanAnsweringService
basicPrintStringOfObject: object toMaxSize: integer
	"avoid the oop conversion when we already have the object"

	| printString |
	printString := [ object printString asUnicodeString ]
		on: Error
		do: [ :ex | 'Error printing object with oop - ' , object asOop printString, ': ', ex printString ].
	printString := printString size > integer
		ifTrue: [ (printString copyFrom: 1 to: integer) , '...' ]
		ifFalse: [ printString ].
	^ printString
%

category: 'client command support'
method: RowanAnsweringService
basicSortedSelectors
  | selectors |
  selectors := IdentitySet new.
  self organizer classes
    do: [ :aClass | 
      | metaClass |
      metaClass := aClass.
      2
        timesRepeat: [ 
          | methodDictionary |
          methodDictionary := metaClass _fullMethodDictEnv0.
          methodDictionary
            valuesDo: [ :method | 
              | selector |
              selector := method selector.
              selector charSize = 1
                ifTrue: [ selectors add: selector ].
              method _selectorPool
                do: [ :sentSelector | 
                  sentSelector charSize = 1
                    ifTrue: [ selectors add: sentSelector ] ] ].
          metaClass := metaClass class ] ].
  ^ selectors asSortedCollection asArray
%

category: 'client command support'
method: RowanAnsweringService
basicSortedSymbols
  | sortedSymbols |
  sortedSymbols := SortedCollection new.
  ((AllUsers userWithId: #'SymbolUser') resolveSymbol: #'AllSymbols') value
    keysDo: [ :symbol | 
      symbol charSize = 1
        ifTrue: [ sortedSymbols add: symbol ] ].
  ^sortedSymbols asArray
%

category: 'client commands'
method: RowanAnsweringService
breakPointsAreEnabled
  answer := RowanService breakPointsAreEnabled.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
canAccessServiceClasses
  "Being able to see RowanLoggingService is a good
	indication that all of the service classes are visible"

  answer := [ '[RowanLoggingService] value. true.' evaluate ]
    on: Error
    do: [ :ex | false ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
classHasSubclasses: oop
  | behavior |
  behavior := Object _objectForOop: oop.
  answer := behavior subclasses notEmpty.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
clearAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: self organizer;
    basicBreakpointMethods.
  GsNMethod clearAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
clearMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: self organizer;
        clearMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
debug: aString context: oop

	answer := self basicExec: aString context: oop shouldDebug: true. 
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
debug: aString context: oop inWindow: handle
  answer := self debug: aString context: oop.
  answer key
    ifTrue: [ RowanBrowserService new saveRootObject: answer value windowHandle: handle ].	"return answer for testing"
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
debug: aString inFrame: level ofProcess: processOop context: oop
	| symbolList frameContents symbolDictionary process tempMethod |
	symbolList := Rowan image symbolList.
	process := Object _objectForOop: processOop.
	process _isTerminated
		ifTrue: [ 
			RowanCommandResult addResult: self.
			^ false
				-> ('Process with oop ' , process asOop printString , ' was terminated.') ].
	frameContents := process _frameContentsAt: level.
	frameContents
		ifNotNil: [ 
			symbolDictionary := SymbolDictionary new
				name: ('DebuggerExecution' , processOop printString) asSymbol.
			1 to: (frameContents at: 9) size do: [ :index | 
				((frameContents at: 9) at: index) first = $.
					ifFalse: [ 
						symbolDictionary
							at: ((frameContents at: 9) at: index) asSymbol
							put: (frameContents at: 11 + index - 1) ] ].
			symbolList add: symbolDictionary before: symbolList first ].
	[ 
	[ tempMethod := aString _compileInContext: (Object _objectForOop: oop) symbolList: symbolList ]
		on: CompileError
		do: [ :ex | 
			answer := false -> ex errorDetails.
			^ answer ].
	tempMethod setBreakAtStepPoint: 1.
	[ answer := true -> (tempMethod _executeInContext: (Object _objectForOop: oop)) asOop ]
		ensure: [ tempMethod clearBreakAtStepPoint: 1 ].

	answer key
		ifTrue: [ 
			RowanService autoCommit == true
				ifTrue: [ System commitTransaction ] ].
	RowanCommandResult addResult: self ]
		ensure: [ 
			1 to: (frameContents at: 9) size do: [ :index | 
				| argsAndTemps |
				argsAndTemps := frameContents at: 9.
				(argsAndTemps at: index) first = $.
					ifFalse: [ 
						| variableService |
						process
							_frameAt: level
							tempAt: index
							put: (symbolDictionary at: (argsAndTemps at: index)).
						variableService := RowanVariableService
							oop: (symbolDictionary at: (argsAndTemps at: index)) asOop
							key: (argsAndTemps at: index)
							value: (symbolDictionary at: (argsAndTemps at: index)) printString
							className: (frameContents at: 8) class name asString.
						RowanCommandResult addResult: variableService ] ].
			symbolList remove: symbolDictionary ].	"return answer for testing"
	^ answer
%

category: 'client commands'
method: RowanAnsweringService
disableAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: self organizer;
    basicBreakpointMethods.
  GsNMethod _disableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
disableMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: self organizer;
        disableMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
enableAllBreaks
  | methodServices |
  methodServices := RowanQueryService new
    organizer: self organizer;
    basicBreakpointMethods.
  GsNMethod _enableAllBreaks.
  methodServices
    do: [ :methodService | RowanCommandResult addResult: methodService update ]
%

category: 'client commands'
method: RowanAnsweringService
enableMethodBreaks: methodServices
  methodServices
    do: [ :methodService | 
      methodService
        organizer: self organizer;
        enableMethodBreaks ]
%

category: 'client commands'
method: RowanAnsweringService
exec: aString

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
exec: aString context: oop

	answer := self basicExec: aString context: oop. 
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
exec: aString context: oop inWindow: handle
  answer := self exec: aString context: oop.
  answer key
    ifTrue: [ RowanBrowserService new saveRootObject: answer value windowHandle: handle ].	"return answer for testing"
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
exec: aString context: oop shouldDebug: shouldDebug

	answer := self basicExec: aString context: oop shouldDebug: shouldDebug returningString: false.
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
exec: aString inFrame: level ofProcess: processOop context: oop
	| symbolList frameContents symbolDictionary process | 
	symbolList := Rowan image symbolList.
	process := Object _objectForOop: processOop.
	process _isTerminated
		ifTrue: [ 
			RowanCommandResult addResult: self.
			^ false -> ('Process with oop ' , process asOop printString
				, ' was terminated.') ].
	frameContents := process _frameContentsAt: level.
	frameContents
		ifNotNil: [ 
			symbolDictionary := SymbolDictionary new name: ('DebuggerExecution', processOop printString) asSymbol. 
			1 to: (frameContents at: 9) size do: [ :index | 
				((frameContents at: 9) at: index) first = $.
					ifFalse: [ 
						symbolDictionary
							at: ((frameContents at: 9) at: index) asSymbol
							put: (frameContents at: 11 + index - 1) ] ].
			symbolList add: symbolDictionary before: symbolList first ].
	[ 
	answer := [ 
	true
		->
			(aString evaluateInContext: (Object _objectForOop: oop) symbolList: symbolList)
				asOop ]
		on: CompileError
		do: [ :ex | false -> ex errorDetails ].
	answer key
		ifTrue: [ 
			RowanService autoCommit == true
				ifTrue: [ System commitTransaction ] ].
	RowanCommandResult addResult: self ]
		ensure: [ 
			1 to: (frameContents at: 9) size do: [ :index | 
				| argsAndTemps |
				argsAndTemps := frameContents at: 9.
				(argsAndTemps at: index) first = $.
					ifFalse: [ 
						| variableService |
						process
							_frameAt: level
							tempAt: index
							put: (symbolDictionary at: (argsAndTemps at: index)).
						variableService := RowanVariableService
							oop: (symbolDictionary at: (argsAndTemps at: index)) asOop
							key: (argsAndTemps at: index)
							value: (symbolDictionary at: (argsAndTemps at: index)) printString
							className: (frameContents at: 8) class name asString.
						RowanCommandResult addResult: variableService ] ].
			symbolList remove: symbolDictionary ].	"return answer for testing"
	^ answer
%

category: 'client commands'
method: RowanAnsweringService
execReturningObject: aString

	"don't return complex objects - ston will likely break" 
	answer := aString evaluate. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
execReturningPrintString: aString context: oop

	answer := self execReturningPrintString: aString context: oop shouldDebug: false.
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
execReturningPrintString: aString context: oop shouldDebug: shouldDebug

	answer := self basicExecReturningPrintString: aString context: oop shouldDebug: shouldDebug.
	RowanCommandResult addResult: self.

	"return answer for testing" 
	^answer
%

category: 'client commands'
method: RowanAnsweringService
expressionSelector: string
  "try to uncover a selector in the string. 
	If it's not possible, return the string and 
	let the application do with it what it will"

  | messageNode |
  messageNode := [ RBParser parseExpression: string ]
    on: Error
    do: [ :ex | nil ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: string , ' #foo' ]
        on: Error
        do: [ :ex | nil ] ].
  messageNode
    ifNil: [ 
      messageNode := [ RBParser parseExpression: '#foo ' , string ]
        on: Error
        do: [ :ex | nil ] ].
  answer := messageNode
    ifNil: [ string ]
    ifNotNil: [ 
      messageNode isMessage
        ifTrue: [ messageNode buildSelector ]
        ifFalse: [ 
          messageNode isCascade
            ifTrue: [ messageNode messages first buildSelector]
            ifFalse: [ string ] ] ].
  RowanCommandResult addResult: self.
  ^ answer	"return answer for testing"
%

category: 'client commands'
method: RowanAnsweringService
fileContentsOf: filePath
	answer := filePath asFileReference contents.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
flipTranscript
	self isTranscriptInstalled ifTrue:[
		self jadeiteServer uninstallTranscript]
	ifFalse:[
		self jadeiteServer installTranscript]
%

category: 'extensions'
method: RowanAnsweringService
formatSource: aString
	[ answer := (RBParser parseMethod: aString) formattedCode ]
		on: Error
		do: [ :ex | 
			| ws |
			ws := WriteStream on: String new.
			ws
				nextPutAll: 'Could not format method ';
				cr;
				nextPutAll: ex printString.
			self inform: ws contents.
			^ self ].
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
initializeAutoCommit

	RowanService setAutoCommit: false
%

category: 'client commands'
method: RowanAnsweringService
initializeBreakPointsAreEnabled
  RowanService setBreakPointsAreEnabled: true
%

category: 'client commands'
method: RowanAnsweringService
interactionHandlerActive
  answer := SessionTemps current
    at: #'rowanServiceInteractionActive'
    ifAbsent: [ true ].
  RowanCommandResult addResult: self
%

category: 'testing'
method: RowanAnsweringService
isTranscriptInstalled
	^self transcriptObject == self jadeiteServer
%

category: 'client commands'
method: RowanAnsweringService
isVariable: oop
	| anObject |
	anObject := Object _objectForOop: oop. 
	answer := anObject class isVariable.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
loadedPackageExists: packageName
	
	| actualName |
	actualName := Rowan image packageNames detect:[:loadedName | loadedName asLowercase = packageName asLowercase] ifNone:[]. 
	answer := (Rowan image loadedPackageNamed: actualName ifAbsent: []) notNil. 
	command := nil. 
	commandArgs := nil. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
lowercaseSelectorsMatching: lowercaseSymbol
  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol asLowercase sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
methodHistoryFor: methodService
  self basicMethodHistoryFor: methodService.
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
methodReferenceCounts: methodServices
  answer := Array new.
  methodServices
    do: [ :methodService | answer add: (self organizer sendersOf: methodService selector) first size ].
  RowanCommandResult addResult: self.
  ^ answer	"for testing"
%

category: 'client commands'
method: RowanAnsweringService
needsCommit
	answer := System needsCommit.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
packageNamed: packageName 
	| service |
	service := RowanPackageService forPackageNamed: packageName.
	service update.
	answer := service.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
packageOrDictionaryFor: classService 
	| service |
	classService update. 
	service := classService packageName = Rowan unpackagedName
		ifTrue: [ RowanDictionaryService new name: classService dictionaryName ]
		ifFalse: [ RowanPackageService forPackageNamed: classService packageName ].
	service update.
	answer := service.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
packageServiceFor: classService 
	| packageService |
	classService update. 
	packageService := RowanPackageService forPackageNamed: classService packageName.
	packageService update.
	^packageService
%

category: 'client commands'
method: RowanAnsweringService
printStringOf: oop toMaxSize: integer
  answer := self basicPrintStringOf: oop toMaxSize: integer.
  RowanCommandResult addResult: self.
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
printStringOfOops: oops toMaxSize: integer
  answer := Array new.
  oops
    do: [ :oop | answer add: (self basicPrintStringOf: oop toMaxSize: integer) ].
  RowanCommandResult addResult: self.
  ^ answer
%

category: 'client commands'
method: RowanAnsweringService
profile: block
  "not used yet. Utility method needs testing.
	Make sure block execution time is long enough - say 1 second. 
	Results may not be consistent

	Usage example: 
| block answeringService | 
block := [| browserService profMonitor |
		browserService := (RowanBrowserService new) .
		10 timesRepeat:[browserService packagesWithTests]]. 
answeringService := RowanAnsweringService new profile: block.
answeringService answer. "

  | time ns |
  time := System millisecondsToRun: block.
  ns := ProfMonitor computeInterval: time / 1000.
  answer := ProfMonitor monitorBlock: block intervalNs: ns.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
projectAndPackageServiceFor: classService
	classService update.
	answer := classService updateType = #'removed:'
		ifTrue: [ nil ]
		ifFalse: [ 
			Array
				with: (self projectServiceFor: classService)
				with: (self packageServiceFor: classService) ].
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
projectServiceFor: classService 
	| projectService |
	classService update. 
	projectService := RowanProjectService new name: classService projectName.
	projectService update.
	^projectService
%

category: 'client commands'
method: RowanAnsweringService
projectsRepositoryRoots: projectNames
	answer := Dictionary new.
	projectNames
		do: [ :projectName | 
			answer
				at: projectName
				put: (RowanProjectService new name: projectName) repositoryRootPath ].
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
removeMethodHistoryFor: methodService
	self rowanMethodHistory removeKey: methodService ifAbsent: [  ]
%

category: 'client commands'
method: RowanAnsweringService
resolveAsService: name
	| projectService packageService classService dictionaryService |
	projectService := RowanProjectService new name: name.
	projectService update projectIsLoaded
		ifTrue: [ 
			answer := projectService.
			^ RowanCommandResult addResult: self ].
	packageService := RowanPackageService forPackageNamed: name.
	packageService update projectName
		ifNotNil: [ 
			answer := packageService.
			^ RowanCommandResult addResult: self ].
	dictionaryService := RowanDictionaryService new name: name.
	dictionaryService update classes notEmpty
		ifTrue: [ 
			answer := dictionaryService.
			^ RowanCommandResult addResult: self ].
	classService := RowanClassService new name: name.
	classService update projectName
		ifNotNil: [ 
			answer := classService.
			^ RowanCommandResult addResult: self ].
	answer := nil.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
runMethodTests: methodServices

	| behavior |
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior := methodService theClass. 
			behavior ifNil: [^self]. 
			behavior debug: methodService selector]].
	answer := true. 
	RowanCommandResult initializeResults. "squash any client updates during server test run"
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
selectorsMatching: lowercaseSymbol
  "assume we're passed a lower case symbol to avoid
	case sensitive misses"

  answer := self basicSortedSelectors
    select: [ :symbol | lowercaseSymbol sunitMatch: symbol asLowercase ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
selectorsMatchingPattern: pattern
  answer := self basicSortedSelectors.
  answer := answer select: [ :each | each _matchPatternNoCase: pattern ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
setAutoCommit: object

	answer := RowanService setAutoCommit: object.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAnsweringService
setBreakPointsAreEnabled: boolean
  boolean
    ifTrue: [ self enableAllBreaks ]
    ifFalse: [ self disableAllBreaks ].
  RowanService setBreakPointsAreEnabled: boolean.
  answer := boolean.
  updateType := #'breakpointSettingChanged:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
setEnableInteractionHandler: boolean
  SessionTemps current at: #'rowanServiceInteractionActive' put: boolean
%

category: 'client commands'
method: RowanAnsweringService
sortedSelectors
  answer := self basicSortedSelectors.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
sortedSymbols
  answer := self basicSortedSymbols. 
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
subclassCreationTemplate: className
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
%

category: 'client commands'
method: RowanAnsweringService
symbolDictionaryNames
	answer := (Rowan image symbolList
		collect: [ :symbolDictionary | symbolDictionary name asString ])
		asSortedCollection asOrderedCollection.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
symbolExists: aSymbol
  answer := (Rowan image symbolList resolveSymbol: aSymbol) isNil not.
  RowanCommandResult addResult: self
%

category: 'testing'
method: RowanAnsweringService
symbolListHasNil
	"Debugging code - Sending after each test in #tearDown method from client.
	Halt if symbol list has a nil." 
	Rowan image symbolList detect:[:symDict | symDict isNil] ifNone:[^false].
	self halt.
%

category: 'client commands'
method: RowanAnsweringService
symbolsMatching: string
  answer := self basicSortedSymbols
    select: [ :symbol | string sunitMatch: symbol ].
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
transcriptInstalled

	answer := self isTranscriptInstalled.
	RowanCommandResult addResult: self.
%

category: 'private'
method: RowanAnsweringService
transcriptObject
	^ SessionTemps current at: #'TranscriptStream_SessionStream'
%

category: 'client commands'
method: RowanAnsweringService
turnOffTranscriptWrites

	self isTranscriptInstalled ifTrue:[
		self flipTranscript]
%

category: 'client commands'
method: RowanAnsweringService
updateAutocompleteSymbols
  | cache newClassNames newLowerCaseSymbols |
  cache := SessionTemps current
    at: #'autocompleteSymbolCache'
    ifAbsent: [ ^ self	"autocomplete not activated" ].
  newClassNames := Array new.
  newLowerCaseSymbols := Array new.
  self organizer classes
    do: [ :cls | 
      (cache first includes: cls name asString)
        ifFalse: [ newClassNames add: cls name asString ].
      self addLowerCaseSymbolsIn: cls To: newLowerCaseSymbols ].
  (SessionTemps current at: #'autocompleteSymbolCache') first
    addAll: newClassNames.
  newLowerCaseSymbols := newLowerCaseSymbols asSet asArray.
  cache last
    do: [ :selector | 
      (newLowerCaseSymbols includes: selector)
        ifTrue: [ newLowerCaseSymbols remove: selector ] ].
  (SessionTemps current at: #'autocompleteSymbolCache') last
    addAll: newLowerCaseSymbols.
  answer := Array with: newClassNames with: newLowerCaseSymbols.
  updateType := #'updateSymbols:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanAnsweringService
updateServices: services

	services do: [:service | service update]
%

! Class implementation for 'RowanFileService'

!		Class methods for 'RowanFileService'

category: 'as yet unclassified'
classmethod: RowanFileService
templateClassName

	^#RowanFileService
%

!		Instance methods for 'RowanFileService'

category: 'command support'
method: RowanFileService
basicIsDirectory: thePath
	| fileReference |
	fileReference := FileReference fileSystem: FileSystem disk path: thePath asPath.
	answer := fileReference isDirectory.
	RowanCommandResult addResult: self
%

category: 'private'
method: RowanFileService
behaviorFromMethodService: methodService 

	| behavior |
	behavior := Rowan globalNamed: methodService className.
	methodService meta == true ifTrue:[behavior := behavior class].
	^behavior
%

category: 'client commands'
method: RowanFileService
deleteFileWithUnicode
	" for tests"

	| fileReference |
	fileReference := FileReference
		fileSystem: FileSystem disk
		path: FileSystem workingDirectory pathString , '/testJadeiteUnicodeFile.txt'.
	fileReference delete
%

category: 'client commands'
method: RowanFileService
directoryContents
	answer := ((GsFile isServerDirectory: path) ifNil: [ false ])
		ifTrue: [ 
			(GsFile contentsOfDirectory: path onClient: false)
				collect: [ :subpath | subpath -> ((GsFile isServerDirectory: subpath) ifNil: [ false ]) ] ]
		ifFalse: [ nil ].
	answer ifNil: [ ^ self ].
	answer := (answer asSortedCollection: [ :x :y | x key < y key ]) asArray.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanFileService
directoryPath
	| fileReference |
	fileReference := FileReference fileSystem: FileSystem disk path: path asPath.
	answer := fileReference isDirectory
		ifTrue: [ fileReference pathString ]
		ifFalse: [ fileReference parent pathString ].
	RowanCommandResult addResult: self.
	^ answer	"return for testing"
%

category: 'client commands'
method: RowanFileService
diveInto: directory
	path := (Path from: path) resolveString: directory.
	self directoryContents
%

category: 'client commands'
method: RowanFileService
expandPath
	answer := path asPath fullName.
	RowanCommandResult addResult: self.
	^ answer	"return for testing"
%

category: 'client commands'
method: RowanFileService
fileContents
	| fileReference |
	fileReference := FileReference fileSystem: FileSystem disk path: path asPath.
	answer := fileReference exists
		ifTrue: [ fileReference readStream contents ]
		ifFalse: [ String new asUnicodeString ].
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanFileService
fileIn
	^ self fileIn: path
%

category: 'client commands'
method: RowanFileService
fileIn: filePath
	"don't halt on compile warnings"

	| fileReference |
	fileReference := filePath asFileReference.
	fileReference exists
		ifFalse: [ ^ self inform: 'File does not exist' ].
	[ GsFileIn fromServerPath: filePath ]
		on: CompileWarning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description;
				flush.
			ex resume ].
	answer := fileReference readStream contents.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanFileService
fileInChunk: aString
	"don't halt on compile warnings"

	| fileIn |
	fileIn := GsFileIn newFromStream: (ReadStream on: aString).
	[ fileIn doFileIn ]
		on: CompileWarning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description;
				flush.
			ex resume ]
%

category: 'client commands'
method: RowanFileService
fileName
	answer := (Path from: path) basename. 
	RowanCommandResult addResult: self.
	^answer "return for testing"
%

category: 'client commands'
method: RowanFileService
fileoutDictionaries: dictionaryNames
	| ws |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	dictionaryNames
		do: [ :dictionaryName | 
			self organizer
				fileOutClassesAndMethodsInDictionary: (Rowan globalNamed: dictionaryName)
				on: ws ].
	self fileOut: ws on: path
%

category: 'client commands'
method: RowanFileService
fileoutMethods: array
	| ws |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	array
		do: [ :service | 
			ws
				nextPutAll:
					((self behaviorFromMethodService: service) fileOutMethod: service selector) ].
	self fileOut: ws on: path
%

category: 'client commands'
method: RowanFileService
isDirectory
	self basicIsDirectory: path
%

category: 'client commands'
method: RowanFileService
isDirectory: directory
	self basicIsDirectory: directory
%

category: 'client commands'
method: RowanFileService
parentDirectoryPath
	answer := (Path from: path) parent pathString. 
	RowanCommandResult addResult: self.
	^answer "return for testing"
%

category: 'accessing'
method: RowanFileService
path
	^path
%

category: 'accessing'
method: RowanFileService
path: object
	path := object
%

category: 'client commands'
method: RowanFileService
pop
	path := (Path from: path) parent pathString. 
	self directoryContents
%

category: 'client commands'
method: RowanFileService
readmeContents
	answer := (GsFile existsOnServer: path)
		ifTrue: [ GsFile getContentsOfServerFile: path ]
		ifFalse: [ String new ].
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanFileService
remove
	GsFile removeServerFile: path
%

category: 'client commands'
method: RowanFileService
write: contents
	"protocol: client commands"

	| thePath fileReference |
	thePath := path last = $/
		ifTrue: [ path copyFrom: 1 to: path size - 1 ] 
		ifFalse: [ path ].
	fileReference := FileReference fileSystem: FileSystem disk path: thePath asPath.
	fileReference
		writeStreamDo: [ :writeStream | 
			writeStream
				truncate;
				nextPutAll: contents asUnicodeString;
				flush ]
%

category: 'client commands'
method: RowanFileService
writeFileWithUnicode
	" for tests" 
| unicode fileReference writeStream |
	unicode := 'This is some unicode text
  
ý š

Another extended char - Ü  aaa œ'.

	fileReference := FileReference fileSystem: FileSystem disk path:  FileSystem workingDirectory pathString, '/testJadeiteUnicodeFile.txt'.
	writeStream := fileReference writeStream.
	[ 
	writeStream
		nextPutAll: unicode asUnicodeString;
		flush ]
		ensure: [ writeStream close ].
%

! Class implementation for 'RowanAutoCommitService'

!		Class methods for 'RowanAutoCommitService'

category: 'rsr'
classmethod: RowanAutoCommitService
templateClassName

	^#RowanAutoCommitService
%

!		Instance methods for 'RowanAutoCommitService'

category: 'client commands'
method: RowanAutoCommitService
autoCommit: boolean

	self class setAutoCommit: boolean.
	autoCommit := self class autoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanAutoCommitService
flipAutoCommit

	autoCommit := self class flipAutoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
%

! Class implementation for 'RowanBrowserService'

!		Class methods for 'RowanBrowserService'

category: 'rsr'
classmethod: RowanBrowserService
templateClassName

	^#RowanBrowserService
%

!		Instance methods for 'RowanBrowserService'

category: 'client commands'
method: RowanBrowserService
abortTransaction
  | autoCommitService autoCommitState |
  autoCommitState := RowanService autoCommit == #'failed'
    ifTrue: [ true ]
    ifFalse: [ RowanService autoCommit ].
  System abortTransaction.
  autoCommitService := RowanAutoCommitService new.
  autoCommitService autoCommit: autoCommitState.
  self updateProjects.
  self updateDictionaries
%

category: 'client commands'
method: RowanBrowserService
abortTransactionAndUpdateServices: services
	self abortTransaction.
	services
		do: [ :service | 
			service
				organizer: self organizer;
				updateLatest ]
%

category: 'client commands'
method: RowanBrowserService
allClasses
	allClasses := self basicAllClasses.
	updateType := #dontUpdate. 
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
allPackages
	| theProjects |
	theProjects := Rowan image loadedProjects asArray.
	projects := theProjects
		collect: [ :project | 
			| projectService |
			projectService := RowanProjectService newNamed: project name.
			projectService packages: projectService packageServices.
			projectService packages
				do: [ :packageService | packageService projectName: project name ].
			projectService ].
	updateType := #'dontUpdate'.
	RowanCommandResult addResult: self
%

category: 'client commands support'
method: RowanBrowserService
basicAllClasses
	| theClasses packageNames |
	theClasses := SortedCollection sortBlock: [ :x :y | x name < y name ].
	packageNames := Rowan image packageNames.
	theClasses
		addAll:
			(self organizer classes
				collect: [ :theClass | 
					| service |
					service := RowanClassService new name: theClass name.
					service
						packageName: (self computePackageNameFor: theClass in: packageNames).
					service projectName: theClass rowanProjectName.
					service ]).
	^ theClasses asArray
%

category: 'client commands'
method: RowanBrowserService
checkForAddedProjects: projectServices
	| loadedProjects addedProjects loadedProjectServices |
	loadedProjects := Rowan image loadedProjects.
	addedProjects := Array new.
	loadedProjectServices := loadedProjects
		collect: [ :project | RowanProjectService newNamed: project name ].
	loadedProjectServices
		do: [ :loadedProjectService | 
			(projectServices includes: loadedProjectService)
				ifFalse: [ addedProjects add: loadedProjectService ] ].
	addedProjects
		do: [ :projectService | RowanCommandResult addResult: projectService update ]
%

category: 'client commands'
method: RowanBrowserService
classHierarchy
	| theClasses |
	theClasses := allClasses collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
%

category: 'private'
method: RowanBrowserService
classHierarchy: theClasses
  hierarchyServices := super classHierarchy: theClasses.
  ^ hierarchyServices
%

category: 'client commands'
method: RowanBrowserService
classHierarchyForDictionariesNamed: dictionaryNames
  | theClasses |
  theClasses := OrderedCollection new.
  dictionaryNames
    do: [ :dictionaryName | 
      (Rowan globalNamed: dictionaryName)
        keysAndValuesDo: [ :key :value | 
          value isClass
            ifTrue: [ theClasses add: value ] ] ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
classHierarchyForPackagesNamed: packageNames
  | theClasses |
  theClasses := OrderedCollection new.
  packageNames
    do: [ :packageName | 
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClasses 
            collect: [ :cls | cls handle]).
      theClasses
        addAll:
          ((Rowan image loadedPackageNamed: packageName) loadedClassExtensions
            collect: [ :cls | cls handle]) ].
  hierarchyServices := self classHierarchy: theClasses.
  updateType := #'classHierarchyUpdate:browser:'.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
clearWindowRegistry
	SessionTemps current at: #'rowanServicesWindowRegistry' put: Dictionary new
%

category: 'client commands'
method: RowanBrowserService
commitTransaction
	"makes sure our permanent method history does not have any rsr inst var values as Semaphores can prevent commits"

	| answeringService |
	self rowanMethodHistory
		keysAndValuesDo: [ :key :value | 
			key nilRsrVariables.
			value do: [ :svc | svc nilRsrVariables ] ].
	answeringService := RowanAnsweringService new answer: System commitTransaction.
	RowanCommandResult addResult: answeringService.
	^answeringService answer
%

category: 'client commands'
method: RowanBrowserService
compileClass: definitionString
  | anonymousMethod |
  self confirmDuplicateName: definitionString.
  anonymousMethod := definitionString
    _compileInContext: nil
    symbolList: Rowan image symbolList.
  SessionTemps current at: #'jadeiteCompileClassMethod' put: anonymousMethod
%

category: 'client commands support'
method: RowanBrowserService
confirmDuplicateName: definitionString
  | className |
  className := (definitionString subStrings at: 3) copyWithout: $'.
  (Rowan image symbolList resolveSymbol: className asSymbol)
    ifNotNil: [ 
      className = selectedClass
        ifFalse: [ 
          (self confirm: 'Class name is already an object. Continue?')
            ifFalse: [ ^ Error signal: 'Class not compiled. Name already exists.' ] ] ]
%

category: 'client commands'
method: RowanBrowserService
defaultClassHierarchy
	hierarchyServices := Dictionary new.   
	self organizer hierarchy keysAndValuesDo: [:key :value |
		| classService |
		classService := key == #nil ifTrue:[#nil] ifFalse: [RowanClassService basicForClassNamed: key name].
		hierarchyServices at: classService put: (value collect:[:cls | RowanClassService basicForClassNamed: cls name]) asArray.
	].
	updateType := #classHierarchyUpdate:browser:. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanBrowserService
dictionariesWithTests
	"for now, just return the dictionaries" 
	self organizer: ClassOrganizer new.	"when we call this method, our world has changed from a reload, etc."
	dictionaries := Rowan image symbolList names collect:[:name | RowanDictionaryService new name: name].
	updateType := #'testDictionaries:'.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
findRemovedServices: services

	services do:[:service | 
		service wasDeleted ifTrue:[
				service updateType: #removed:.
				RowanCommandResult addResult: service.
		]
	].
%

category: 'initialize'
method: RowanBrowserService
initialize
  super initialize.
  newCachedSelectors := Array new.
  newCachedClasses := Array new
%

category: 'accessing'
method: RowanBrowserService
newCachedClasses
	newCachedClasses ifNil: [newCachedClasses := Array new]. 
	^newCachedClasses
%

category: 'accessing'
method: RowanBrowserService
newCachedClasses: object
	newCachedClasses := object
%

category: 'accessing'
method: RowanBrowserService
newCachedSelectors
	^newCachedSelectors
%

category: 'accessing'
method: RowanBrowserService
newCachedSelectors: object
	newCachedSelectors := object
%

category: 'client commands'
method: RowanBrowserService
newProjectNamed: projectName windowHandle: handle
	| definedProjectService project |
	definedProjectService := RowanDefinedProjectService new name: projectName.
	definedProjectService projectOop: (project := Rowan newProjectNamed: projectName) asOop.
	self saveRootObject: definedProjectService projectOop windowHandle: handle. 
	definedProjectService := definedProjectService updateType: #newProject:.
	definedProjectService specService: (RowanLoadSpecService new initialize: project loadSpecification asOop).
	RowanCommandResult addResult: definedProjectService
%

category: 'window registry'
method: RowanBrowserService
openWindows

	"for testing"

	^SessionTemps current at: #rowanServicesWindowRegistry ifAbsent:[]
%

category: 'client commands'
method: RowanBrowserService
packagesWithTests
	| packageNames |
	self organizer: ClassOrganizer new.	"when we call this method, our world has changed from a reload, etc."
	testPackages := Set new.
	testCount := 0.
	packageNames := Rowan image packageNames.  
	(self organizer allSubclassesOf: TestCase)
		do: [ :sub | 
			| packageName testMethodCount |
			testMethodCount := (sub sunitSelectors
				select: [ :each | each beginsWith: 'test' ]) size.	"sending #testSelectors was slower"
			testCount := testCount + testMethodCount.
			testMethodCount > 0
				ifTrue: [ 
					packageName := self computePackageNameFor: sub in: packageNames.
					packageName = Rowan unpackagedName
						ifFalse: [ 
							testPackages
								add:
									((RowanPackageService forPackageNamed: packageName)
										updateProjectName;
										yourself) ].
					(Rowan image loadedClassExtensionsForClass: sub)
						do: [ :loadedThing | 
							testPackages
								add:
									((RowanPackageService forPackageNamed: loadedThing loadedPackage name)
										updateProjectName;
										yourself)	"don't update the entire package for performance improvement" ] ] ].
	updateType := #'testPackages:'.
	testPackages := testPackages asArray.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
recompileMethodsAfterClassCompilation
	"compileClass: must be run first"

	| theClass classService packageService projectService |
	theClass := [ 
	[ (SessionTemps current at: #'jadeiteCompileClassMethod') _executeInContext: nil ]
		on: CompileWarning , CompileError
		do: [ :ex | 
			(ex isKindOf: CompileError)
				ifTrue: [ 
					| compileErrorService |
					self postCommandExecutionWithoutAutoCommit.
					compileErrorService := RowanCompileErrorServiceServer new.
					compileErrorService gsArguments: ex errorDetails.
					updates := Array with: compileErrorService.
					^ RowanCommandResult addResult: compileErrorService ]
				ifFalse: [ ex resume ] ] ]
		ensure: [ SessionTemps current at: #'jadeiteCompileClassMethod' put: nil ].
	classService := RowanClassService new name: theClass name.
	classService update.
	classService updateSubclasses.
	classService isNewClass: true.	"if nothing else, the dirty state of the package/project services
	should be updated. Would like a less heavy weight solution than this, though."
	packageService := RowanPackageService forPackageNamed: classService packageName.
	packageService update.
	packageService
		testClasses;
		updateType: nil.
	projectService := RowanProjectService newNamed: packageService projectName.
	projectService update.
	packageService selectedClass: classService.
	RowanCommandResult addResult: classService.
	selectedClass := classService.
	classService updateType: #'newClass:browser:'.
	self updateSymbols: (Array with: theClass name asString).
	self classHierarchy. 
	RowanCommandResult addResult: self.
	^classService
%

category: 'client commands'
method: RowanBrowserService
releaseWindowHandle: integer
	| registry object |
	registry := SessionTemps current
		at: #'rowanServicesWindowRegistry'
		ifAbsent: [ ^ self ].
	object := registry at: integer ifAbsent: [ ^ self ].
	Rowan loggingServiceClass current
		logComment:
			'Release object with oop: ' , object asOop printString , ' window handle: '
				, integer printString.
	registry removeKey: integer ifAbsent: [  ]
%

category: 'client commands'
method: RowanBrowserService
reloadProjects: projectServices andUpdateServices: services
	| projectNames answeringService |
	services do: [ :service | service organizer: self organizer ].
	projectServices do: [ :service | service organizer: self organizer ].
	projectServices do: [ :projectService | projectService reloadProject ].
	projectNames := projectServices
		collect: [ :projectService | projectService name ].
	services
		do: [ :service | 
			(projectNames includes: service rowanProjectName)
				ifTrue: [ service updateLatest ] ].
	answeringService := RowanAnsweringService new organizer: self organizer.
	answeringService updateAutocompleteSymbols.
%

category: 'client commands'
method: RowanBrowserService
removeDictionariesNamed: dictionaryNames
	"remove from both transient & persistent symbol lists" 

	dictionaryNames do:[:dictionaryName | 
		| dictionaryNameSymbol |
		dictionaryNameSymbol := dictionaryName asSymbol.
		(Rowan image symbolList names includes: dictionaryNameSymbol) ifTrue:[
			Rowan image symbolList removeDictionaryNamed: dictionaryNameSymbol].
		(System myUserProfile symbolList names includes: dictionaryNameSymbol) ifTrue:[
			System myUserProfile symbolList removeDictionaryNamed: dictionaryNameSymbol]].
	self updateDictionaries.
%

category: 'client commands'
method: RowanBrowserService
removeMethods: methodServices

	| notRemoved |
	notRemoved := Array new. 
	removedMethods := Array new. 
	methodServices do: [:methodService |
		| classService |
		classService := RowanClassService forClassNamed: methodService className. 
		classService meta: methodService meta. 
		classService removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		classService updatePackageAndProject.
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService]. 
		removedMethods add: methodService].
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:ea | ea selector]) printString].
	updateType := #methodsRemoved:. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanBrowserService
saveRootObject: oop windowHandle: integer
	" a window has been opened on the client. Save the 
	root object of the window so it won't be recycled"

	| registry object |
	registry := SessionTemps current
		at: #'rowanServicesWindowRegistry' 
		ifAbsentPut: [ Dictionary new ].
	registry at: integer ifAbsentPut: [ Array new ].
	object := Object _objectForOop: oop.
	(object isKindOf: GsProcess)
		ifTrue: [ RowanDebuggerService new saveProcessOop: object asOop	"make sure the process oop gets saved beyond the life of a debugger or notifier" ].
	((registry at: integer) includes: object)
		ifFalse: [ 
			object := registry at: integer ifAbsent: [ ^ self ].
			Rowan loggingServiceClass current
				logComment:
					'Saving object with oop: ' , object asOop printString , ' window handle: '
						, integer printString.
			(registry at: integer) add: (Object _objectForOop: oop) ]
%

category: 'accessing'
method: RowanBrowserService
selectedClass

	^selectedClass
%

category: 'accessing'
method: RowanBrowserService
selectedClass: object

	selectedClass := object
%

category: 'client commands'
method: RowanBrowserService
turnOffNativeCode
	"setting a breakpoint anywhere in the system will turn off native code"

	| methodService |
	methodService := RowanMethodService new
		selector: #'methodForTurningOffNativeCode';
		meta: false;
		className: 'RowanBrowserServiceServer'.
	methodService setBreakAt: 1.
%

category: 'client commands'
method: RowanBrowserService
unloadProjectsNamed: projectNames
	projectNames do: [ :projectName | (RwProject newNamed: projectName) unload ].
	self updateProjects
%

category: 'client commands'
method: RowanBrowserService
unsetSecretBreakpoint
	"used for turning off native code"

	| methodService |
	methodService := RowanMethodService new
		selector: #'methodForTurningOffNativeCode';
		meta: false;
		className: 'RowanBrowserServiceServer'.
	methodService clearBreakAt: 1.
%

category: 'client commands'
method: RowanBrowserService
updateDictionaries
	dictionaries := Rowan image symbolList names
		collect: [ :name | 
			RowanDictionaryService new
				name: name asString;
				update ].
	dictionaries := dictionaries asOrderedCollection.
	updateType ifNil: [ updateType := OrderedCollection new ].
	updateType add: #'dictionaryListUpdate:'.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanBrowserService
updateProjects
  | sortedProjects |
  self packagesWithTests. "make sure tests are always updated" 
  sortedProjects := SortedCollection sortBlock: [ :a :b | a name < b name ].
  sortedProjects addAll: Rowan image loadedProjects.
  projects := sortedProjects
    collect: [ :project | RowanProjectService newNamed: project name ].
  updateType := Array with: updateType with: #'projectsUpdate:browser:'. "temporary hack" 
  RowanCommandResult addResult: self
%

category: 'update'
method: RowanBrowserService
updateSymbols: classNames
  self newCachedClasses addAll: classNames.
  updateType := #'addCachedSymbols:'
%

! Class implementation for 'RowanBrowserServiceServer'

!		Instance methods for 'RowanBrowserServiceServer'

category: 'private'
method: RowanBrowserServiceServer
methodForTurningOffNativeCode

	"set a breakpoint here to turn off native code so anonymous breakpoints will be honored" 
	
	^'do not remove this method'
%

! Class implementation for 'RowanClassService'

!		Class methods for 'RowanClassService'

category: 'instance creation'
classmethod: RowanClassService
basicForClassNamed: className 
	"Don't get method services. Efficient for classes with many methods"
	^self new basicForClassNamed: className
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className 

	^self new forClassNamed: className
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className meta: aBoolean

	| inst |
	inst := self forClassNamed: className subStrings first.
	inst meta: aBoolean.
	^inst
%

category: 'instance creation'
classmethod: RowanClassService
forClassNamed: className package: packageName

	| inst |
	inst := self forClassNamed: className.
	inst packageName: packageName.
	^inst
%

category: 'instance creation'
classmethod: RowanClassService
minimalForClassNamed: className 
	"Don't get method services. Efficient for classes with many methods"
	^self new minimalForClassNamed: className
%

category: 'instance creation'
classmethod: RowanClassService
minimalForClassNamed: className packageNames: packageNames
	"Don't get method services. Efficient for classes with many methods"
	^self new minimalForClassNamed: className packageNames: packageNames
%

category: 'rsr'
classmethod: RowanClassService
templateClassName

	^#RowanClassService
%

!		Instance methods for 'RowanClassService'

category: 'comparing'
method: RowanClassService
= classService
	(classService class canUnderstand: #isClassService) ifFalse:[^false].
	^classService isClassService
			ifTrue: [  name asString = classService name asString and: [meta = classService meta]]
			ifFalse: [^false]
%

category: 'client commands'
method: RowanClassService
addCategory: string
	| theClass |
	theClass := self theClass.
	meta
		ifTrue: [ theClass := theClass class ].
	theClass addCategory: string.
	self updateClass.
%

category: 'constants'
method: RowanClassService
addSubclassWarningString

	^'Superclass is not packaged. Enter the desired package name'
%

category: 'client commands'
method: RowanClassService
allSubclassServices
	| subclassServices |
	RowanCommandResult addResult: self.
	self updateType: #'updatedFullInClassHierarchy:browser:'.
	subclassServices := self theClass subclasses asArray
		collect: [ :aClass | RowanClassService minimalForClassNamed: aClass name ].
	subclassServices isEmpty ifFalse: [hasSubclasses := true]. 
	hierarchyServices := Dictionary new.
	hierarchyServices at: #'expand' put: subclassServices.
	subclassServices do: [ :classService | classService allSubclassServices ]
%

category: 'Accessing'
method: RowanClassService
allTests
	| allSelectors theClass |
	self isTestCase
		ifFalse: [ ^ Array new ].
	theClass := self theClass thisClass.
	theClass isAbstract
		ifTrue: [ ^ Array new ].
	allSelectors := self theClass thisClass allTestSelectors.
	^ allSelectors
		collect: [ :selector | 
			RowanMethodService
				forSelector: selector
				class: (theClass whichClassIncludesSelector: selector asString)
				meta: false
				organizer: self organizer ]
%

category: 'testing'
method: RowanClassService
arePackageAndProjectClean

	^self packageIsDirty not and:[self projectIsDirty not]
%

category: 'initialization'
method: RowanClassService
basicForClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self].
	self basicRefreshFrom: theClass.
%

category: 'Accessing'
method: RowanClassService
basicRefreshFrom: theClass
	| classOrMeta theFilters |
	oop := theClass asOop.
	command := nil. 
	commandArgs := nil. 
	superclassName := theClass superClass ifNotNil:[:theSuper | theSuper name asString]. 
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	self setComment.
	template := self classCreationTemplate.
	theFilters := SortedCollection new.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	self setIsTestCase.
	self updateIsExtension.
	hasSubclasses := (self organizer subclassesOf: theClass) notEmpty.
	classCategory := classOrMeta category.
%

category: 'Accessing'
method: RowanClassService
behavior

	| behavior |
	behavior := self theClass. 
	meta == true ifTrue:[behavior := behavior class].
	^behavior
%

category: 'accessing'
method: RowanClassService
classCategory
	^classCategory
%

category: 'accessing'
method: RowanClassService
classCategory: object
	classCategory := object
%

category: 'client commands'
method: RowanClassService
classComment: string
	| theClass |
	theClass := self theClass. 
	theClass rwComment: string.
%

category: 'rowan'
method: RowanClassService
classCreationTemplate
	
	^self browserTool classCreationTemplateForClass: self theClass hybridBrowser: true.
%

category: 'rowan'
method: RowanClassService
classCreationTemplateUsing: packageNames
	"copying RwPrjBrowserToolV2>>classCreationTemplateForClass:hybridBrowser: with one change for performance"

	| result anArray lfsp newByteSubclass civs superClass className thePackageName nonRowanClass |
	result := String new.
	superClass := self theClass superclass.
	className := self theClass name asString.
	superClass
		ifNil: [ result addAll: 'nil' ]
		ifNotNil: [ result addAll: superClass name asString ].
	lfsp := Character lf asString tab.
	newByteSubclass := false.
	thePackageName := self computePackageNameFor: self theClass in: packageNames. "performance improvement here"
	nonRowanClass := thePackageName = Rowan unpackagedName.
	(self theClass isBytes _and: [ superClass isBytes not ])
		ifTrue: [ 
			nonRowanClass
				ifTrue: [ result addAll: ' byteSubclass: ''' ]
				ifFalse: [ result addAll: ' rwByteSubclass: ''' ].
			result
				addAll: className;
				addLast: $'.
			newByteSubclass := true ]
		ifFalse: [ 
			(self theClass isIndexable and: [ superClass isIndexable not ])
				ifTrue: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' indexableSubclass: ''' ]
						ifFalse: [ result addAll: ' rwIndexableSubclass: ''' ].
					result
						addAll: className;
						addLast: $' ]
				ifFalse: [ 
					nonRowanClass
						ifTrue: [ result addAll: ' subclass: ''' ]
						ifFalse: [ result addAll: ' rwSubclass: ''' ].
					result
						addAll: className;
						addLast: $' ] ].
	newByteSubclass
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'instVarNames: #(';
				addAll:
						(self theClass _instVarNamesWithSeparator: lfsp , '                 ');
				add: $) ].
	result
		addAll: lfsp;
		addLast: 'classVars: #('.
	self theClass _sortedClassVarNames
		do: [ :aKey | 
			result addLast: $ .
			(aKey includesValue: $')
				ifTrue: [ result addAll: aKey _asSource ]
				ifFalse: [ result addAll: aKey ] ].
	result addLast: $).
	result
		addAll: lfsp;
		addLast: 'classInstVars: #('.
	civs := self theClass class allInstVarNames.
	civs removeFrom: 1 to: self theClass class superClass instSize.
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
	result addAll: '#()'.	"ignored for now"
	nonRowanClass
		ifTrue: [ 
			"if the class is unpackaged, then we need to provide for the specification of symbol dictionary into which the class will be installed"
			result
				addAll: lfsp;
				addAll: 'inDictionary: '.
			anArray := Rowan image symbolList dictionariesAndSymbolsOf: self theClass.
			anArray isEmpty
				ifTrue: [ result addAll: '''''' ]
				ifFalse: [ result addAll: ((anArray at: 1) at: 1) name asString ] ]
		ifFalse: [ 
			result
				addAll: lfsp;
				addAll: 'category: '.
			result addAll: self theClass category printString.
			(true and: [ thePackageName = self theClass category ])
				ifFalse: [ 
					result
						addAll: lfsp;
						addAll: 'packageName: '.
					result addAll: thePackageName printString ] ].
	self theClass _hasConstraints
		ifTrue: [ 
			result
				add: lfsp;
				add: self theClass _rwDefinitionOfConstraints ].
	result
		add: lfsp;
		add: self theClass _rwOptionsForDefinition.
	result add: Character lf.
	^ result
%

category: 'client commands'
method: RowanClassService
classHierarchy
	hierarchyServices := self classHierarchy: (Array with: self theClass). 
	RowanCommandResult addResult: self.
%

category: 'Accessing'
method: RowanClassService
classHierarchyNames

	| names |
	names := Array new. 
	hierarchyServices keys do:[:classService | 
		classService == #nil ifFalse:[names add: classService name]].
	^names
%

category: 'Accessing'
method: RowanClassService
classInstVarNames
	^classInstVarNames
%

category: 'Updating'
method: RowanClassService
classInstVarNames: newValue
	classInstVarNames := newValue
%

category: 'Accessing'
method: RowanClassService
classOrMeta

	^meta 
			ifTrue:[self theClass class] 
			ifFalse: [self theClass].
%

category: 'instance creation'
method: RowanClassService
classServiceFromOop: anOop
	| theClass className classService |
	theClass := Object _objectForOop: anOop. 
	className := theClass name. 
	classService := RowanClassService new name: className.
	^className asString = name asString ifTrue:[
			className asString = 'Object' 
				ifTrue:[
					classService basicRefreshFrom: theClass]
				ifFalse:[
					classService fastRefresh]]
		ifFalse:[
			classService minimalRefreshFrom: theClass]
%

category: 'instance creation'
method: RowanClassService
classServiceFromOop: anOop packageNames: packageNames
	| theClass className classService |
	theClass := Object _objectForOop: anOop. 
	className := theClass name. 
	classService := RowanClassService new name: className.
	^classService minimalRefreshFrom: theClass packageNames: packageNames
%

category: 'Accessing'
method: RowanClassService
classType
	^classType
%

category: 'Updating'
method: RowanClassService
classType: newValue
	classType := newValue
%

category: 'Accessing'
method: RowanClassService
classVarNames
	^classVarNames
%

category: 'Updating'
method: RowanClassService
classVarNames: newValue
	classVarNames := newValue
%

category: 'Accessing'
method: RowanClassService
comment
	^comment
%

category: 'Updating'
method: RowanClassService
comment: newValue
	comment := newValue
%

category: 'constants'
method: RowanClassService
compileMethod: methodString behavior: aBehavior symbolList: aSymbolList inCategory: categorySymbol
	"returns (nil -> anArrayOfErrors) or (aGsNMethod -> compilerWarnings) or (aGsNMethod -> nil)"

	| method warnings |
	
	[ [ [ [ method := aBehavior rwCompileMethod: methodString category: categorySymbol.]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: false ]]
			on: CompileError
			do: [:ex | ^nil -> (ex gsArguments at: 1)]]
				on: CompileWarning
				do: 
					[:ex | 
					warnings := ex warningString.
					ex resume]]
					on: RwPerformingUnpackagedEditNotification
					do: [:ex | ex resume ] .
	^[(self compiledMethodAt: method key selector inClass: aBehavior) -> warnings] on: Error
		do: [:ex | ex return: method -> warnings]
%

category: 'client commands'
method: RowanClassService
copyClassTo: newClassName
	| newTemplate newClass newClassService index |
	(Rowan image symbolList resolveSymbol: newClassName)
		ifNotNil: [ ^ self inform: newClassName , ' already exists' ].
	index := template findPattern: (Array with: name) startingAt: 1.
	newTemplate := template copy.
	newTemplate removeFrom: index to: index + name size - 1.
	newTemplate insertAll: newClassName at: index.
	newClass := GsCurrentSession currentSession execute: newTemplate.
	newClassService := RowanClassService new name: newClassName.
	self theClass thisClass
		methodsDo: [ :selector :gsMethod | 
			newClassService
				compileMethod: gsMethod sourceString
				behavior: newClass
				symbolList: Rowan image symbolList
				inCategory: (self theClass thisClass categoryOfSelector: selector) asSymbol ].
	self theClass thisClass class
		methodsDo: [ :selector :gsMethod | 
			newClassService
				compileMethod: gsMethod sourceString
				behavior: newClass class
				symbolList: Rowan image symbolList
				inCategory:
					(self theClass thisClass class categoryOfSelector: selector) asSymbol ].
	newClassService update.
	(RowanPackageService forPackageNamed: newClassService packageName) update.
	(RowanDictionaryService new name: dictionaryName) update
%

category: 'Updating'
method: RowanClassService
definedPackageName: newValue

	definedPackageName := newValue
%

category: 'Accessing'
method: RowanClassService
dictionaryName
	^dictionaryName
%

category: 'rsr'
method: RowanClassService
executeCommand

	updateAfterCommand ifNil: [updateAfterCommand := true]. 
	^super executeCommand
%

category: 'Updating'
method: RowanClassService
expand: boolean

	expand := boolean
%

category: 'client commands'
method: RowanClassService
fastRefresh
	"pushes less information to ston so it's faster"

	| theClass |
	theClass := self theClass. 
	self refreshFrom: theClass. 
	methods do:[:service1 |
			service1 source: nil;
				stepPoints: Array new].
	visibleTests do:[:service2 |
			service2 source: nil;
				stepPoints: Array new.
			].
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanClassService
fileoutCategories: array on: path
	| ws file |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	array
		do: [ :category | ws nextPutAll: (self behavior fileOutCategory: category) ].
	(GsFile existsOnServer: path)
		ifTrue: [ 
			(self confirm: 'File exists. File out anyway?')
				ifFalse: [ ^ self ] ].
	file := GsFile openAppendOnServer: path.
	[ file nextPutAll: ws contents ]
		ensure: [ file close ]
%

category: 'client commands'
method: RowanClassService
fileoutClassOn: path
	| ws |
	ws := WriteStream on: String new.
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: self theClass fileOutClass.
	self fileOut: ws on: path
%

category: 'Accessing'
method: RowanClassService
filters
	^filters
%

category: 'Updating'
method: RowanClassService
filters: newValue
	filters := newValue
%

category: 'initialization'
method: RowanClassService
forClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self refreshFrom: theClass.
%

category: 'client commands'
method: RowanClassService
fullHierarchy
	| behavior sortedSubclasses |
	behavior := self theClass.
	hierarchyServices := Dictionary new.
	hierarchyServices at: #'expand' put: Array new.
	sortedSubclasses := behavior subclasses
		asSortedCollection: [ :x :y | x name < y name ].
	updateType := #'updatedFullInClassHierarchy:browser:'.
	RowanCommandResult addResult: self.
	sortedSubclasses
		do: [ :subclass | 
			| classService |
			classService := (self classServiceFromOop: subclass asOop) meta: meta.
			(hierarchyServices at: #'expand') add: classService.
			classService allSubclassServices ]
%

category: 'comparing'
method: RowanClassService
hash
	^self name hash bitXor: meta hash
%

category: 'Accessing'
method: RowanClassService
hasSubclasses
	^hasSubclasses
%

category: 'Accessing'
method: RowanClassService
hasSubclasses: object
	hasSubclasses := object
%

category: 'Accessing'
method: RowanClassService
hierarchyServices

	^hierarchyServices
%

category: 'initialization'
method: RowanClassService
initialize

	isExtension := false.
	selectedMethods := Array new.
	meta := false. "assume most of our work is on the instance side"
	selectedPackageServices := Array new.
	isNewClass := false.
	methods := Array new.
	isInSymbolList := true.
	categories := Array new.
	updateAfterCommand := true.
	hasSubclasses := false.
%

category: 'initialization'
method: RowanClassService
initializeCategoriesFor: classOrMeta

	| theFilters |
	theFilters := SortedCollection new.
	classOrMeta env: 0 categorysDo: [:category :selector | theFilters add: category asString].
	categories := theFilters asOrderedCollection.
%

category: 'method history'
method: RowanClassService
initializeMethodHistoryFor: source
  "about to compile a method. If possible, ensure it's method history is setup."

  | rowanMethodHistory methodHistory selector methodService |
  rowanMethodHistory := self rowanMethodHistory.
  selector := [ (Rowan platform parseSelectorFrom: source) asSymbol ]
    on: CompileWarning
    do: [ :ex | ex resume ].
  selector = #'_____could_not_parse_selector_from_method_source_____'
    ifTrue: [ ^ self	"invalid source, continue and let save method fail" ]
    ifFalse: [ 
      | compiledMethod |
      compiledMethod := (Object _objectForOop: oop)
        compiledMethodAt: selector
        environmentId: 0
        otherwise: nil.
      compiledMethod
        ifNil: [ ^ self	"we'll create history after the method is compiled" ].
      methodService := RowanMethodService
        forSelector: selector
        class: self theClass
        meta: meta
        organizer: self organizer.
      methodHistory := rowanMethodHistory
        at: methodService unregisteredCopy
        ifAbsentPut: [ Array with: methodService ] ]
%

category: 'initialization'
method: RowanClassService
initializeTestMethodsFor: aClass
	| testSelectors |
	(aClass inheritsFrom: TestCase) ifTrue:[
		aClass isAbstract ifTrue:[^self]. 
		testSelectors := aClass thisClass allTestSelectors.
		methods do:[:methodService | 
			methodService isTestMethod: (testSelectors includes: methodService selector)]].
%

category: 'initialization'
method: RowanClassService
initializeVariablesFor: classOrMeta

	| theFilters |
	theFilters := SortedCollection new.
	theFilters addAll: (classOrMeta allInstVarNames collect:[:instVar | instVar asString]).
	variables := theFilters asOrderedCollection.
%

category: 'Accessing'
method: RowanClassService
instVarNames
	^instVarNames
%

category: 'Updating'
method: RowanClassService
instVarNames: newValue
	instVarNames := newValue
%

category: 'testing'
method: RowanClassService
isClassService

	^true
%

category: 'Updating'
method: RowanClassService
isExtension: boolean

	isExtension := boolean
%

category: 'Updating'
method: RowanClassService
isNewClass: boolean
	isNewClass := boolean
%

category: 'testing'
method: RowanClassService
isPackageClean

	^self packageIsDirty not
%

category: 'testing'
method: RowanClassService
isProjectClean
  ^ self projectIsDirty not
%

category: 'Updating'
method: RowanClassService
isTestCase

	^isTestCase
%

category: 'Updating'
method: RowanClassService
isTestCase: aBoolean

	isTestCase := aBoolean
%

category: 'testing'
method: RowanClassService
loadedPackageExistsAndIsInSameDictionary: thePackageName
	| actualName loadedPackage packageDictionaryName |
	actualName := Rowan image packageNames
		detect: [ :loadedName | loadedName asLowercase = thePackageName asLowercase ]
		ifNone: [  ].
	loadedPackage := Rowan image loadedPackageNamed: actualName ifAbsent: [  ].
	^ loadedPackage
		ifNil: [ false ]
		ifNotNil: [ 
			packageDictionaryName := loadedPackage loadedProject
				gemstoneSymbolDictNameForPackageNamed: thePackageName.
			packageDictionaryName = dictionaryName ]
%

category: 'Accessing'
method: RowanClassService
meta

	^meta
%

category: 'Updating'
method: RowanClassService
meta: anObject

	meta := anObject
%

category: 'Accessing'
method: RowanClassService
methods

	"for testing"
	^methods
%

category: 'private'
method: RowanClassService
methodServiceFrom: gsNMethod in: behavior compiltationResult: compilationResult
	| methodService |

	methodService := RowanMethodService forGsNMethod: gsNMethod organizer: self organizer. 
	methodService compilationWarnings: compilationResult value.
	^methodService
%

category: 'private'
method: RowanClassService
methodServicesFor: classOrMeta organizer: theOrganizer

	methods addAll: (classOrMeta selectors collect:[:sel | 
			RowanMethodService 
				forSelector: sel class: classOrMeta thisClass meta: meta organizer: theOrganizer])
%

category: 'private'
method: RowanClassService
methodServicesFor: classOrMeta organizer: theOrganizer subclasses: subclasses
	methods
		addAll:
			(classOrMeta selectors
				collect: [ :sel | 
					RowanMethodService
						forSelector: sel
						class: classOrMeta thisClass
						meta: meta
						organizer: theOrganizer
						subclasses: subclasses])
%

category: 'private'
method: RowanClassService
methodsIn: theClass categories: theCategories

	| selectors |
	selectors := Array new. 
	theCategories do:[:category |
		selectors addAll: (theClass selectorsIn: category)]. 
	^methods select:[:methodService | selectors includes: methodService selector]
%

category: 'testing'
method: RowanClassService
methodsNamed: selector
	"For testing. Multiple because class could have both instance and class methods"

	^methods select:[:methodService | methodService selector = selector]
%

category: 'initialization'
method: RowanClassService
minimalForClassNamed: className 

	| theClass |
	self name: className. 
	theClass := self theClass. 
	self minimalRefreshFrom: theClass.
%

category: 'initialization'
method: RowanClassService
minimalForClassNamed: className packageNames: packageNames
	| theClass |
	self name: className.
	theClass := self theClass.
	self minimalRefreshFrom: theClass packageNames: packageNames
%

category: 'initialization'
method: RowanClassService
minimalRefreshFrom: theClass
	| classOrMeta  |
	command := nil. 
	commandArgs := nil. 
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	oop := theClass asOop.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	packageName := definedPackageName := classOrMeta rowanPackageName.
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	template := self classCreationTemplate.
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	self setIsTestCase.
%

category: 'initialization'
method: RowanClassService
minimalRefreshFrom: theClass packageNames: packageNames
	| classOrMeta  |
	command := nil. 
	commandArgs := nil. 
	versions := theClass classHistory size.
	version := theClass classHistory indexOf: theClass.
	oop := theClass asOop.
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	packageName := definedPackageName := (self computePackageNameFor: classOrMeta in: packageNames).
	self setDictionary: classOrMeta.
	projectName := classOrMeta rowanProjectName.
	instVarNames := classOrMeta instVarNames asArray. 
	template := self classCreationTemplateUsing: packageNames.
	self initializeVariablesFor: classOrMeta. 
	self initializeCategoriesFor: classOrMeta.
	self setIsTestCase.
%

category: 'client commands'
method: RowanClassService
moveMethods: methodServices to: category
	"update the dirty flag of the project & package both before and after the move"

	| behavior |
	behavior := self classOrMeta.
	methodServices
		do: [ :methodService | 
			| beforePackageName |
			methodService organizer: self organizer.
			beforePackageName := methodService packageName.
			behavior rwMoveMethod: methodService selector toCategory: category.
			methodService update.
			methodService updatePackageProjectAfterCategoryChange: beforePackageName ].
	self update.
	self selectedMethods: methodServices
%

category: 'client commands'
method: RowanClassService
moveMethodSelectors: methodSelectors toPackageNamed: thePackageName
	| targetPackageService theClass |
	theClass := meta ifTrue: [self theClass class] ifFalse: [self theClass]. 
	methodSelectors
		do: [ :selector | theClass rwMoveMethod: selector toPackage: thePackageName ].
	self update.
	targetPackageService := (RowanPackageService forPackageNamed: thePackageName)
		update.
	(RowanProjectService new name: targetPackageService projectName) update
%

category: 'client commands'
method: RowanClassService
moveToPackageNamed: thePackageName
	| sourcePackageService targetPackageService |
	sourcePackageService := RowanPackageService forPackageNamed: packageName. 
	self theClass rwMoveClassToPackage: thePackageName.
	self update.
	sourcePackageService update. 
	(RowanProjectService new name: sourcePackageService projectName) update.
	(targetPackageService := RowanPackageService forPackageNamed: thePackageName) update.
	(RowanProjectService new name: targetPackageService projectName) update
%

category: 'Accessing'
method: RowanClassService
name
	^name
%

category: 'Updating'
method: RowanClassService
name: newValue
	name := newValue asString
%

category: 'private'
method: RowanClassService
objectInBaseNamed: aString

	^Rowan image symbolList objectNamed: aString asSymbol
%

category: 'client commands'
method: RowanClassService
oneLevelClassHierarchy
	"good for expanding an existing hierarchy quickly"

	| behavior sortedSubclasses |
	behavior := self theClass.
	hierarchyServices := Dictionary new.
	hierarchyServices at: #'expand' put: Array new.
	sortedSubclasses := behavior subclasses
		asSortedCollection: [ :x :y | x name < y name ].
	sortedSubclasses
		do: [ :subclass | 
			| classService |
			classService := (self classServiceFromOop: subclass asOop) meta: meta.
			(hierarchyServices at: #'expand') add: classService.
			(self organizer subclassesOf: subclass) notEmpty
				ifTrue: [ classService hasSubclasses: true ] ].
	updateType := #'updatedOneLevelInClassHierarchy:browser:'.
	RowanCommandResult addResult: self
%

category: 'Accessing'
method: RowanClassService
oop
	^oop
%

category: 'Updating'
method: RowanClassService
oop: newValue
	oop := newValue
%

category: 'testing'
method: RowanClassService
packageIsDirty
	| behavior |
	packageName
		ifNil: [ 
			behavior := self theClass.
			packageName := behavior rowanPackageName ].
	packageName = Rowan unpackagedName
		ifTrue: [ ^ true ].	"avoid a refresh by assuming it's dirty"
	^ (RowanPackageService forPackageNamed: packageName) rowanDirty
%

category: 'Accessing'
method: RowanClassService
packageName
	
	^packageName
%

category: 'Updating'
method: RowanClassService
packageName: pkgName
	
	packageName := pkgName
%

category: 'Accessing'
method: RowanClassService
poolDictionaryNames
	^poolDictionaryNames
%

category: 'Updating'
method: RowanClassService
poolDictionaryNames: newValue
	poolDictionaryNames := newValue
%

category: 'rsr'
method: RowanClassService
postCommandExecution
	super postCommandExecution.
	methods
		ifNil: [ 
			methods := Array new	"may not have been a behavior" ]
		ifNotNil: [ methods do: [ :methodService | methodService clearOrganizers ] ]
%

category: 'printing'
method: RowanClassService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'testing'
method: RowanClassService
projectIsDirty

	| behavior |
	behavior := self theClass.
	behavior rowanProjectName =  Rowan unpackagedName ifTrue:[^true]. "avoid a refresh by assuming it's dirty" 
	^(RowanProjectService new name: behavior rowanProjectName) rowanDirty
%

category: 'other'
method: RowanClassService
projectName

	^projectName
%

category: 'Updating'
method: RowanClassService
projectName: newValue
	projectName := newValue
%

category: 'initialization'
method: RowanClassService
refreshFrom: theClass
	| classOrMeta  |
	self basicRefreshFrom: theClass. 
	classOrMeta := meta == true ifTrue:[theClass class] ifFalse:[theClass].
	self refreshMethodsFor: classOrMeta.
	shouldUpdate := false.
%

category: 'initialization'
method: RowanClassService
refreshMethodsFor: classOrMeta
	| gsNMethods subclasses |
	methods := SortedCollection sortBlock: [ :x :y | x selector < y selector ].
	subclasses := self organizer allSubclassesOf: classOrMeta thisClass.
	self methodServicesFor: classOrMeta organizer: self organizer subclasses: subclasses.
	methods := methods asOrderedCollection.
	classOrMeta allInstVarNames
		do: [ :instVar | 
			gsNMethods :=self  organizer accessorsOf: instVar inClass: classOrMeta.
			gsNMethods
				do: [ :gsNMethod | 
					| service |
					service := methods
						detect: [ :methodService | methodService selector = gsNMethod selector and:[methodService className = gsNMethod inClass name asString] ]
						ifNone: [  ].
					service ifNotNil: [ service accessedInstVars add: instVar asString ] ] ].
	self initializeTestMethodsFor: classOrMeta thisClass.
	self setVisibleTests.	"methods must be available"
	selectedPackageServices
		ifNotNil: [ 
			methods
				do: [ :methodService | 
					methodService
						inSelectedPackage:
							(selectedPackageServices
								detect: [ :selectedPackageService | selectedPackageService name = methodService packageName ]
								ifNone: [  ]) notNil ] ]
%

category: 'client commands'
method: RowanClassService
removeCategories: theCategories
	| theClass  | 
	self refreshFrom: self theClass. 
	theClass := self theClass.
	meta ifTrue:[theClass := theClass class]. 
	theCategories do: [:category |
		theClass rwRemoveCategory: category.
		].
	shouldUpdate := true.
%

category: 'client commands'
method: RowanClassService
removeInstVar: instVarName
	"assumes inst var refs were removed"

	| theClass definitionString browserService anonymousMethod |
	self refreshFrom: self theClass.
	theClass := self theClass.
	(self instVarNames includes: instVarName asSymbol)
		ifFalse: [ 
			| superService |
			superService := RowanClassService new
				name: theClass superClass name;
				meta: meta.
			^ superService removeInstVar: instVarName ].
	meta
		ifTrue: [ theClass := theClass class ].
	definitionString := self template.
	definitionString := self
		replaceSubString: ' ' , instVarName , ' '
		in: definitionString
		with: ' '.
	definitionString := self
		replaceSubString: ' ' , instVarName , ')'
		in: definitionString
		with: ')'.
	definitionString := self
		replaceSubString: '(' , instVarName , ' '
		in: definitionString
		with: '('.
	definitionString := self
		replaceSubString: '(' , instVarName , ')'
		in: definitionString
		with: '()'.
	anonymousMethod := definitionString
		_compileInContext: nil
		symbolList: Rowan image symbolList.
	SessionTemps current at: #'jadeiteCompileClassMethod' put: anonymousMethod.
	browserService := RowanBrowserServiceServer new.
	browserService recompileMethodsAfterClassCompilation.
	shouldUpdate := true
%

category: 'client commands'
method: RowanClassService
removeMethods: methodsToRemove

	| notRemoved |
	notRemoved := Array new. 
	methodsToRemove do: [:methodService |
		self removeSelector: methodService selector ifAbsent:[notRemoved add: methodService].
		(notRemoved includes: methodService) ifFalse:[
			methodService updateType: #removed:.
			RowanCommandResult addResult: methodService.
		]].
	self updateTests.
	notRemoved isEmpty ifFalse:[
		self error: 'These selectors were not removed - ', (notRemoved collect:[:svc | svc selector]) printString].
%

category: 'rowan'
method: RowanClassService
removeSelector: selector

	self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta
%

category: 'rowan'
method: RowanClassService
removeSelector: selector ifAbsent: absentBlock
	| theClass |
	theClass := self theClass. 
	meta ifTrue: [theClass := theClass class].
	(theClass compiledMethodAt: selector otherwise: nil) isNil ifTrue:[ ^absentBlock value ].
	[self browserTool removeMethod: selector forClassNamed: name asString isMeta: meta]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ]
%

category: 'client commands'
method: RowanClassService
renameCategoryFrom: old to: new

	| affectedSelectors behavior |

	self update. 
	self addCategory: new. 
	behavior := self classOrMeta.
	affectedSelectors := behavior selectorsIn: old.
	methods := methods select:[:methodService | affectedSelectors includes: methodService selector].
	self moveMethods: methods to: new.
	self removeCategories: (Array with: old).
	self updateClass.
%

category: 'client commands'
method: RowanClassService
renameClass: oldClassName to: newClassName
	"needs better class reference replacement than just string replacement"

	| references newMethods newClass oldClass |
	newMethods := Array new.
	oldClass := Rowan image resolveClassNamed: oldClassName.
	newClass := Rowan projectTools browser
		renameClassNamed: oldClassName
		to: newClassName.
	oop := newClass asOop.
	name := newClassName.
	self update.
	renamedName := oldClassName.
	self updateMethodsAfterRenameFrom: oldClassName to: newClassName.
	self updateSubclassesOf: newClass.
	references := self organizer update referencesToObject: oldClass.
	references
		do: [ :method | 
			| newSource compileResult failedCompile methodService oldSource |
			failedCompile := false.
			oldSource := method sourceString.
			newSource := self
				replaceSubString: oldClassName
				in: oldSource
				with: newClassName.
			compileResult := [ 
			method inClass
				rwCompileMethod: newSource
				category: (method inClass categoryOfSelector: method selector) asSymbol ]
				on: CompileError
				do: [ :ex | 
					failedCompile := true.
					method ].
			methodService := RowanMethodService
				forGsNMethod: compileResult
				organizer: self organizer.
			failedCompile
				ifTrue: [ methodService comparisonSource: oldClassName ]
				ifFalse: [ methodService comparisonSource: oldSource ].
			methodService failedCompile: failedCompile.
			methodService renamedName: oldClassName.
			newMethods add: methodService ].
	selectedPackageServices do: [ :ea | ea update ].
	RowanCommandResult addResult: (RowanAnsweringService new answer: newMethods)
%

category: 'private'
method: RowanClassService
replaceSubString: old in: string with: new
	| offset newSource |
	newSource := string. 
	offset := 1. 	
	[(offset := newSource findString: old startingAt: offset) = 0] whileFalse:[
		newSource := newSource copyReplaceFrom: offset to: offset + old size - 1 with: new. 
		offset := offset + new size. 
	].
	^newSource
%

category: 'rowan'
method: RowanClassService
rowanProjectName

	^projectName
%

category: 'client commands'
method: RowanClassService
runClassTests: classService

	"if it errors, the client will handle the error. 
	If it passes, we return true and the client
	will display decent results." 
	| behavior |
	behavior := classService theClass. 
	self refreshFrom: behavior.
	self tests do:[:methodService |
			behavior debug: methodService selector]. 
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
%

category: 'client commands'
method: RowanClassService
runMethodTests: methodServices

	| behavior |
	behavior := self theClass.  
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior debug: methodService selector]].
	RowanCommandResult addResult: (RowanAnsweringService new answer: true).
%

category: 'client commands'
method: RowanClassService
saveMethodSource: source category: category
	| behavior compilationResult gsNMethod updatedCategory methodService unicodeSource |
	unicodeSource := source. 
	meta
		ifNil: [ 
			behavior := Object _objectForOop: oop.
			meta := behavior isMeta ]
		ifNotNil: [ 
			behavior := meta
				ifTrue: [ self theClass class ]
				ifFalse: [ self theClass ] ].
	oop := behavior asOop.
	self initializeMethodHistoryFor: unicodeSource.
	updatedCategory := category ifNil: [ 'other' ].
	compilationResult := self
		compileMethod: unicodeSource
		behavior: behavior
		symbolList: Rowan image symbolList
		inCategory: updatedCategory asSymbol.
	(gsNMethod := compilationResult key) isNil
		ifTrue: [ 
			System
				signal: 1001
				args: (Array with: compilationResult value)
				signalDictionary: GemStoneError ].
	organizer := ClassOrganizer new. 
	methodService := self
		methodServiceFrom: gsNMethod
		in: behavior
		compiltationResult: compilationResult.
	RowanCommandResult addResult: methodService.
	self refreshFrom: self theClass. "make sure new methods have proper inst var refs"
	RowanQueryService new
		organizer: ClassOrganizer new;
		hierarchyImplementorsOf: methodService selector
			inClass: methodService className.	"this will update hierarchy method indicators for client"
	self selectedMethods: (Array with: methodService).
	self updateDirtyState.
	(methods includes: methodService)
		ifFalse: [ methods add: methodService ].
	methodService isTestMethod
		ifTrue: [ self updateTests ].
	self
		updateSymbols:
			gsNMethod _selectorPool asArray , (Array with: methodService selector).
	methodService addToMethodHistory.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanClassService
saveMethodSources: sources category: category
	sources do:[:source | 
		self saveMethodSource: source category: category]
%

category: 'other'
method: RowanClassService
selectedMethods
	"client side selection. Used after a method compile" 
	^selectedMethods
%

category: 'Updating'
method: RowanClassService
selectedMethods: theMethods
	selectedMethods := theMethods
%

category: 'Accessing'
method: RowanClassService
selectedPackageServices
	^selectedPackageServices
%

category: 'Updating'
method: RowanClassService
selectedPackageServices: newValue
	selectedPackageServices := newValue
%

category: 'Accessing'
method: RowanClassService
selectors

	^methods collect:[:methodService | methodService selector]
%

category: 'perform'
method: RowanClassService
servicePerform: symbol withArguments: collection
	| wasClean |
	packageName ifNil: [ self update ].	"the class may not have enough information to perform the symbol"
	self isUpdatingButFoundToBeDeleted
		ifTrue: [ ^ self handleDeletedService ].
	wasClean := self isPackageClean.
	super
		servicePerform: symbol
		withArguments: collection
		shouldUpdate: updateAfterCommand.
	wasClean
		ifTrue: [ self updatePackageAndProject ]
%

category: 'Accessing'
method: RowanClassService
setComment
  comment := self theClass thisClass comment
%

category: 'private'
method: RowanClassService
setDictionary: classOrMeta
	| dictionaryList |
		dictionaryList := Rowan image symbolList dictionariesAndSymbolsOf: classOrMeta thisClass.
		dictionaryName := dictionaryList isEmpty 
		ifTrue:[String new]
		ifFalse:[dictionaryList first first name asString].
%

category: 'Updating'
method: RowanClassService
setIsTestCase

	isTestCase := self theClass isSubclassOf: TestCase
%

category: 'client commands'
method: RowanClassService
setIsTestCaseCommand

	self setIsTestCase.
%

category: 'client commands'
method: RowanClassService
setVisibleTests
	visibleTests := SortedCollection sortBlock: [:x :y | x selector < y selector]. 
	visibleTests addAll: self allTests.
	visibleTests := visibleTests asArray.
%

category: 'client commands'
method: RowanClassService
subclassCreationTemplate
  | answerService newClassPackageName |
  answerService := RowanAnsweringService new.
  newClassPackageName := self theClass rowanPackageName = Rowan unpackagedName
    ifTrue: [ self addSubclassWarningString ]
    ifFalse: [ self theClass rowanPackageName ].
  answerService
    answer:
      (self browserTool
        classCreationTemplateForSubclassOf: name
        className: 'NewSubclass'
        category: newClassPackageName).
  RowanCommandResult addResult: answerService
%

category: 'private'
method: RowanClassService
subclassServices: subclasses

	| sortedSubclasses |

	sortedSubclasses := SortedCollection sortBlock: [:x :y | x name < y name]. 
	sortedSubclasses addAll: subclasses. 
	^(sortedSubclasses collect:[:cls | (self classServiceFromOop: cls asOop) meta: meta]) asArray.
%

category: 'Accessing'
method: RowanClassService
subclassType
	^subclassType
%

category: 'Updating'
method: RowanClassService
subclassType: newValue
	subclassType := newValue
%

category: 'Accessing'
method: RowanClassService
superclassName
	^superclassName
%

category: 'Updating'
method: RowanClassService
superclassName: newValue
	superclassName := newValue
%

category: 'Accessing'
method: RowanClassService
template
	^template
%

category: 'Updating'
method: RowanClassService
template: newValue
	template := newValue
%

category: 'private'
method: RowanClassService
tests

	^methods select:[:methodService | methodService selector asString matchPattern: #('test' $*)]
%

category: 'instance creation'
method: RowanClassService
theClass
	| theClass |
	theClass := oop ifNil:[Rowan globalNamed: name] ifNotNil: [Object _objectForOop: oop].
	theClass isMeta ifTrue:[oop := theClass thisClass asOop]. 
	(Rowan globalNamed: name) ifNil:[isInSymbolList := false]. 
	theClass ifNil: [^nil]. 
	^theClass thisClass
%

category: 'updates'
method: RowanClassService
update 
	super update.
	self updateClass.
%

category: 'updates'
method: RowanClassService
updateClass

	"It's possible to have a nil class. For example, if we added & selected
	a class then aborted."

	| theClass |
	theClass := self theClass. 
	theClass isNil ifTrue:[oop := nil. ^self]. 
	theClass isBehavior ifFalse:[oop := theClass asOop. ^self].
	self refreshFrom: theClass.
	RowanCommandResult addResult: self
%

category: 'updates'
method: RowanClassService
updateDirtyState
	| projectService wasDirty |
	selectedPackageServices
		do: [ :packageService | 
			wasDirty := packageService isDirty.
			wasDirty
				ifFalse: [ 
					packageService update.
					(packageService isDirty and: [ wasDirty not ])
						ifTrue: [ 
							RowanCommandResult addResult: packageService.
							projectService := RowanProjectService
								newNamed: self theClass rowanProjectName.
							RowanCommandResult addResult: projectService ] ] ]
%

category: 'initialization'
method: RowanClassService
updateIsExtension
  isExtension := ((selectedPackageServices
    collect: [ :packageService | packageService name ])
    includes: definedPackageName) not
%

category: 'updates'
method: RowanClassService
updateLatest
  oop := ((Rowan image symbolList resolveSymbol: name)
    ifNil: [ 
      wasRemoved := true.
      updateType := #'removedClass:'.
      RowanCommandResult addResult: self.
      ^ self ]) value asOop.
  super updateLatest
%

category: 'private'
method: RowanClassService
updateMethodsAfterRenameFrom: oldClassName to: newClassName
  methods
    do: [ :methodService | 
      methodService
        renamedName: oldClassName;
        className: newClassName. 
      RowanCommandResult addResult: methodService ]
%

category: 'updates'
method: RowanClassService
updatePackageAndProject
	| packageService projectService |
	packageName = Rowan unpackagedName ifTrue:[^self]. 
	packageService := RowanPackageService forPackageNamed: packageName.
	packageService update.
	projectService := RowanProjectService new name: projectName.
	projectService update
%

category: 'private'
method: RowanClassService
updateSubclasses
	self updateSubclassesOf: self theClass
%

category: 'private'
method: RowanClassService
updateSubclassesOf: newClass
  organizer := ClassOrganizer new.
  (self organizer allSubclassesOf: newClass)
    do: [ :subclass | 
      | subclassService |
      subclassService := RowanClassService minimalForClassNamed: subclass name.
      RowanCommandResult addResult: subclassService ]
%

category: 'updates'
method: RowanClassService
updateSymbols: newSymbols
  | browserService |
  browserService := RowanBrowserService new.
  browserService newCachedSelectors addAll: newSymbols.
  browserService updateType: #'addCachedSymbols:'. 
  RowanCommandResult addResult: browserService
%

category: 'updates'
method: RowanClassService
updateTests
	"update the test browsers on certain operations"

	RowanBrowserService new packagesWithTests.
	(RowanPackageService forPackageNamed: packageName) testClasses.
	RowanCommandResult addResult: self update
%

category: 'accessing'
method: RowanClassService
updateType
	^updateType
%

category: 'accessing'
method: RowanClassService
updateType: object
	updateType := object
%

category: 'Accessing'
method: RowanClassService
version
	^version
%

category: 'Updating'
method: RowanClassService
version: newValue
	version := newValue
%

category: 'Accessing'
method: RowanClassService
versions
	^versions
%

category: 'Updating'
method: RowanClassService
versions: newValue
	versions := newValue
%

category: 'Accessing'
method: RowanClassService
visibleTests

	^visibleTests
%

category: 'testing'
method: RowanClassService
wasDeleted
	^(Rowan globalNamed: name) isNil
%

category: 'Accessing'
method: RowanClassService
wasRemoved: boolean

	wasRemoved := boolean
%

! Class implementation for 'RowanCompileErrorService'

!		Class methods for 'RowanCompileErrorService'

category: 'rsr'
classmethod: RowanCompileErrorService
templateClassName

	^#RowanCompileErrorService
%

!		Instance methods for 'RowanCompileErrorService'

category: 'accessing'
method: RowanCompileErrorService
gsArguments
	^gsArguments
%

category: 'accessing'
method: RowanCompileErrorService
gsArguments: object
	gsArguments := object
%

! Class implementation for 'RowanComponentService'

!		Class methods for 'RowanComponentService'

category: 'instance creation'
classmethod: RowanComponentService
forComponentNamed: componentName projectService: theProjectService
	| inst component |
	component := theProjectService rwProject
		componentOrPackageGroupNamed: componentName.
	inst := self new
		name: componentName;
		basename: component label.
	inst computeSubComponentsUsingProjectService: theProjectService.
	inst projectService: theProjectService.
	^ inst
%

category: 'rsr'
classmethod: RowanComponentService
templateClassName

	^#RowanComponentService
%

!		Instance methods for 'RowanComponentService'

category: 'operations'
method: RowanComponentService
addFlattenedHierarchyTo: dictionary
	| sortedComponents |
	sortedComponents := componentServices
		asSortedCollection: [ :x :y | x name < y name ].
	dictionary at: self put: sortedComponents asArray.
	componentServices
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
%

category: 'accessing'
method: RowanComponentService
basename
	^basename
%

category: 'accessing'
method: RowanComponentService
basename: object
	basename := object
%

category: 'accessing'
method: RowanComponentService
component
	^ projectService rwProject componentNamed: name
%

category: 'accessing'
method: RowanComponentService
componentServices
	^componentServices
%

category: 'accessing'
method: RowanComponentService
componentServices: anArray

	componentServices := anArray
%

category: 'operations'
method: RowanComponentService
computeSubComponentsUsingProjectService: theProjectService
	componentServices := (theProjectService rwProject
		loadedSubcomponentsOf: name
		ifNone: [ Array new ])
		collect: [ :subcomponent | 
			RowanComponentService
				forComponentNamed: subcomponent name
				projectService: theProjectService ]
%

category: 'instance creation'
method: RowanComponentService
forComponentNamed: componentName projectService: theProjectService
	| inst component |
	component := theProjectService rwProject componentOrPackageGroupNamed: componentName.
	inst := self new
		name: componentName;
		basename: component label.
	inst computeSubComponentsUsingProjectService: theProjectService.
	inst projectService: theProjectService. 
	^ inst
%

category: 'initialization'
method: RowanComponentService
initialize
	" to do "
	super initialize.
	componentServices := Array new.
%

category: 'accessing'
method: RowanComponentService
name
	^name
%

category: 'accessing'
method: RowanComponentService
name: object
	name := object
%

category: 'accessing'
method: RowanComponentService
projectService
	^projectService
%

category: 'accessing'
method: RowanComponentService
projectService: theProjectService
	projectService := theProjectService
%

category: 'updating'
method: RowanComponentService
update
	super update.
	self updatePackageServices
%

category: 'clientCommands'
method: RowanComponentService
updatePackageServices
	packageServices := (projectService rwProject allPackageNamesIn: name)
		collect: [ :packageName | RowanPackageService forPackageNamed: packageName ].
	RowanCommandResult addResult: self
%

! Class implementation for 'RowanDebuggerService'

!		Instance methods for 'RowanDebuggerService'

category: 'debug string'
method: RowanDebuggerService
debugStringFrom: aString
  | debugStream newStream char peekChar |
  debugStream := ReadStream on: aString trimLeadingBlanks.
  [ 
  peekChar := debugStream peek.
  peekChar = Character tab
    or: [ peekChar = Character cr or: [ peekChar = Character lf ] ]	"trimLeadingBlanks doesn't look for tab or cr or lf" ]
    whileTrue: [ debugStream next ].
  debugStream contents isEmpty
    ifTrue: [ ^ 'nil halt.' ].
  newStream := WriteStream on: String new.
  (char := debugStream next) = $|
    ifTrue: [ 
      newStream nextPut: char.
      newStream
        nextPutAll: (debugStream upTo: $|);
        nextPut: $|;
        nextPut: Character space;
        nextPutAll: 'nil halt. ' ]
    ifFalse: [ 
      newStream
        nextPutAll: 'nil halt. ';
        nextPut: char ].
  newStream nextPutAll: debugStream upToEnd.
  ^ newStream contents
%

category: 'other'
method: RowanDebuggerService
registeredWindowsIncludesOop: oop
	| dictionary registeredOops |
	dictionary := SessionTemps current
		at: #'rowanServicesWindowRegistry'
		ifAbsentPut: [ Dictionary new ].
	registeredOops := Array new.
	dictionary values
		do: [ :array | array do: [ :object | registeredOops add: object asOop ] ].
	^ registeredOops includes: oop
%

category: 'release'
method: RowanDebuggerService
saveProcessOop: processOop
  | jadeiteProcesses process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      (jadeiteProcesses includes: process)
        ifFalse: [ 
          "tests may pass through this method twice"
          jadeiteProcesses add: process ] ]
%

category: 'clientCommands'
method: RowanDebuggerService
terminateProcess: processOop
  | process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifFalse: [ ^ self ].
  RowanDebuggerService new releaseProcessOop: processOop.
  process terminate.
  (Delay forMilliseconds: 10) wait	"allow forked processes to finish"
%

category: 'updating'
method: RowanDebuggerService
updateProcesses
	| gsProcess |
	gsProcess := Object _objectForOop: initialProcessOop.
	(gsProcess isKindOf: GsProcess)
		ifFalse: [ ^ processes := OrderedCollection new	"might be a dead debugger" ].
	processes := OrderedCollection
		with: (RowanProcessService onActiveProcess: gsProcess).
	ProcessorScheduler scheduler readyProcesses
		do: [ :each | 
			processes
				add:
					(RowanProcessService new
						oop: each asOop;
						status: 'ready') ].
	ProcessorScheduler scheduler suspendedProcesses
		do: [ :each | 
			processes
				add:
					(RowanProcessService new
						oop: each asOop;
						status: 'suspended') ].
	ProcessorScheduler scheduler waitingProcesses
		do: [ :each | 
			processes
				add:
					(RowanProcessService new
						oop: each asOop;
						status: 'waiting') ].
	RowanCommandResult addResult: self
%

! Class implementation for 'RowanDefinitionService'

!		Class methods for 'RowanDefinitionService'

category: 'accessing'
classmethod: RowanDefinitionService
named: aName

	^self new name: aName
%

!		Instance methods for 'RowanDefinitionService'

category: 'accessing'
method: RowanDefinitionService
name
	^name
%

category: 'accessing'
method: RowanDefinitionService
name: object
	name := object
%

category: 'accessing'
method: RowanDefinitionService
properties
	^properties
%

category: 'accessing'
method: RowanDefinitionService
properties: object
	properties := object
%

! Class implementation for 'RowanComponentDefinitionService'

!		Class methods for 'RowanComponentDefinitionService'

category: 'private'
classmethod: RowanComponentDefinitionService
componentDefinitionServiceClassFor: theComponent
	^ (theComponent isKindOf: RwPlatformSubcomponent)
		ifTrue: [ RowanPlatformSubcomponentDefinitionService ]
		ifFalse: [ 
			(theComponent isKindOf: RwSubcomponent)
				ifTrue: [ RowanSubcomponentDefinitionService ]
				ifFalse: [ RowanComponentDefinitionService ] ]
%

category: 'instance creation'
classmethod: RowanComponentDefinitionService
forComponentNamed: componentName projectDefinition: projectDefinition
	| inst subcomponentNames component |
	inst := self new.
	inst name: componentName.
	component := projectDefinition
		componentOrPackageGroupNamed: componentName
		ifAbsent: [ nil ].
	(projectDefinition componentNamed: componentName ifAbsent: [ ^ nil ])
		ifNotNil: [ 
			inst setPrePostDoItsFrom: component.
			inst setConditionFrom: component ].
	subcomponentNames := component
		ifNil: [ Array new ]
		ifNotNil: [ component componentNames ].
	inst
		subcomponentDefinitions:
			(subcomponentNames
				collect: [ :subcomponentName | 
					| subComponent |
					(subComponent := projectDefinition
						componentNamed: subcomponentName
						ifAbsent: [  ])
						ifNotNil: [ 
							(self componentDefinitionServiceClassFor: subComponent)
								forComponentNamed: subcomponentName
								projectDefinition: projectDefinition ] ]) asSet asArray.
	^ inst
%

category: 'rsr'
classmethod: RowanComponentDefinitionService
templateClassName

	^#RowanComponentDefinitionService
%

!		Instance methods for 'RowanComponentDefinitionService'

category: 'other'
method: RowanComponentDefinitionService
addFlattenedHierarchyTo: dictionary
	| sortedSubcomponents |
	sortedSubcomponents := subcomponentDefinitions asSortedCollection:[:x :y | x name < y name]. 
	dictionary at: self put: sortedSubcomponents asArray.
	sortedSubcomponents
		do: [ :componentService | componentService addFlattenedHierarchyTo: dictionary ]
%

category: 'client commands support'
method: RowanComponentDefinitionService
computePackageNames
	self computePackageNames: projectDefinitionService definition
%

category: 'client commands support'
method: RowanComponentDefinitionService
computePackageNames: projectDefinition
	| component  |
	component := projectDefinition
		componentNamed: name
		ifAbsent: [ ^ packageNames := Array new ].
	packageNames := component packageNames
%

category: 'client commands'
method: RowanComponentDefinitionService
packageNames
	self computePackageNames. 
	RowanCommandResult addResult: self.
%

category: 'accessing'
method: RowanComponentDefinitionService
packageNames: object
	packageNames := object
%

category: 'accessing'
method: RowanComponentDefinitionService
postloadDoitName
	^postloadDoitName
%

category: 'accessing'
method: RowanComponentDefinitionService
postloadDoitName: object
	postloadDoitName := object
%

category: 'accessing'
method: RowanComponentDefinitionService
preloadDoitName
	^preloadDoitName
%

category: 'accessing'
method: RowanComponentDefinitionService
preloadDoitName: object
	preloadDoitName := object
%

category: 'accessing'
method: RowanComponentDefinitionService
projectDefinitionService
	^projectDefinitionService
%

category: 'accessing'
method: RowanComponentDefinitionService
projectDefinitionService: object
	projectDefinitionService := object
%

category: 'accessing'
method: RowanComponentDefinitionService
setConditionFrom: component

	"only subcomponents have conditions"
%

category: 'accessing'
method: RowanComponentDefinitionService
setPrePostDoItsFrom: component
	self
		preloadDoitName:
			(component preloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ]).
	self
		postloadDoitName:
			(component postloadDoitName
				ifNotNil: [ :theName | theName -> (component doitDict at: theName) ])
%

category: 'accessing'
method: RowanComponentDefinitionService
subcomponentDefinitions
	^subcomponentDefinitions
%

category: 'accessing'
method: RowanComponentDefinitionService
subcomponentDefinitions: object
	subcomponentDefinitions := object
%

! Class implementation for 'RowanPackageGroupService'

!		Class methods for 'RowanPackageGroupService'

category: 'constants'
classmethod: RowanPackageGroupService
allPackagesGroupName
	^'All Packages'
%

category: 'instance creation'
classmethod: RowanPackageGroupService
forPackageGroupNamed: packageGroupName loadedProject: rwProject
	| inst packageGroup |
	packageGroup := rwProject packageGroupNamed: packageGroupName.
	inst := self new name: packageGroupName.
	inst condition: packageGroup condition.
	inst computeLoadedPackageNamesFor: rwProject.
	inst computePackageServices;
	projectName: rwProject name. 
	^ inst
%

category: 'instance creation'
classmethod: RowanPackageGroupService
forPackageGroupNamed: packageGroupName projectDefinition: projectDefService
	| inst projectDefinition |
	inst := self new name: packageGroupName; projectDefinitionService: projectDefService.
	projectDefinition := projectDefService definition. 
	inst condition: (projectDefinition packageGroupNamed: packageGroupName) condition.
	inst computePackageNames.
	^ inst
%

!		Instance methods for 'RowanPackageGroupService'

category: 'client commands support'
method: RowanPackageGroupService
computeLoadedPackageNamesFor: rwProject
	| packageGroup |
	packageNames := name = self class allPackagesGroupName
		ifTrue: [ Rowan image packageNamesForLoadedProjectNamed: rwProject name ]
		ifFalse: [ 
			packageGroup := rwProject packageGroupNamed: name.
			packageGroup packageNames
				select: [ :packageName | (Rowan image loadedPackageNamed: packageName ifAbsent: [  ]) notNil ] ]
%

category: 'client commands support'
method: RowanPackageGroupService
computePackageNames
	projectDefinitionService := RowanProjectDefinitionService new name: projectDefinitionService name.  "Always get a new one in browser"
	name = self class allPackagesGroupName
		ifTrue: [ 
			packageNames := Rowan image
				packageNamesForLoadedProjectNamed: projectDefinitionService name ]
		ifFalse: [ 
			| loadedPackageNames packageGroup |
			loadedPackageNames := (Rowan image
				loadedProjectNamed: projectDefinitionService name) packageNames.
			packageGroup :=  self projectDefinition packageGroupNamed: name. 
			packageNames := packageGroup packageNames
				select: [ :pkgName | loadedPackageNames includes: pkgName ] ]
%

category: 'updating'
method: RowanPackageGroupService
computePackageServices
	packageServices := packageNames
		collect: [ :packageName | (RowanPackageService forPackageNamed: packageName) update ]
%

category: 'accessing'
method: RowanPackageGroupService
condition
	^condition
%

category: 'accessing'
method: RowanPackageGroupService
condition: object
	condition := object
%

category: 'replication'
method: RowanPackageGroupService
excludedInstVars

	^super excludedInstVars, #( #projectDefinition)
%

category: 'accessing'
method: RowanPackageGroupService
projectDefinition
	^projectDefinitionService initializeDefinitionOop definition
%

category: 'accessing'
method: RowanPackageGroupService
projectName
	^projectName
%

category: 'accessing'
method: RowanPackageGroupService
projectName: object
	projectName := object
%

category: 'updating'
method: RowanPackageGroupService
update
	| rwProject theProjectName |
	theProjectName := projectDefinitionService
		ifNil: [ projectName ]
		ifNotNil: [ projectDefinitionService name ].
	rwProject := Rowan projectNamed: theProjectName.
	self computeLoadedPackageNamesFor: rwProject.
	self computePackageServices.
	wasUpdated := true.
	RowanCommandResult addResult: self.
	shouldUpdate := false	"don't perform the secondary update"
%

! Class implementation for 'RowanPackageGroupDefinitionService'

!		Instance methods for 'RowanPackageGroupDefinitionService'

category: 'client commands'
method: RowanPackageGroupDefinitionService
computePackageNames
	name = self class allPackagesGroupName
		ifTrue: [ packageNames := self projectDefinition packageNames ]
		ifFalse: [ 
			| packageGroup |
			packageGroup := self projectDefinition packageGroupNamed: name.
			packageNames := packageGroup packageNames ]
%

category: 'packages'
method: RowanPackageGroupDefinitionService
computePackageServices
	packageServices := packageNames
		collect: [ :packageName | RowanPackageDefinitionService new name: packageName ]
%

! Class implementation for 'RowanSubcomponentDefinitionService'

!		Instance methods for 'RowanSubcomponentDefinitionService'

category: 'accessing'
method: RowanSubcomponentDefinitionService
setConditionFrom: component
	condition := component condition
%

! Class implementation for 'RowanPackageDefinitionService'

!		Class methods for 'RowanPackageDefinitionService'

category: 'rsr'
classmethod: RowanPackageDefinitionService
templateClassName

	^#RowanPackageDefinitionService
%

! Class implementation for 'RowanProjectDefinitionService'

!		Class methods for 'RowanProjectDefinitionService'

category: 'rsr'
classmethod: RowanProjectDefinitionService
templateClassName

	^#RowanProjectDefinitionService
%

!		Instance methods for 'RowanProjectDefinitionService'

category: 'client commands'
method: RowanProjectDefinitionService
addComponent: componentName
	self definition addLoadComponentNamed: componentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
addPackageGroupNamed: aComponentName
	"for adding new package groups. condition and comment added later by the user"

	self definition
		addPackageGroupNamed: aComponentName
		condition: String new
		comment: String new
%

category: 'client commands'
method: RowanProjectDefinitionService
addPackageGroupNamed: packageGroupName condition: condition comment: aString
	(self definition packageGroupNamed: packageGroupName) condition: condition.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
addPackageNamed: packageName toComponentNamed: componentName
	| componentDefinitionService |
	self definition addPackageNamed: packageName toComponentNamed: componentName.
	componentDefinitionService := (self
		componentDefinitionServiceClassFor: componentName)
		forComponentNamed: componentName
		projectDefinition: self definition.
	componentDefinitionService computePackageNames: self definition.
	RowanCommandResult addResult: componentDefinitionService
%

category: 'client commands'
method: RowanProjectDefinitionService
addPackagesNamed: packageNames toPackageGroupNamed: packageGroupName
	| packageGroupService |
	(self definition packageGroupNamed: packageGroupName) addPackageNames: packageNames.
	packageGroupService := self packageGroupServiceClass forPackageGroupNamed: packageGroupName projectDefinition: self.
	RowanCommandResult addResult: packageGroupService
%

category: 'client commands'
method: RowanProjectDefinitionService
addPostloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName
	self definition
		addPostloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
addPreloadDoitName: doitName withSource: doitSource toComponentNamed: aComponentName
	self definition
		addPreloadDoitName: doitName
		withSource: doitSource
		toComponentNamed: aComponentName.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanProjectDefinitionService
addSubcomponentNamed: componentName toComponentNamed: toComponentName
	self rowanFixMe.	"What should the default condition be?"
	self definition
		addSubcomponentNamed: componentName
		condition: 'common'
		toComponentNamed: toComponentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
%

category: 'constants'
method: RowanProjectDefinitionService
allUsersName

	^self definition loadSpecification _gemstoneAllUsersName
%

category: 'accessing'
method: RowanProjectDefinitionService
comment: projectComment
	self definition loadSpecification comment: projectComment
%

category: 'accessing'
method: RowanProjectDefinitionService
componentDefinitions
	^componentDefinitions
%

category: 'accessing'
method: RowanProjectDefinitionService
componentDefinitions: object
	componentDefinitions := object
%

category: 'accessing'
method: RowanProjectDefinitionService
componentDefinitionServiceClassFor: componentName
	| theComponent |
	theComponent := self definition componentNamed: componentName.
	^ RowanComponentDefinitionService
		componentDefinitionServiceClassFor: theComponent
%

category: 'client commands'
method: RowanProjectDefinitionService
componentDefinitionServicesFor: projectDefinition
	| componentDictionary |
	componentDictionary := Dictionary new.
	componentDictionary
		at: #'nil'
		put:
			(projectDefinition componentNames
				collect: [ :componentName | 
					| componentDefinitionService |
					componentDefinitionService := (self
						componentDefinitionServiceClassFor: componentName)
						forComponentNamed: componentName
						projectDefinition: projectDefinition.
					componentDefinitionService
						ifNotNil: [ 
							componentDefinitionService addFlattenedHierarchyTo: componentDictionary.
							componentDefinitionService ] ]) asSet asArray.	"eliminate nils"
	^ componentDictionary
%

category: 'accessing'
method: RowanProjectDefinitionService
definition

	^Object _objectForOop: definitionOop
%

category: 'client commands'
method: RowanProjectDefinitionService
editInWindow: handle
	self initializeDefinitionOop.
	handle
		ifNotNil: [ 
			RowanBrowserService new saveRootObject: self asOop windowHandle: handle.
			RowanBrowserService new saveRootObject: definitionOop windowHandle: handle ].
	RowanCommandResult addResult: self
%

category: 'replication'
method: RowanProjectDefinitionService
excludedInstVars
	^ super excludedInstVars , #(#'definition')
%

category: 'client commands'
method: RowanProjectDefinitionService
export

	self definition resolveProject export
%

category: 'client commands'
method: RowanProjectDefinitionService
exportLoadSpecification
	self definition resolveProject exportLoadSpecification
%

category: 'initialization'
method: RowanProjectDefinitionService
initialize

	super initialize. 
	showLoadedPackageGroupsOnly := true.
%

category: 'initialization'
method: RowanProjectDefinitionService
initializeComponentDefinitions
	self
		componentDefinitions: (self componentDefinitionServicesFor: self definition)
%

category: 'client commands'
method: RowanProjectDefinitionService
initializeDefinitionOop
	definitionOop
		ifNil: [ 
			definition := (Rowan projectNamed: name) defined.
			definitionOop := definition asOop ]
%

category: 'initialization'
method: RowanProjectDefinitionService
initializePackageGroups
	definitionOop ifNil: [self initializeDefinitionOop].
	self
		packageGroups:
			(self definition packageGroupNames
				collect: [ :theName | 
					self packageGroupServiceClass
						forPackageGroupNamed: theName
						projectDefinition: self ]).
	wasUpdated := true.
%

category: 'client commands'
method: RowanProjectDefinitionService
movePackageNamed: aPackageName toComponentNamed: aComponentName
	self definition movePackageNamed: aPackageName toComponentNamed: aComponentName
%

category: 'accessing'
method: RowanProjectDefinitionService
packageDefinitions
	^packageDefinitions
%

category: 'accessing'
method: RowanProjectDefinitionService
packageDefinitions: object
	packageDefinitions := object
%

category: 'accessing'
method: RowanProjectDefinitionService
packageGroups
	^packageGroups
%

category: 'accessing'
method: RowanProjectDefinitionService
packageGroups: object
	packageGroups := object
%

category: 'accessing'
method: RowanProjectDefinitionService
packageGroupServiceClass
	^ showLoadedPackageGroupsOnly
		ifTrue: [ RowanPackageGroupService ]
		ifFalse: [ RowanPackageGroupDefinitionService ]
%

category: 'client commands'
method: RowanProjectDefinitionService
removeComponentNamed: componentName
	self definition removeComponentNamed: componentName.
	self initializeComponentDefinitions.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
removePackageGroupNamed: aComponentName
	self definition removePackageGroupNamed: aComponentName
%

category: 'client commands'
method: RowanProjectDefinitionService
removePackageGroupsNamed: packageGroupNames
	packageGroupNames
		do: [ :packageGroupName | self definition removePackageGroupNamed: packageGroupName].
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanProjectDefinitionService
removePackagesFromProjectDefinition: packageNames
	packageNames
		do: [ :packageName | self definition removePackageNamed: packageName ]
%

category: 'client commands'
method: RowanProjectDefinitionService
removePackagesNamed: packageNames fromPackageGroupNamed: packageGroupName
	packageNames
		do: [ :packageName | 
			(self definition packageGroupNamed: packageGroupName)
				removePackageNamed: packageName ]
%

category: 'client commands'
method: RowanProjectDefinitionService
renameComponent: aComponentPath to: aComponentName
	self definition renameComponentNamed: aComponentPath to: aComponentName.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
renamePackageGroup: packageGroupName to: newPackageGroupName
	self definition renamePackageGroupNamed: packageGroupName to: newPackageGroupName.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
renamePackageNamed: packageName to: newPackageName
	self definition renamePackageNamed: packageName to: newPackageName.
	self update. 
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectDefinitionService
setConditionalAttributes: array
	self definition customConditionalAttributes: array
%

category: 'client commands'
method: RowanProjectDefinitionService
setLoadSpecProperty: property to: value
	self definition loadSpecification
		perform: (property , ':') asSymbol
		with: value
%

category: 'client commands'
method: RowanProjectDefinitionService
setPlatformProperty: property to: value
	property = #'defaultMethodEnv'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self definition loadSpecification gemstoneSetDefaultSymbolDictNameTo: value ].

	property = #'useSessionMethods'
		ifTrue: [ 
			self definition loadSpecification
				gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
%

category: 'client commands'
method: RowanProjectDefinitionService
subcomponentNamed: subcomponentName condition: condition
	| subcomponent |
	subcomponent := self definition componentNamed: subcomponentName.
	subcomponent condition: condition.
%

category: 'updating'
method: RowanProjectDefinitionService
update
	self initializeComponentDefinitions.
	self initializePackageGroups. 
	self updateLoadSpecService.
	comment := self definition loadSpecification comment. 
	conditionalAttributes := self definition customConditionalAttributes asOrderedCollection. 
	self updatePlatformProperties. 
	RowanCommandResult addResult: self
%

category: 'updating'
method: RowanProjectDefinitionService
updateLoadSpecService
	specService := RowanLoadSpecService new
		initialize: self definition loadSpecification asOop.
	specService removeHiddenAttributes.
%

category: 'updating'
method: RowanProjectDefinitionService
updatePlatformProperties
	platformProperties := Dictionary new.
	platformProperties
		at: #'defaultMethodEnv'
		put:
			(self definition loadSpecification
				gemstoneDefaultMethodEnvForUser: self allUsersName).
	platformProperties
		at: #'defaultSymbolDict'
		put: self definition loadSpecification gemstoneDefaultSymbolDictName.
	platformProperties
		at: #'useSessionMethods'
		put:
			(self definition loadSpecification
				gemstoneDefaultUseSessionMethodsForExtensionsForUser: self allUsersName)
%

! Class implementation for 'RowanDictionaryService'

!		Class methods for 'RowanDictionaryService'

category: 'rsr'
classmethod: RowanDictionaryService
templateClassName

	^#RowanDictionaryService
%

!		Instance methods for 'RowanDictionaryService'

category: 'accessing'
method: RowanDictionaryService
classes
	^classes
%

category: 'accessing'
method: RowanDictionaryService
classes: object
	classes := object
%

category: 'client commands'
method: RowanDictionaryService
classHierarchy
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	"reuse behavior in package service for now" 
	hierarchyServices := (RowanPackageService new classes: classes) classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
%

category: 'Updating'
method: RowanDictionaryService
defaultTemplate: newValue
	defaultTemplate := newValue
%

category: 'command support'
method: RowanDictionaryService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: nil packageName: nil
%

category: 'initialization'
method: RowanDictionaryService
initialize

	self setDefaultTemplate
%

category: 'client commands'
method: RowanDictionaryService
insertAt: index

	| theDictionary |
	theDictionary := SymbolDictionary new. 
	theDictionary at: name asSymbol put: theDictionary. 
	System myUserProfile insertDictionary: theDictionary at: index. 
	RowanBrowserService new updateDictionaries.
%

category: 'testing'
method: RowanDictionaryService
isDictionaryService
  ^ true
%

category: 'accessing'
method: RowanDictionaryService
name
	^name
%

category: 'accessing'
method: RowanDictionaryService
name: object
	name := object
%

category: 'client commands'
method: RowanDictionaryService
removeClass: classService
	self removeClassNamed: classService name. 
	self setDefaultTemplate.
	classService updateType: #removedClass:.
	RowanCommandResult addResult: classService
%

category: 'client commands'
method: RowanDictionaryService
removeClassNamed: className
	[ self browserTool removeClassNamed: className ]
		on: RwPerformingUnpackagedEditNotification
		do: [ :ex | ex resume ]
%

category: 'client commands'
method: RowanDictionaryService
removeGlobalNamed: symbol
	| dictionary |
	dictionary := (System myUserProfile resolveSymbol: name asSymbol) value.
	dictionary ifNotNil: [
		dictionary removeKey: symbol ifAbsent:[]].
	self update.
%

category: 'perform'
method: RowanDictionaryService
servicePerform: symbol withArguments: collection
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
%

category: 'client commands'
method: RowanDictionaryService
setDefaultTemplate

	defaultTemplate := self genericClassCreationTemplate.
%

category: 'Updating'
method: RowanDictionaryService
testClasses

	self update. 
	testClasses := Set new.
	self classes
		do: [ :classService | 
			| cls |
			cls := classService theClass.
			(cls inheritsFrom: TestCase)
				ifTrue: [ 
					cls isAbstract
						ifFalse: [ testClasses add: classService update] ] ].
	updateType := #'testClasses:browser:'.
	testClasses := testClasses asArray.
	RowanCommandResult addResult: self
%

category: 'updates'
method: RowanDictionaryService
update
	| dictionary sorted |
	super update.
	classes := Array new.
	sorted := SortedCollection sortBlock: [ :x :y | x first < y first ].
	dictionary := Rowan image symbolList objectNamed: name.
	dictionary ifNil: [ ^ self ].
	(dictionary isKindOf: SymbolDictionary)
		ifFalse: [ ^ self ].
	classCategories := Set new. 
	dictionary
		keysAndValuesDo: [ :key :value | 
			value isClass
				ifTrue: [ 
					| classService |
					classService := RowanClassService new name: key asString.
					classService versions: value classHistory size.
					classService version: (value classHistory indexOf: value).
					classService setIsTestCase.
					classService classCategory: value category.  
					classes add: classService.
					classCategories add: value category ]
				ifFalse: [ 
					| printString theKey |
					printString := [ 
					value printString charSize > 1
						ifTrue: [ '<<unprintable string. charSize > 1>>' ]
						ifFalse: [ value printString ] ]
						on: Error
						do: [ :ex | 'unprintable string. Error - <' , ex printString , '>' ].
					key charSize = 1
						ifTrue: [ theKey := key ]
						ifFalse: [ theKey := '<<unprintable string. charSize > 1>>' ].
					sorted
						add:
							(Array
								with: name , '.' , theKey
								with: value class name
								with: value asOop
								with: printString) ] ].
	globals := sorted asArray.
	classCategories := classCategories  asSortedCollection asArray. 
	RowanCommandResult addResult: self
%

category: 'testing'
method: RowanDictionaryService
wasDeleted
  ^ (Rowan globalNamed: name) isNil
%

! Class implementation for 'RowanFrameService'

!		Class methods for 'RowanFrameService'

category: 'other'
classmethod: RowanFrameService
process: aGsProcess level: anInteger organizer: aClassOrganizer

	^self basicNew
		initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer;
		yourself
%

category: 'rsr'
classmethod: RowanFrameService
templateClassName

	^#RowanFrameService
%

!		Instance methods for 'RowanFrameService'

category: 'other'
method: RowanFrameService
initializeProcess: aGsProcess level: anInteger organizer: aClassOrganizer
  "In 3.2.15 the server does some whacky things with IP, stepPoint, and nested methods.
	See http://kermit.gemtalksystems.com/bug?bug=45553 --JGF"

  | frameData gsNMethod homeMethodService |
  frameData := aGsProcess _frameContentsAt: anInteger.
  frameData isNil
    ifTrue: [ ^ self	"not sure if bad frame data is a 3.2.15 bug or not" ].
  oop := (frameData at: 8) asOop.
  gsNMethod := frameData at: 1.
  label := aGsProcess _reportAt: anInteger.
  method := RowanMethodService
    forGsNMethod: gsNMethod
    organizer: aClassOrganizer.
  homeMethodService := RowanMethodService
    forGsNMethod: gsNMethod homeMethod
    organizer: aClassOrganizer.
  method breakPoints: homeMethodService breakPoints.
  homeMethodSelector := gsNMethod homeMethod selector.
  homeMethodClassName := gsNMethod homeMethod inClass
    ifNotNil: [ :cls | 
      | className |
      className := cls name asString.
      classIsResolvable := (Rowan image
        resolveClassNamed: cls theNonMetaClass name asString) isNil not.
      className ].
  stepPoint :=  gsNMethod homeMethod _stepPointForMeth: gsNMethod ip: (frameData at: 2).
  vars := self varsFor: frameData
%

category: 'accessing'
method: RowanFrameService
label: object
	label := object
%

category: 'other'
method: RowanFrameService
method

	^method
%

category: 'other'
method: RowanFrameService
method: aMethodService

	method := aMethodService
%

category: 'other'
method: RowanFrameService
vars

	^vars
%

category: 'other'
method: RowanFrameService
varsFor: anArray
	| keys list receiver values |
	receiver := anArray at: 10.
	values := OrderedCollection new.
	(Reflection classOf: receiver) name == #'ClientForwarder'
		ifTrue: [ 
			keys := OrderedCollection with: 'clientObject'.
			values add: receiver clientObject.
			receiver := '[aClientForwarder(' , (self oopOf: receiver) printString , ')]'.
			keys addAll: (anArray at: 9).
			keys := keys reject: [ :each | each first == $. ] ]
		ifFalse: [ 
			((receiver isKindOf: BlockClosure) or: [ receiver isKindOf: Class ])
				ifTrue: [ 
					keys := OrderedCollection new.
					keys addAll: (anArray at: 9).
					keys := keys reject: [ :each | each first == $. ] ]
				ifFalse: [ 
					keys := receiver class allInstVarNames asOrderedCollection
						collect: [ :each | '-' , each ].
					1 to: keys size do: [ :i | values add: (receiver instVarAt: i) ].
					keys
						addAll:
							(receiver dynamicInstanceVariables asOrderedCollection
								collect: [ :each | '$' , each asString ]).
					receiver dynamicInstanceVariables
						do: [ :symbol | values add: (receiver dynamicInstVarAt: symbol) ].
					keys addAll: (anArray at: 9).
					keys := keys reject: [ :key | key first == $. ].
					keys := keys collect: [ :key | key copyReplaceChar: $$ with: $. ] ] ].

	keys addFirst: #'receiver'.
	values addFirst: receiver.
	values
		addAll:
			(anArray size >= 11
				ifTrue: [ anArray copyFrom: 11 to: anArray size ]
				ifFalse: [ #() ]).
	list := Array new.
	1 to: (keys size min: values size) do: [ :i | 
		| theOop key value valueClass |
		key := keys at: i.
		value := values at: i.
		valueClass := value class.
		theOop := value asOop.
		value := [ value printString ]
			on: Error
			do: [ :ex | 
				ex
					return:
						'(' , value class name , ' printString error: ' , ex description , ')' ].
		value size > 500
			ifTrue: [ value := (value copyFrom: 1 to: 500) , '...' ].
		list
			add:
				(RowanVariableServiceServer
					oop: theOop
					key: key
					value: value
					className: valueClass name asString) ].
	^ list
%

! Class implementation for 'RowanFrameServiceServer'

!		Class methods for 'RowanFrameServiceServer'

category: 'ston'
classmethod: RowanFrameServiceServer
stonName
	"JfD does not know about this class yet"
	^ 'RowanFrameService'
%

! Class implementation for 'RowanInspectorService'

!		Class methods for 'RowanInspectorService'

category: 'rsr'
classmethod: RowanInspectorService
templateClassName

	^#RowanInspectorService
%

!		Instance methods for 'RowanInspectorService'

category: 'command support'
method: RowanInspectorService
addDynamicInstVars: anObject
	| dynamic dynamicSize |
	dynamic := anObject dynamicInstanceVariables.
	dynamicSize := dynamic size.
	1 to: dynamicSize do: [ :i | 
		objects
			add:
				('--' , (dynamic at: i) asUnicodeString)
					-> (Reflection oopOf: (anObject dynamicInstVarAt: (dynamic at: i))) ]
%

category: 'command support'
method: RowanInspectorService
addFirstIndexedVars: anObject
  | max |
  self setIndexedSize: anObject.
  max := indexedSize min: maxIndexedVars.
  objects addAll: (self safeVariablesFrom: 1 to: max).
  visibleIndices := max
%

category: 'command support'
method: RowanInspectorService
addInstVars: anObject
	| namedSize |
	instVarNames := anObject class allInstVarNames.
	namedSize := instVarNames size.
	1 to: namedSize do: [ :i | 
		objects
			add:
				(self instVarPrefix , (instVarNames at: i) asUnicodeString)
					-> (Reflection oopOf: (anObject instVarAt: i)) ]
%

category: 'client commands'
method: RowanInspectorService
addKey: keyString
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := keyString
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: newObject ifAbsentPut: nil.
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop
%

category: 'client commands'
method: RowanInspectorService
addObject: string after: index
  | newObject theObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject class isIndexable
    ifTrue: [ 
      | insertionCollection |
      insertionCollection := (theObject isKindOf: CharacterCollection)
        ifTrue: [ newObject ]
        ifFalse: [ Array with: newObject ].
      theObject insertAll: insertionCollection at: index + 1 ]
    ifFalse: [ theObject add: newObject ].
  selectionOop := newObject asOop.
  objects := OrderedCollection new.
  isOop := true.
  self inspect: oop.
  theObject class isIndexable
    ifTrue: [ self inspect: oop from: visibleIndices to: visibleIndices + 1 ]
%

category: 'client commands'
method: RowanInspectorService
executeThenInspect: string context: anOop inWindow: handle
  | answer |
  answer := RowanAnsweringService new basicExec: string context: anOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
%

category: 'client commands'
method: RowanInspectorService
executeThenInspect: string inFrame: level process: processOop context: contextOop inWindow: handle
  | answer |
  answer := RowanAnsweringService new exec: string inFrame: level ofProcess: processOop context: contextOop.
  answer key
    ifTrue: [ 
      RowanBrowserService new saveRootObject: answer value windowHandle: handle.
      self inspect: answer value ]
    ifFalse: [ 
      compileErrorArray := answer value.
      RowanCommandResult addResult: self ]
%

category: 'initialization'
method: RowanInspectorService
initialize
  super initialize.
  objects := OrderedCollection new.
  instVarsAreRemovable := false.
  isUnordered := false
%

category: 'client commands'
method: RowanInspectorService
inspect: oopOrObject
  ^ self inspect: oopOrObject inWindow: nil
%

category: 'client commands'
method: RowanInspectorService
inspect: anOop from: indexStart to: indexStop
  | anObject stop |
  anObject := Object _objectForOop: anOop.
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
%

category: 'command support'
method: RowanInspectorService
inspect: oopOrObject inWindow: handle
	| anObject |
	self setOopFrom: oopOrObject.
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: oop windowHandle: handle ].
	anObject := Object _objectForOop: oop.
	isVariable := anObject class isVariable.
	(self isClientForwarder: anObject)
		ifTrue: [ ^ self inspectClientForwarder: anObject ].
	className := anObject class name.
	myself := 'self' -> (self selfPrintString: anObject).
	(anObject isKindOf: Dictionary superclass)
		ifTrue: [ ^ objects addAll: (self inspectDictionary: anObject from: 1 to: maxIndexedVars) ].
	self addInstVars: anObject.
	self addDynamicInstVars: anObject.
	self addFirstIndexedVars: anObject.
	isStringObject := anObject class canUnderstand: #asByteArray.
	nextIndices := Array new. 
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanInspectorService
inspectBytes: oopOrObject

  ^ self inspect: oopOrObject asByteArray inWindow: nil
%

category: 'client commands'
method: RowanInspectorService
inspectBytes: oopOrObject inWindow: handle
	^ self
		inspect: (Object _objectForOop: oopOrObject) asByteArray asOop
		inWindow: handle
%

category: 'command support'
method: RowanInspectorService
inspectClientForwarder: anObject

	oop := Reflection oopOf: anObject. 
	myself := 'self' -> anObject clientObject printString. 
	RowanCommandResult addResult: self.
%

category: 'command support'
method: RowanInspectorService
inspectDictionary: aDictionary from: start to: stop
  | sortedKeys service |
  isDictionary := true.
  instVarsAreRemovable := true.
  self addInstVars: aDictionary.
  self addDynamicInstVars: aDictionary.
  service := RowanAnsweringService new.
  sortedKeys := (aDictionary keys
    collect: [ :key | 
      (service basicPrintStringOfObject: key toMaxSize: self maxPrintStringSize)
        -> key ]) asSortedCollection.
  visibleIndices := (sortedKeys size min: stop) min: self maxVariables.
  nextIndices := OrderedCollection new.
  (sortedKeys copyFrom: start to: visibleIndices)
    do: [ :assoc | 
      nextIndices
        add:
          assoc key
            ->
              (Array
                with: (Reflection oopOf: assoc value)
                with: (Reflection oopOf: (aDictionary at: assoc value))) ].
  indexedSize := aDictionary size.
  RowanCommandResult addResult: self.
  ^ nextIndices
%

category: 'constants'
method: RowanInspectorService
instVarPrefix
	^ '-'
%

category: 'testing'
method: RowanInspectorService
isClientForwarder: anObject

	^(Reflection classOf: anObject) name == #'ClientForwarder'
%

category: 'accessing'
method: RowanInspectorService
isOop
	^isOop
%

category: 'accessing'
method: RowanInspectorService
isOop: object
	isOop := object
%

category: 'accessing'
method: RowanInspectorService
maxIndexedVars
	^maxIndexedVars
%

category: 'accessing'
method: RowanInspectorService
maxIndexedVars: object
	maxIndexedVars := object
%

category: 'constants'
method: RowanInspectorService
maxPrintStringSize
  "currently matches Jadeite client"

  ^ 100000
%

category: 'constants'
method: RowanInspectorService
maxVariables
	"See Jadeite issue #668. Currently, 
	Dolphin doesn't seem to be able to 
	handle more than about 65k elements
	in a list
	https://github.com/GemTalk/Jadeite/issues/668#issuecomment-566795924" 
	
	^65000
%

category: 'client commands'
method: RowanInspectorService
nextIndexedVarsFrom: indexStart to: indexStop
  | stop anObject |
  anObject := Object _objectForOop: oop.
  (anObject isKindOf: Dictionary superclass)
    ifTrue: [ 
      ^ objects
        addAll:
          (self inspectDictionary: anObject from: indexStart to: indexStop) ].
  stop := (indexStop min: indexedSize) min: self maxVariables.
  nextIndices := self safeVariablesFrom: indexStart to: stop.
  visibleIndices := stop.
  RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanInspectorService
oop: anInteger

	oop := anInteger
%

category: 'private'
method: RowanInspectorService
reinspect: theObject
  | formerlyVisible |
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  (visibleIndices > 0 and: [ visibleIndices < formerlyVisible ])
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'client commands'
method: RowanInspectorService
removeDynamicInstVars: dynamicInstVarNames
  | theObject |
  theObject := Object _objectForOop: oop.
  dynamicInstVarNames
    do: [ :dynamicInstVar | theObject removeDynamicInstVar: dynamicInstVar ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeIndexedInstVarsAt: indices
  | theObject removalObjects |
  theObject := Object _objectForOop: oop.
  removalObjects := indices collect: [ :index | theObject at: index ].
  removalObjects do: [ :removalObject | theObject remove: removalObject ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeKeys: keyOops
  | theObject removalKey |
  theObject := Object _objectForOop: oop.
  keyOops
    do: [ :keyOop | 
      removalKey := Object _objectForOop: keyOop.
      theObject removeKey: removalKey.
      visibleIndices := visibleIndices - 1 max: 0 ].
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
removeOop: elementOop
  | theObject removal |
  theObject := Object _objectForOop: oop.
  removal := Object _objectForOop: elementOop.
  theObject remove: removal.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
replaceElement: oldOop with: string
  | theObject newObject formerlyVisible oldObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  oldObject := Object _objectForOop: oldOop.
  theObject remove: oldObject.
  theObject add: newObject.
  selectionOop := newObject asOop.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'command support'
method: RowanInspectorService
safeVariablesFrom: indexStart to: indexStop
  [ ^ self variablesFrom: indexStart to: indexStop ]
    on: Error
    do: [ :ex | 
      indexedSize := 0.
      instVarsAreRemovable := false.
      visibleIndices := 0 to: 0.
      nextIndices := 0 to: 0.
      isVariable := false.
      statusText := 'Error getting object contents: ' , ex description.
      ^ OrderedCollection new ]
%

category: 'client commands'
method: RowanInspectorService
save: string dynamicInstVar: instVarName
  | theObject newObject |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject dynamicInstVarAt: instVarName put: newObject.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
save: string indexedVarAt: index
  | theObject newObject formerlyVisible |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  theObject at: index put: newObject.
  formerlyVisible := visibleIndices.
  objects := OrderedCollection new.
  isOop := false.
  self inspect: theObject.
  visibleIndices > 0
    ifTrue: [ self inspect: oop from: visibleIndices to: formerlyVisible ]
%

category: 'client commands'
method: RowanInspectorService
save: string namedInstVar: instVarName
  | theObject newObject instVarIndex |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  instVarIndex := theObject class allInstVarNames indexOf: instVarName.
  theObject instVarAt: instVarIndex put: newObject.
  self reinspect: theObject
%

category: 'client commands'
method: RowanInspectorService
saveKey: keyOop value: string
  | theObject newObject key |
  theObject := Object _objectForOop: oop.
  newObject := string
    evaluateInContext: theObject
    symbolList: Rowan image symbolList.
  key := Object _objectForOop: keyOop.
  theObject at: key put: newObject.
  self reinspect: theObject
%

category: 'printing'
method: RowanInspectorService
selfPrintString: anObject
  ^ [ anObject printString asUnicodeString ]
    on: Error
    do: [ :ex | 
      | printString |
      printString := 'Error printing object with oop ' , oop printString
        , '. Error text: ' , ex printString ]
%

category: 'private'
method: RowanInspectorService
setIndexedSize: anObject
  indexedSize := anObject class isVariable
    ifFalse: [ 0 ]
    ifTrue: [ 
      instVarsAreRemovable := true.
      (self shouldReferenceBySize: anObject)
        ifTrue: [ anObject size ]
        ifFalse: [ anObject _primitiveSize - anObject class allInstVarNames size ] ]
%

category: 'private'
method: RowanInspectorService
setOopFrom: oopOrObject
  oop := isOop
    ifTrue: [ oopOrObject ]
    ifFalse: [ Reflection oopOf: oopOrObject ]
%

category: 'testing'
method: RowanInspectorService
shouldReferenceBySize: anObject

	(anObject isKindOf: UnorderedCollection) ifTrue:[^true].
	(anObject isKindOf: MultiByteString) ifTrue:[^true]. 
	^false
%

category: 'command support'
method: RowanInspectorService
variablesFrom: indexStart to: indexStop
  | namedSize anObject indexedVars |
  indexedVars := OrderedCollection new.
  anObject := Object _objectForOop: oop.
  namedSize := anObject class allInstVarNames size.
  isUnordered := anObject isKindOf: UnorderedCollection.
  isUnordered
    ifTrue: [ 
      | sorted service |
      service := RowanAnsweringService new.
      sorted := (anObject
        collect: [ :var | 
          (service basicPrintStringOfObject: var toMaxSize: self maxPrintStringSize)
            -> (Reflection oopOf: var) ]) asSortedCollection: [:x :y | x key < y key]. 
      indexStart to: indexStop do: [ :i | indexedVars add: (sorted at: i) ] ]
    ifFalse: [ indexStart to: indexStop do: [ :i | indexedVars add: i printString -> (Reflection oopOf: (anObject at: i)) ] ].
  ^ indexedVars
%

! Class implementation for 'RowanLoadSpecService'

!		Class methods for 'RowanLoadSpecService'

category: 'rsr'
classmethod: RowanLoadSpecService
templateClassName

	^#RowanLoadSpecService
%

!		Instance methods for 'RowanLoadSpecService'

category: 'command support'
method: RowanLoadSpecService
addInstVars: anObject
	"coerce the oops into strings for display in the browsers"

	objects := Array new.
	super addInstVars: anObject.
	1 to: objects size do: [ :index | 
		objects
			at: index
			put:
				(objects at: index) key
					->
						((Object _objectForOop: (objects at: index) value) ifNil: [ String new ])
							asString ]
%

category: 'updating'
method: RowanLoadSpecService
allUsersName

	^self loadSpec _gemstoneAllUsersName
%

category: 'client commands'
method: RowanLoadSpecService
editInWindow: handle
	loadSpecPath
		ifNotNil: [ :path | 
			[ loadSpecOop := (RwSpecification fromFile: loadSpecPath) asOop ]
				on: Error
				do: [ :ex | self inform: 'Not a valid load spec file!'. ^self ] ].
	handle
		ifNotNil: [ RowanBrowserService new saveRootObject: loadSpecOop windowHandle: handle ].
	self update.
	RowanCommandResult addResult: self
%

category: 'replication'
method: RowanLoadSpecService
excludedInstVars
	^ super excludedInstVars , #(#'loadSpec')
%

category: 'client commands'
method: RowanLoadSpecService
export
	| path directory |
	path := Path from: loadSpecPath.
	directory := path parent fullName. 
	self loadSpec exportTo:  (FileReference / directory)
%

category: 'constants'
method: RowanLoadSpecService
hiddenAttributes
	^ #('customConditionalAttributes' 'comment' 'componentNames' 'platformProperties' 'mercurialUrl' 'svnUrl' 'projectsHome' 'repositoryResolutionPolicy')
%

category: 'initialization'
method: RowanLoadSpecService
initialize
	super initialize.
	isOop := true.
	maxIndexedVars := 0.
	conditionalAttributes := Array new.
	comment := String new.
	platformProperties := Dictionary new.
%

category: 'initialization'
method: RowanLoadSpecService
initialize: anOop
	loadSpecOop := anOop. 
	isOop := true. 
	maxIndexedVars := 0.
	conditionalAttributes := Array new. 
	comment := String new. 
	super inspect: anOop
%

category: 'constants'
method: RowanLoadSpecService
instVarPrefix
	^ String new
%

category: 'client commands'
method: RowanLoadSpecService
load
	self loadSpec load
%

category: 'accessing'
method: RowanLoadSpecService
loadSpec

	^loadSpec ifNil: [loadSpec := Object _objectForOop: loadSpecOop]
%

category: 'accessing'
method: RowanLoadSpecService
removeHiddenAttributes
	self hiddenAttributes
		do: [ :attribute | objects remove: (objects detect: [ :assoc | assoc key = attribute ]) ]
%

category: 'client commands'
method: RowanLoadSpecService
setComment: aString
	self loadSpec comment: aString
%

category: 'client commands'
method: RowanLoadSpecService
setConditionalAttributes: anArray
	self loadSpec customConditionalAttributes: anArray
%

category: 'client commands'
method: RowanLoadSpecService
setLoadSpecProperty: property to: value
	self loadSpec
		perform: (property , ':') asSymbol
		with: value
%

category: 'client commands'
method: RowanLoadSpecService
setPlatformProperty: property to: value
	property = #'defaultMethodEnv'
		ifTrue: [ self loadSpec gemstoneSetDefaultMethodEnvTo: value ].
	property = #'defaultSymbolDict'
		ifTrue: [ self loadSpec gemstoneSetDefaultSymbolDictNameTo: value ].
	property = #'useSessionMethods'
		ifTrue: [ self loadSpec gemstoneSetDefaultUseSessionMethodsForExtensionsTo: value ]
%

category: 'updating'
method: RowanLoadSpecService
update
	loadSpecOop
		ifNil: [ loadSpecOop := (RwSpecification fromFile: loadSpecPath) asOop ].
	self updateSettableAttributes.
	comment := self loadSpec comment.
	componentNames := self loadSpec componentNames. 
	conditionalAttributes := self loadSpec customConditionalAttributes
		asOrderedCollection.
	self updatePlatformProperties.
	RowanCommandResult addResult: self
%

category: 'updating'
method: RowanLoadSpecService
updatePlatformProperties
	platformProperties := Dictionary new.
	platformProperties
		at: #'defaultMethodEnv'
		put: (self loadSpec gemstoneDefaultMethodEnvForUser: self allUsersName).
	platformProperties
		at: #'defaultSymbolDict'
		put: self loadSpec gemstoneDefaultSymbolDictName.
	platformProperties
		at: #'useSessionMethods'
		put:
			(self loadSpec
				gemstoneDefaultUseSessionMethodsForExtensionsForUser: self allUsersName)
%

category: 'updating'
method: RowanLoadSpecService
updateSettableAttributes
	self initialize: loadSpecOop.
	self removeHiddenAttributes
%

! Class implementation for 'RowanLoggingService'

!		Class methods for 'RowanLoggingService'

category: 'accessing'
classmethod: RowanLoggingService
current

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanLoggingService ifAbsentPut: [RowanLoggingService new]
%

category: 'accessing'
classmethod: RowanLoggingService
current: anObject

	SessionTemps current at: #rowanLoggingService put: anObject
%

!		Instance methods for 'RowanLoggingService'

category: 'accessing'
method: RowanLoggingService
fileName
	^fileName
%

category: 'accessing'
method: RowanLoggingService
fileName: object
	fileName := object
%

category: 'initialization'
method: RowanLoggingService
initialize

	super initialize. 
	isLogging := false.
%

category: 'client commands'
method: RowanLoggingService
logComment: string
	| stonString ws |
	isLogging
		ifFalse: [ ^ self ].
	comment := string.
	id := id + 1.
	date := Date today.
	time := Time now.
	location := #'server'.
	stonString := STON toString: self.
	ws := FileStreamPortable write: fileName mode: #'append'.
	[ 
	ws
		nextPutAll: stonString;
		lf ]
		ensure: [ ws close ].
	comment := nil	"service may be reused. Clear comment"
%

category: 'accessing'
method: RowanLoggingService
logFileContents

	| rs |
	rs := [FileStreamPortable read: fileName] on: Error do:[:ex | ^String new].
	[^rs contents] ensure: [rs close]
%

category: 'client commands'
method: RowanLoggingService
logReceivedServices

	mode := #received.
	self class current: self. 
	self logServices
%

category: 'client commands'
method: RowanLoggingService
logSentServices

	mode := #sent.
	services := RowanCommandResult results copy asOrderedCollection.
	self logServices.
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanLoggingService
logServices
	| ws |
	isLogging
		ifFalse: [ ^ self ].
	id := id + 1.
	date := Date today.
	time := Time now.
	location := #'server'.
	ws := FileStreamPortable write: fileName mode: #'append'.
	ws
		nextPutAll: date printString;
		space;
		nextPutAll: time printString;
		space;
		nextPutAll: mode printString;
		space;
		nextPutAll: location printString;
		lf.
	[ 
	services
		do: [ :service | 
			ws
				nextPutAll: service logString;
				lf ] ]
		ensure: [ ws close ]
%

! Class implementation for 'RowanMethodService'

!		Class methods for 'RowanMethodService'

category: 'instance creation'
classmethod: RowanMethodService
forGsNMethod: aGsNMethod organizer: anOrganizer
	^self new
		initialize: aGsNMethod organizer: anOrganizer;
		yourself
%

category: 'instance creation'
classmethod: RowanMethodService
forSelector: sel class: theClass meta: boolean organizer: anOrganizer
	| service |
	service := self new.
	service
		selector: sel;
		meta: boolean.
	service forClass: theClass organizer: anOrganizer.
	^ service
%

category: 'instance creation'
classmethod: RowanMethodService
forSelector: sel class: theClass meta: boolean organizer: anOrganizer subclasses: theSubclasses
	| service |
	service := self new.
	service
		selector: sel;
		meta: boolean.
	service forClass: theClass organizer: anOrganizer subclasses: theSubclasses.
	^ service
%

category: 'utility'
classmethod: RowanMethodService
put: string onStreamWithoutUnicode: stream

	string do:[:char | 
			char asInteger > 255 ifTrue:[
				stream nextPutAll: '$\x', char asInteger asHexString]
			ifFalse:[
				stream nextPut: char
			]].
%

category: 'utility'
classmethod: RowanMethodService
removeUnicode: string

		| ws | 
		ws := WriteStream on: String new. 
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents
%

category: 'utility'
classmethod: RowanMethodService
removeUnicodeFromSource: string

	"possibly a unicode string which is not yet 
	presentable in Jadeite."

	(string isKindOf: MultiByteString) ifTrue:[
		| ws | 
		ws := WriteStream on: String new. 
		ws nextPutAll: '''METHOD SOURCE CANNOT BE DISPLAYED
This method source contains unicode and is not displayable in Jadeite. 
An approximation of the source code is given printed below with $\x<char hex value>
replacing the actual unicode character.''';
			cr; 
			cr.
		self put: string onStreamWithoutUnicode: ws. 
		^ws contents]
	ifFalse:[
		^string].
%

category: 'instance creation'
classmethod: RowanMethodService
source: source selector: selector category: category className: className packageName: packageName meta: boolString

	| service |
	self rowanFixMe. "Need to include super and sub implementors"
	service := self new. 
	service 
		source: source;
		selector: selector;
		category: category asString;
		className: className;
		packageName: packageName;
		meta: boolString == true.
	^service
%

category: 'rsr'
classmethod: RowanMethodService
templateClassName

	^#RowanMethodService
%

!		Instance methods for 'RowanMethodService'

category: 'comparing'
method: RowanMethodService
= methodService
	(methodService class canUnderstand: #isMethodService) ifFalse:[^false].
	methodService isMethodService ifFalse:[^false].
	^selector = methodService selector
		and: [className asString = methodService className asString and: [meta = methodService meta]]
%

category: 'Accessing'
method: RowanMethodService
accessedInstVars
  ^ accessedInstVars
%

category: 'Updating'
method: RowanMethodService
accessedInstVars: anArray
	accessedInstVars := anArray
%

category: 'method history'
method: RowanMethodService
addToMethodHistory
	| methodHistory |
	methodHistory := self rowanMethodHistory
		at: self unregisteredCopy
		ifAbsentPut: [ Array new ].
	methodHistory add: self unregisteredCopy
%

category: 'client commands'
method: RowanMethodService
allReferences
  | methods |
  oop := self gsNMethod asOop.
  methods := self organizer sendersOf: selector.
  references := methods first
    collect: [ :gsNMethod | self class forGsNMethod: gsNMethod organizer: self organizer ].
  RowanCommandResult addResult: self
%

category: 'Accessing'
method: RowanMethodService
breakPoints

	^breakPoints
%

category: 'Accessing'
method: RowanMethodService
breakPoints: collection
  breakPoints := collection
%

category: 'initialization'
method: RowanMethodService
breakPointsFor: aGsNMethod
  "Answers an Array stepPoints"

  | list theMethod |
  list := OrderedCollection new.
  theMethod := aGsNMethod isMethodForBlock
    ifTrue: [ 
      isMethodForBlock := true.
      aGsNMethod homeMethod ]
    ifFalse: [ aGsNMethod ].
  homeMethodOop := theMethod asOop.
  theMethod _allBreakpoints
    ifNil: [ ^ OrderedCollection new ]
    ifNotNil: [ :anArray | 
      1 to: anArray size by: 4 do: [ :i | 
        list
          add:
            (theMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)) ] ].
  ^ list asOrderedCollection
%

category: 'Accessing'
method: RowanMethodService
category
	^category
%

category: 'Updating'
method: RowanMethodService
category: newValue
	category := newValue asString
%

category: 'Accessing'
method: RowanMethodService
className
	^className
%

category: 'Updating'
method: RowanMethodService
className: newValue
	className := newValue asString
%

category: 'Accessing'
method: RowanMethodService
classOrMeta
	| theClass |
	theClass := self theClass ifNil: [^nil]. 
	^meta 
			ifTrue:[theClass class] 
			ifFalse: [theClass].
%

category: 'client commands'
method: RowanMethodService
clearBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method _breakOperation: 2 forStepPoint: stepPoint breakpointLevel: 1.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
clearMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self clearBreakAt: breakPoint ]
%

category: 'Updating'
method: RowanMethodService
comparisonSource: string

	comparisonSource := string
%

category: 'Accessing'
method: RowanMethodService
compilationWarnings

	^compilationWarnings
%

category: 'Updating'
method: RowanMethodService
compilationWarnings: newValue
	compilationWarnings := newValue
%

category: 'client commands'
method: RowanMethodService
debugTest: testSelector inClassName: theClassName
  testResult := 'passed'.
  [ (Rowan image objectNamed: theClassName) debug: testSelector asSymbol ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanMethodService
debugTestAsFailure: testSelector inClassName: theClassName
  testResult := 'passed'.
  [ 
  ((Rowan image objectNamed: theClassName) selector: testSelector asSymbol)
    debugAsFailure ]
    on: Exception
    do: [ :ex | 
      RowanDebuggerService new saveProcessOop: GsProcess _current asOop.
      testResult := ex class = TestFailure
        ifTrue: [ 'failure' ]
        ifFalse: [ 
          (ex class isSubclassOf: Notification)
            ifTrue: [ 'passed' ]
            ifFalse: [ 'error' ] ].
      ex pass ].
  testRunClassName := theClassName.
  RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanMethodService
definedClassName
	^definedClassName
%

category: 'accessing'
method: RowanMethodService
definedClassName: object
	definedClassName := object
%

category: 'Accessing'
method: RowanMethodService
definedPackage

	^definedPackage
%

category: 'rowan'
method: RowanMethodService
definitionClass

	^RwMethodDefinition
%

category: 'client commands'
method: RowanMethodService
disableBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method disableBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
disableMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self disableBreakAt: breakPoint ]
%

category: 'client commands'
method: RowanMethodService
enableBreakAt: stepPoint
	| method |
	method := self isUnboundMethod 
			ifTrue:[(Object _objectForOop: oop) homeMethod] 
			ifFalse:[self gsNMethod].
	method setBreakAtStepPoint: stepPoint.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanMethodService
enableMethodBreaks
  self update.
  breakPoints do: [ :breakPoint | self enableBreakAt: breakPoint ]
%

category: 'Updating'
method: RowanMethodService
failedCompile: boolean	
	
	failedCompile := boolean
%

category: 'client commands'
method: RowanMethodService
fileout
	| answeringService ws |
	answeringService := RowanAnsweringService new.
	ws := WriteStream on: String new. 
	self writeFileOutHeaderOn: ws.
	ws nextPutAll: (self behavior fileOutMethod: selector asString environmentId: 0).
	answeringService answer: ws contents.
	RowanCommandResult addResult: answeringService.
%

category: 'Accessing'
method: RowanMethodService
firstReference: integer

	firstReference := integer
%

category: 'initialization'
method: RowanMethodService
forClass: theClass organizer: theOrganizer
	"assume meta and selector are set"

	| classOrMeta gsNMethod |
	organizer := theOrganizer.
	classOrMeta := meta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	gsNMethod := classOrMeta compiledMethodAt: selector.
	definedPackage := gsNMethod rowanPackageName.
	oop := gsNMethod asOop.
	stepPoints := self stepPointsFor: gsNMethod.
	breakPoints := self breakPointsFor: gsNMethod.
	self updateSource: gsNMethod sourceString.
	category := (classOrMeta categoryOfSelector: selector) asString.
	className := theClass name asString.
	packageName := gsNMethod rowanPackageName.
	projectName := gsNMethod rowanProjectName.
	self setSupersAndSubsFor: classOrMeta.
	isExtension := self rowanIsExtension.
	self initializeTestMethodsFor: classOrMeta thisClass.
	self setDefinedClass: classOrMeta
%

category: 'initialization'
method: RowanMethodService
forClass: theClass organizer: theOrganizer subclasses: subclasses
	"assume meta and selector are set"

	| classOrMeta gsNMethod |
	organizer := theOrganizer.
	classOrMeta := meta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	gsNMethod := classOrMeta compiledMethodAt: selector.
	definedPackage := gsNMethod rowanPackageName.
	oop := gsNMethod asOop.
	stepPoints := self stepPointsFor: gsNMethod.
	breakPoints := self breakPointsFor: gsNMethod.
	self updateSource: gsNMethod sourceString.
	category := (classOrMeta categoryOfSelector: selector) asString.
	className := theClass name asString.
	packageName := gsNMethod rowanPackageName.
	projectName := gsNMethod rowanProjectName.
	self setSupersAndSubsFor: classOrMeta using: subclasses.
	isExtension := self rowanIsExtension.
	self initializeTestMethodsFor: classOrMeta thisClass.
	self setDefinedClass: classOrMeta
%

category: 'instance creation'
method: RowanMethodService
forSelector: sel class: theClass meta: boolean organizer: anOrganizer

	| service |
	service := self new. 
	service selector: sel;
		meta: boolean.
	service forClass: theClass organizer: anOrganizer.
	^service
%

category: 'Accessing'
method: RowanMethodService
gsNMethod
	| theBehavior |
	theBehavior := self theClass ifNil: [^nil]. 
	^ theBehavior compiledMethodAt: selector otherwise: nil
%

category: 'comparing'
method: RowanMethodService
hash
  ^ (selector hash bitXor: className hash) bitXor: meta hash
%

category: 'Accessing'
method: RowanMethodService
hasSubs

	^hasSubs
%

category: 'Updating'
method: RowanMethodService
hasSubs: aBoolean

	hasSubs := aBoolean
%

category: 'Accessing'
method: RowanMethodService
hasSupers

	^hasSupers
%

category: 'Updating'
method: RowanMethodService
hasSupers: aBoolean

	hasSupers := aBoolean
%

category: 'initialization'
method: RowanMethodService
initialize

	super initialize. 
	hasSupers := false. 
	hasSubs := false.
	accessedInstVars := Array new.
	isTestMethod := false.
	failedCompile := false.
	isMethodForBlock := false.
	hasMethodHistory := true.
	inSelectedPackage := true.
	isExtension := false.
%

category: 'initialization'
method: RowanMethodService
initialize: aGsNMethod organizer: aClassOrganizer

	| inClass |
	oop := aGsNMethod asOop.
	definedPackage := aGsNMethod rowanPackageName. 
	selector := aGsNMethod selector.
	stepPoints := self stepPointsFor: aGsNMethod.
	breakPoints := self breakPointsFor: aGsNMethod.
	((inClass := aGsNMethod inClass) isNil or: [selector isNil]) ifTrue: [
		meta := false.
		self updateSource: aGsNMethod sourceString.
		hasSupers := false.
		hasSubs := false.
		organizer := aClassOrganizer. 
		inSelectedPackage := false.
		^self
	].
	selectedPackageServices ifNotNil: [
		inSelectedPackage := (selectedPackageServices detect: [:packageService | packageService name = packageName] ifNone:[]) notNil. 
		]. 
	meta := inClass isMeta.
	self 
		forClass: inClass thisClass 
		organizer: aClassOrganizer.
	self initializeTestMethodsFor: inClass thisClass.
	self setHasMethodHistory
%

category: 'initialization'
method: RowanMethodService
initializeTestMethodsFor: aClass
	| testSelectors |
	(aClass inheritsFrom: TestCase)
		ifTrue: [ 
			aClass isAbstract
				ifTrue: [ ^ self ].
			testSelectors := aClass thisClass allTestSelectors.
			isTestMethod := testSelectors includes: selector ]
%

category: 'accessing'
method: RowanMethodService
inSelectedPackage
	^inSelectedPackage
%

category: 'accessing'
method: RowanMethodService
inSelectedPackage: object
	inSelectedPackage := object
%

category: 'testing'
method: RowanMethodService
isMethodService

	^true
%

category: 'testing'
method: RowanMethodService
isTestMethod

	^isTestMethod
%

category: 'Updating'
method: RowanMethodService
isTestMethod: boolean

	isTestMethod := boolean
%

category: 'testing'
method: RowanMethodService
isUnboundMethod

	(className notNil and: [selector notNil]) ifTrue:[^false].
	^(Object _objectForOop: oop) isKindOf: GsNMethod
%

category: 'Accessing'
method: RowanMethodService
meta
	^meta
%

category: 'Updating'
method: RowanMethodService
meta: aBoolean
	"allow nil parameter for now" 
	meta := aBoolean == true
%

category: 'Accessing'
method: RowanMethodService
methodDefinitions
	^methodDefinitions
%

category: 'Updating'
method: RowanMethodService
methodDefinitions: newValue
	methodDefinitions := newValue
%

category: 'Accessing'
method: RowanMethodService
name

	^selector
%

category: 'accessing'
method: RowanMethodService
oop
	^oop
%

category: 'accessing'
method: RowanMethodService
oop: object
	oop := object
%

category: 'Accessing'
method: RowanMethodService
packageName
	^packageName
%

category: 'Updating'
method: RowanMethodService
packageName: newValue
	packageName := newValue
%

category: 'services'
method: RowanMethodService
packageService
	"construct a package service based on the package this method resides in"

	^ RowanPackageService forPackageNamed: packageName
%

category: 'printing'
method: RowanMethodService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $(;
				nextPutAll: (className ifNil:[nil printString]); 
				nextPutAll: '>>'; 
				nextPutAll: (selector ifNil:[nil printString]);
				nextPut: $)
%

category: 'services'
method: RowanMethodService
projectService
	"construct a project service based on the package this method resides in"

	^ RowanProjectService new name: projectName
%

category: 'client commands'
method: RowanMethodService
reformatSource
  source := (RBParser parseMethod: source) formattedCode.
  updateType := #'dontUpdateSystem'.	"let browser update the source"
  RowanCommandResult addResult: self
%

category: 'rowan'
method: RowanMethodService
removeSelector: sel class: clsName
	
	self rowanFixMe. "combine remove methods variants"
	self browserTool removeMethod: sel asSymbol forClassNamed: (clsName subStrings first) isMeta: (clsName subStrings size = 2)
%

category: 'Accessing'
method: RowanMethodService
renamedName
	^renamedName
%

category: 'Accessing'
method: RowanMethodService
renamedName: object
	renamedName := object
%

category: 'rowan'
method: RowanMethodService
rowanIsExtension

	^Rowan projectTools browser isExtensionMethod: selector asString forClassNamed: className asString isMeta: meta
%

category: 'rowan'
method: RowanMethodService
rowanProjectName

	^projectName
%

category: 'client commands'
method: RowanMethodService
runTest: testSelector inClassName: theClassName
	| sunitTestResult |
	sunitTestResult := (Rowan image objectNamed: theClassName)
		run: testSelector asSymbol.
	sunitTestResult errorCount > 0
		ifTrue: [ testResult := 'error' ].
	sunitTestResult failureCount > 0
		ifTrue: [ testResult := 'failure' ].
	sunitTestResult passedCount > 0
		ifTrue: [ testResult := 'passed' ].
	testRunClassName := theClassName.
	updateType := #'testResultUpdate:browser:'.
	RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanMethodService
searchString
	^searchString
%

category: 'accessing'
method: RowanMethodService
searchString: object
	searchString := object
%

category: 'Accessing'
method: RowanMethodService
selectedPackageServices

	^selectedPackageServices
%

category: 'Updating'
method: RowanMethodService
selectedPackageServices: collection

	selectedPackageServices := collection
%

category: 'Accessing'
method: RowanMethodService
selector

	^selector
%

category: 'Updating'
method: RowanMethodService
selector: aSymbol

	selector := aSymbol
%

category: 'perform'
method: RowanMethodService
servicePerform: symbol withArguments: collection
	| theCommand |
	self isUpdatingButFoundToBeDeleted
		ifTrue: [ ^ self handleDeletedService ].
	theCommand := command.
	super
		servicePerform: symbol
		withArguments: collection
		shouldUpdate: (theCommand == #'reformatSource') not
%

category: 'client commands'
method: RowanMethodService
setBreakAt: stepPoint
	| method |
	method := self isUnboundMethod
		ifTrue: [ (Object _objectForOop: oop) homeMethod ]
		ifFalse: [ self gsNMethod ].
	method setBreakAtStepPoint: stepPoint breakpointLevel: 1.
	self class breakPointsAreEnabled
		ifFalse: [ self disableBreakAt: stepPoint ].
	self update.
	RowanCommandResult addResult: self
%

category: 'initialization'
method: RowanMethodService
setDefinedClass: aClass
	"only used in the test browsers (for now) to identify which class the method is defined in.
	That may be different than what is displayed, say, when we run a superclass test in concrete subclass.
	Assume selector is defined in aClass hierarchy."

	definedClassName := (aClass whichClassIncludesSelector: selector asString) name
		asString
%

category: 'method history'
method: RowanMethodService
setHasMethodHistory
	self rowanMethodHistory at: self ifAbsent: [ ^ hasMethodHistory := false ].
	hasMethodHistory := true
%

category: 'initialization'
method: RowanMethodService
setSupersAndSubsFor: theClass
	| theSuper implementingClass |
	true
		ifTrue: [ 
			hasSubs := false.
			hasSupers := false.
			^ self	"may be implicated in gem out of memory conditions if tempobj cache size isn't raised" ].
	theSuper := theClass superClass.
	theSuper
		ifNotNil: [ 
			implementingClass := theSuper whichClassIncludesSelector: selector.
			hasSupers := implementingClass notNil.
			hasSupers
				ifTrue: [ 
					comparisonSource := (implementingClass
						compiledMethodAt: selector
						environmentId: 0
						otherwise: nil) sourceString.
					superDisplayString := implementingClass name , '>>' , selector ] ].
	(self organizer allSubclassesOf: theClass thisClass)
		do: [ :cls | 
			| aClass |
			aClass := theClass isMeta
				ifTrue: [ cls class ]
				ifFalse: [ cls ].
			(hasSubs := (aClass
				compiledMethodAt: selector
				environmentId: 0
				otherwise: nil) notNil)
				ifTrue: [ ^ self ] ]
%

category: 'initialization'
method: RowanMethodService
setSupersAndSubsFor: theClass using: subclasses
	| theSuper implementingClass |
	theSuper := theClass superClass.
	theSuper
		ifNotNil: [ 
			implementingClass := theSuper whichClassIncludesSelector: selector.
			hasSupers := implementingClass notNil.
			hasSupers
				ifTrue: [ 
					comparisonSource := (implementingClass
						compiledMethodAt: selector
						environmentId: 0
						otherwise: nil) sourceString.
					superDisplayString := implementingClass name , '>>' , selector ] ].
	subclasses
		do: [ :cls | 
			| aClass |
			aClass := theClass isMeta
				ifTrue: [ cls class ]
				ifFalse: [ cls ].
			(hasSubs := (aClass
				compiledMethodAt: selector
				environmentId: 0
				otherwise: nil) notNil)
				ifTrue: [ ^ self ] ]
%

category: 'Accessing'
method: RowanMethodService
source

	^source
%

category: 'Updating'
method: RowanMethodService
source: string
	
	self updateSource: string
%

category: 'Accessing'
method: RowanMethodService
stepPoints

	"for testing"
	
	^stepPoints
%

category: 'Updating'
method: RowanMethodService
stepPoints: collection

	stepPoints := collection
%

category: 'initialization'
method: RowanMethodService
stepPointsFor: aGsNMethod
	"Answers an Array of Associations (offset -> selector) indexed by step point"

	|  selectors list |
	(selectors := aGsNMethod _sourceOffsetsOfSends) ifNil: [^#()].
	list := aGsNMethod homeMethod  _sourceOffsets.
	list := list collect: [:each |
		| index eachSelector |
		eachSelector := ''.
		index := selectors indexOf: each.
		0 < index ifTrue: [eachSelector := selectors at: index + 1].
		each -> eachSelector.
	].
	^list
%

category: 'Accessing'
method: RowanMethodService
testResult

	^testResult
%

category: 'accessing'
method: RowanMethodService
theClass
	"the dictionary browser may have versions numbers in the name"

	| theName theBehavior theClass |
	theName := (className copyUpTo: Character space) asSymbol.
	theClass := ((AllUsers userWithId: 'SystemUser') objectNamed: theName asSymbol)
		ifNil: [ 
			(AllUsers userWithId: GsCurrentSession currentSession userProfile userId)
				objectNamed: theName asSymbol ].
	theClass
		ifNil: [ 
			"can't find class"
			^ nil ].
	theBehavior := self meta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].
	^theBehavior
%

category: 'updates'
method: RowanMethodService
update
	super update.
	self isUnboundMethod ifFalse:[
		self wasRecycled ifTrue:[oop := self gsNMethod asOop].
		self wasDeleted ifTrue:[
			self updateType: #methodsRemoved:. 
			^RowanCommandResult addResult: self. ].  "removed method"
		oop ifNil: [oop := self gsNMethod asOop]].
	self 
		initialize: (Object _objectForOop: oop) 
		organizer: self organizer.
	RowanCommandResult addResult: self.
%

category: 'updates'
method: RowanMethodService
updateLatest
	| theClass compiledMethod |
	theClass := (RowanClassService new name: className) theClass.
	theClass ifNil: [ ^ self ].
	compiledMethod := theClass compiledMethodAt: selector otherwise: nil.
	compiledMethod
		ifNil: [ 
			self updateType: #'removed:'.
			^ RowanCommandResult addResult: self ].
	oop := compiledMethod asOop.
	super updateLatest
%

category: 'updates'
method: RowanMethodService
updatePackageProjectAfterCategoryChange: beforePackageName
	"the dirty state of the package & project may have changed so update packages
	and projects both before and after the move"

	| beforePackageService |
	(beforePackageService := RowanPackageService forPackageNamed: beforePackageName)
		update.
	(RowanProjectService new name: beforePackageService projectName) update.
	self packageService update.
	self projectService update
%

category: 'updates'
method: RowanMethodService
updateSource: string
	source := string
%

category: 'testing'
method: RowanMethodService
wasDeleted
	selector isNil ifTrue:[^false].
	^self gsNMethod isNil
%

category: 'testing'
method: RowanMethodService
wasRecycled
	(oop notNil and:[self gsNMethod asOop ~= oop]) ifTrue:[^true].
	^false
%

! Class implementation for 'RowanPackageService'

!		Class methods for 'RowanPackageService'

category: 'instance creation'
classmethod: RowanPackageService
forPackageNamed: aName
	| inst |
	inst := self new.
	inst name: aName.
	aName isNil
		ifFalse: [ inst updateIsDirty ].
	inst setDefaultTemplate.
	inst updateProjectName.
	^ inst
%

category: 'rsr'
classmethod: RowanPackageService
templateClassName

	^#RowanPackageService
%

!		Instance methods for 'RowanPackageService'

category: 'comparing'
method: RowanPackageService
= packageService
	(packageService isKindOf: RowanPackageService) ifFalse:[^false].
	^name = packageService name
%

category: 'testing'
method: RowanPackageService
arePackageAndProjectClean
  ^ self rowanDirty not and: [ self projectIsDirty not ]
%

category: 'rowan'
method: RowanPackageService
changes
   "diffForPackageName: not implemented yet"
 
   ^ (Rowan packageTools diff diffForPackageName: name) asString
%

category: 'other'
method: RowanPackageService
classes: collection

	classes := collection
%

category: 'client commands'
method: RowanPackageService
classHierarchy
	| theClasses |
	self update. 
	theClasses := classes collect:[:classService | classService theClass].
	hierarchyServices := self classHierarchy: theClasses. 
	RowanCommandResult addResult: self.
%

category: 'commands support'
method: RowanPackageService
classHierarchy: theClasses
  hierarchyServices := super classHierarchy: theClasses.
  ^ hierarchyServices
%

category: 'rowan'
method: RowanPackageService
createClassNamed: className superclass: superClassName instVars: instVars
	| classDefinition browserTool |
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: superClassName
		instvars: instVars
		classinstvars: #()
		classvars: #()
		category: ''
		comment: ''
		pools: #()
		type: 'normal'.
	browserTool := Rowan projectTools browser.
	browserTool createClass: classDefinition inPackageNamed: name.
	self update.
%

category: 'rowan'
method: RowanPackageService
createPackageNamed: aString inProject: projName
	| projectService | 
	name := aString.
	projectService := RowanProjectService new.
	projectDefinition := projectService createProjectNamed: projName.  
	projectDefinition addPackageNamed: name.
	projectDefinition load.
%

category: 'Updating'
method: RowanPackageService
defaultTemplate: newValue
	defaultTemplate := newValue
%

category: 'rowan'
method: RowanPackageService
definition

	^(Rowan image loadedPackageNamed: name) asDefinition
%

category: 'rowan'
method: RowanPackageService
deletePackage

	self browserTool removePackageNamed: name.
%

category: 'client commands'
method: RowanPackageService
exportTopazFormatTo: filePath
	| rwProject |
	rwProject := (RowanProjectService new name: projectName) rwProject.
	rwProject
		exportTopazFormatTo: filePath
		logClassCreation: false
		excludeClassInitializers: false
		excludeRemoveAllMethods: false
		usingPackageNamesMap:
			(Dictionary new
				at: filePath put: {name};
				yourself)
%

category: 'rowan'
method: RowanPackageService
genericClassCreationTemplate

	^self browserTool classCreationTemplateForSubclassOf: 'Object' category: name packageName: nil
%

category: 'comparing'
method: RowanPackageService
hash
	^self name hash
%

category: 'Accessing'
method: RowanPackageService
hierarchyServices

	^hierarchyServices
%

category: 'Accessing'
method: RowanPackageService
isDirty
	isDirty ifNil: [self updateIsDirty]. 
	^isDirty
%

category: 'Updating'
method: RowanPackageService
isDirty: boolean

	isDirty := boolean
%

category: 'testing'
method: RowanPackageService
isPackageService

	^true
%

category: 'Accessing'
method: RowanPackageService
jadeite_testClasses

	"for testing" 
	^testClasses
%

category: 'rowan'
method: RowanPackageService
loadedClasses

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClasses
%

category: 'rowan'
method: RowanPackageService
loadedClassExtensions

	| loadedPackage |
	loadedPackage := Rowan image loadedPackageNamed: name ifAbsent:[^KeyValueDictionary new].
	^loadedPackage loadedClassExtensions
%

category: 'rowan'
method: RowanPackageService
loadedClassHandles
	| loadedClasses |
	loadedClasses := self loadedClasses collect:[:loadedClass | loadedClass handle].
	loadedClasses addAll: (self loadedClassExtensions collect:[:extension | extension handle]).
	^loadedClasses
%

category: 'rowan'
method: RowanPackageService
loadedClassNames

	^self loadedClasses collect:[:loadedClass | loadedClass name]
%

category: 'Accessing'
method: RowanPackageService
name
	^name
%

category: 'Updating'
method: RowanPackageService
name: newValue
	name := newValue
%

category: 'Accessing'
method: RowanPackageService
packageName
	^name
%

category: 'Updating'
method: RowanPackageService
packageName: newValue
	name := newValue
%

category: 'printing'
method: RowanPackageService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'Accessing'
method: RowanPackageService
projectDefinition
	^projectDefinition
%

category: 'Updating'
method: RowanPackageService
projectDefinition: newValue
	projectDefinition := newValue
%

category: 'testing'
method: RowanPackageService
projectIsDirty
  projectName ifNil: [ self updateProjectName ].
  ^ (RowanProjectService new name: projectName) rowanDirty
%

category: 'Accessing'
method: RowanPackageService
projectName
	^projectName
%

category: 'Updating'
method: RowanPackageService
projectName: newValue
	projectName := newValue
%

category: 'client commands'
method: RowanPackageService
removeClass: classService
  self removeClassNamed: classService name.
  self setDefaultTemplate.
  classService updateType: #'removedClass:'.
  classService wasRemoved: true.
  RowanCommandResult addResult: classService.
  RowanBrowserService new packagesWithTests	"sunit browser might need updated"
%

category: 'commands support'
method: RowanPackageService
removeClassNamed: className

	self browserTool removeClassNamed: className.
%

category: 'rowan'
method: RowanPackageService
rowanDirty

	^(RwPackage newNamed: name) isDirty
%

category: 'rowan'
method: RowanPackageService
rowanProjectName

	^projectName
%

category: 'other'
method: RowanPackageService
selectedClass
	
	^selectedClass
%

category: 'Accessing'
method: RowanPackageService
selectedClass: classService
	selectedClass := classService.
	classService selectedPackageServices: (Array with: self)
%

category: 'perform'
method: RowanPackageService
servicePerform: symbol withArguments: collection
	| wasClean projectService |
	self isUpdatingButFoundToBeDeleted
		ifTrue: [ ^ self handleDeletedService ].
	wasClean := self arePackageAndProjectClean.
	super servicePerform: symbol withArguments: collection.
	wasClean
		ifTrue: [ 
			projectService := RowanProjectService new name: projectName.
			projectService updateIsDirty.
			RowanCommandResult addResult: projectService ]
%

category: 'commands support'
method: RowanPackageService
services: services from: levels expand: toExpand
  ^ self
    services: services
    from: levels
    expand: toExpand
    classes: (classes collect: [ :classService | classService theClass ])
%

category: 'client commands'
method: RowanPackageService
setDefaultTemplate

	defaultTemplate := self genericClassCreationTemplate.
%

category: 'extensions'
method: RowanPackageService
testClasses
	testClasses := Set new.
	self loadedClasses
		valuesDo: [ :loadedClass | 
			| cls |
			cls := loadedClass handle.
			(cls inheritsFrom: TestCase)
				ifTrue: [ 
					cls isAbstract
						ifFalse: [ 
							| classService |
							classService := RowanClassService basicForClassNamed: cls name.
							testClasses add: classService update ] ] ].
	self loadedClassExtensions
		valuesDo: [ :loadedClass | 
			| cls |
			cls := loadedClass handle.
			(cls inheritsFrom: TestCase)
				ifTrue: [ 
					cls isAbstract
						ifFalse: [ 
							| classService |
							classService := RowanClassService basicForClassNamed: cls name.
							testClasses add: classService update ] ] ].
	updateType := #'testClasses:browser:'.
	testClasses := testClasses asArray.
	RowanCommandResult addResult: self
%

category: 'updates'
method: RowanPackageService
update
	| allPackageNames thePackage |
	super update.
	thePackage := Rowan image loadedPackageNamed: name ifAbsent: [ ^ self ].
	allPackageNames := Rowan image packageNames.
	classes := (self loadedClassNames keys
		collect: [ :string | RowanClassService minimalForClassNamed: string packageNames: allPackageNames ])
		asArray.
	classes
		addAll:
			(self loadedClassExtensions keys
				collect: [ :string | 
					| classService |
					classService := (RowanClassService
						minimalForClassNamed: string
						packageNames: allPackageNames) isExtension: true.
					(Rowan image loadedClassForClass: classService theClass ifAbsent: [  ])
						ifNotNil: [ :cls | classService definedPackageName: cls packageName ].
					classService ]).
	classes do: [ :clsService | clsService packageName: self name ].
	self updateIsDirty.
	projectName := (Rowan image loadedPackageNamed: name) projectName.
	RowanCommandResult addResult: self.
	dictionaryName := thePackage gs_symbolDictionary. 
	shouldUpdate := false
%

category: 'updates'
method: RowanPackageService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: classes size do:[:index |
		| classesService |
		classesService := classes at: index. 
		classesService = updatedService ifTrue:[
			classes at: index put: updatedService
		]].
%

category: 'rowan'
method: RowanPackageService
updateIsDirty

	isDirty := self rowanDirty.
%

category: 'updates'
method: RowanPackageService
updateProject
	| projectService |

	projectService := RowanProjectService new name: projectName. 
	projectService update.
	RowanCommandResult addResult: projectService.
%

category: 'updates'
method: RowanPackageService
updateProjectName

	projectName := (Rowan image loadedPackageNamed: name) projectName.
%

category: 'testing'
method: RowanPackageService
wasDeleted

	^(Rowan image loadedPackageNamed: name
			ifAbsent: []) isNil
%

! Class implementation for 'RowanProcessService'

!		Class methods for 'RowanProcessService'

category: 'instance creation'
classmethod: RowanProcessService
new

	"in general use one of the other constructor methods.
	Needed for STON replication"
	^super new
%

category: 'instance creation'
classmethod: RowanProcessService
onActiveProcess: aGsProcess

	^self basicNew
		initialize;
		initialize: aGsProcess status: 'active';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onReadyProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'ready';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onSuspendedProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'suspended';
		yourself
%

category: 'instance creation'
classmethod: RowanProcessService
onWaitingProcess: aGsProcess

	^self basicNew
		initialize: aGsProcess status: 'waiting';
		yourself
%

category: 'rsr'
classmethod: RowanProcessService
templateClassName

	^#RowanProcessService
%

!		Instance methods for 'RowanProcessService'

category: 'initialize'
method: RowanProcessService
clearOrganizers
	self organizer: nil. 
	updates := nil.
	frames
		ifNotNil: [ 
			frames
				do: [ :frameService | 
					frameService organizer: nil.  
					frameService method organizer: nil.
					frameService vars
						ifNotNil: [ :vars | vars do: [ :variableService | variableService organizer: nil ] ] ] ]
%

category: 'accessing'
method: RowanProcessService
errorMessage
	^errorMessage
%

category: 'accessing'
method: RowanProcessService
errorMessage: object
	errorMessage := object
%

category: 'initialize'
method: RowanProcessService
frames

	^frames
%

category: 'initialize'
method: RowanProcessService
initialize

	frames := Array new
%

category: 'initialize'
method: RowanProcessService
initialize: aGsProcess status: aString
	| theOrganizer |
	theOrganizer := ClassOrganizer new.
	frames := Array new: aGsProcess stackDepth.
	1 to: aGsProcess stackDepth do: [ :i | 
		frames
			at: i
			put:
				([ RowanFrameServiceServer process: aGsProcess level: i organizer: theOrganizer ]
					on: Error , Halt
					do: [ :ex | 
						GsFile gciLogServer: 'error building Jadeite debugger - ' , ex printString.
						RowanFrameServiceServer basicNew
							label: 'unprintable frame - error: ' , ex printString;
							method:
									(RowanMethodServiceServer new
											stepPoints: Array new;
											breakPoints: Array new) ]) ].
	oop := aGsProcess asOop.
	status := aString
%

category: 'accessing'
method: RowanProcessService
oop
	^oop
%

category: 'accessing'
method: RowanProcessService
oop: object
	oop := object
%

category: 'accessing'
method: RowanProcessService
status
	^status
%

category: 'accessing'
method: RowanProcessService
status: object
	status := object
%

category: 'updating'
method: RowanProcessService
update
	super update. 
	self initialize: (Object _objectForOop: oop) status: 'suspended'.
	RowanCommandResult addResult: self.
%

! Class implementation for 'RowanProcessServiceServer'

!		Class methods for 'RowanProcessServiceServer'

category: 'instance creation'
classmethod: RowanProcessServiceServer
existingProcessServiceFor: suspendedProcess
	" for use with rsr only"

	^self processServices at: suspendedProcess ifAbsent: [  ]
%

category: 'instance creation'
classmethod: RowanProcessServiceServer
openDebuggerOn: suspendedProcess exception: ex connection: connection
	" for use with rsr only"

	| processService |
	
	processService := self saveOnSuspendedProcess: suspendedProcess exception: ex connection: connection.
	^processService openDebugger
%

category: 'instance creation'
classmethod: RowanProcessServiceServer
processServices
	" for use with rsr only"

	^ SessionTemps current
		at: #'jadeiteProcessServices'
		ifAbsentPut: [ Dictionary new ]
%

category: 'instance creation'
classmethod: RowanProcessServiceServer
removeProcessServiceFor: suspendedProcess
	" for use with rsr only"

	| processService |
	processService := self processServices at: suspendedProcess ifAbsent: [  ].
	processService ifNotNil: [ processService cleanUpClient ].
	self processServices removeKey: suspendedProcess ifAbsent: [  ]
%

category: 'instance creation'
classmethod: RowanProcessServiceServer
saveOnSuspendedProcess: suspendedProcess exception: ex connection: connection
	" for use with rsr only"

	| processService |
	processService := RowanProcessServiceServer
		onSuspendedProcess: suspendedProcess.
	processService
		registerWith: connection;
		errorMessage: ex messageText;
		updates: Array new;
		postCommandExecution.
	self processServices at: suspendedProcess put: processService.
	^ processService
%

!		Instance methods for 'RowanProcessServiceServer'

category: 'debugger'
method: RowanProcessServiceServer
cleanUpClient
	| promise |
	self postCommandExecution. 
	promise := remoteSelf cleanUpClient.
	^ promise wait
%

category: 'debugger'
method: RowanProcessServiceServer
openDebugger
	| promise |
	promise := remoteSelf openDebugger.
	^ promise wait
%

category: 'debugger'
method: RowanProcessServiceServer
updateClient
	| promise |
	self update. 
	self postCommandExecution. 
	promise := remoteSelf updateClient.
	^ promise wait
%

! Class implementation for 'RowanProjectService'

!		Class methods for 'RowanProjectService'

category: 'instance creation'
classmethod: RowanProjectService
newNamed: aString

	| inst |
	inst := self new.
	inst name: aString.
	inst refresh.
	^inst
%

category: 'rsr'
classmethod: RowanProjectService
templateClassName

	^#RowanProjectService
%

!		Instance methods for 'RowanProjectService'

category: 'comparing'
method: RowanProjectService
= projectService

	(projectService isKindOf: RowanProjectService) ifFalse:[^false]. 
	^projectService isProjectService ifTrue: [name = projectService name] ifFalse: [^false]
%

category: 'client commands'
method: RowanProjectService
addNewPackageNamed: packageName inSymbolDictionaryNamed: symbolDictionaryName toComponentNamed: componentName
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self rwProject
				addNewPackageNamed: packageName
				inSybolDictionaryNamed: symbolDictionaryName
				toComponentNamed: componentName.
			self update.
			^ self answer: #'added' ].
	self answer: #'duplicatePackage'.
	self update. 
	RowanCommandResult addResult: self.
%

category: 'client commands'
method: RowanProjectService
addPackageNamed: packageName
	self rowanFixMe.	"remove after #addPackageNamed:toComponentNamed: is integrated in v2.0 work"
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self browserTool
				addPackageNamed: packageName
				toComponentNamed: 'Core'
				andProjectNamed: name.
			self update.
			^ self answer: #'added' ].
	self answer: #'duplicatePackage'
%

category: 'client commands'
method: RowanProjectService
addPackagesNamed: packageNames
	"for tests"
	packageNames do:[:packageName | 
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self browserTool
				addPackageNamed: packageName
				toComponentNamed: 'Core'
				andProjectNamed: name.
			 ]].
	self update.
%

category: 'rowan'
method: RowanProjectService
audit
	^ (Rowan projectNamed: name) audit
%

category: 'client commands support'
method: RowanProjectService
basicLoadProject: aBlock 
	| updatedProjects |
	[ updatedProjects := aBlock value ]
		on: Warning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description.
			ex resume ].
	updatedProjects
		do: [ :project | 
			| projectService |
			projectService := project name = name
				ifTrue: [ self	"don't send an extraneous service back across the wire" ]
				ifFalse: [ RowanProjectService newNamed: project name ].
			projectService update.
			RowanCommandResult addResult: projectService ].
	RowanBrowserService new updateProjects
%

category: 'initialization'
method: RowanProjectService
basicRefresh
	name = Rowan unpackagedName
		ifTrue: [ 
			isLoaded := false.
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
	componentServices := self componentServices.
	specService := RowanLoadSpecService new initialize: self rwProject loadSpecification asOop.
	packageConvention := self rwProject packageConvention.
	RowanCommandResult addResult: self
%

category: 'accessing'
method: RowanProjectService
branch

	name isNil ifTrue:[^String new].
	^self rwProject currentBranchName
%

category: 'accessing'
method: RowanProjectService
branch: anObject

	branch := anObject
%

category: 'client commands'
method: RowanProjectService
changes
	| jadeServer projectNames |
	jadeServer := Rowan platform jadeServerClassNamed: #'JadeServer'.
	changes := Array new.
	projectNames := name ifNil: [ Rowan projectNames ] ifNotNil: [ {name} ].
	projectNames
		do: [ :aProjectName | 
			(Rowan projectTools diff patchesForProjectNamed: aProjectName)
				do: [ :assoc | 
					| patch "key is packageName, value is a CypressPatch" |
					patch := assoc value.
					changes
						add:
							(jadeServer new
								_mcDescriptionOfPatch: patch
								baseName: 'closest ancestor'
								alternateName: nil
								packageName: assoc key) ] ].
	self refresh.
	RowanCommandResult addResult: self
%

category: 'client commands'
method: RowanProjectService
checkout: branchName
	| project |
	project := self rwProject. 
	Rowan gitTools gitcheckoutIn: project repositoryRootPath asFileReference with: branchName.
	self reloadProject.
%

category: 'client commands'
method: RowanProjectService
checkoutTag: tagName 
	Rowan gitTools gitcheckoutIn: self repositoryRoot with: tagName.
	self reloadProject.
%

category: 'client commands'
method: RowanProjectService
commitWithMessage: message
	
	Rowan projectTools write writeProjectNamed: name.
	Rowan projectTools commit commitProjectNamed: name message: message
%

category: 'accessing'
method: RowanProjectService
componentServices
	^self componentServicesFor: self rwProject
%

category: 'client commands support'
method: RowanProjectService
componentServicesFor: theRwProject
	| componentDictionary |
	componentDictionary := Dictionary new.
	componentDictionary
		at: #'nil'
		put:
			(theRwProject componentNames
				collect: [ :componentName | 
					| componentService |
					componentService := RowanComponentService
						forComponentNamed: componentName
						projectService: self.
					componentService addFlattenedHierarchyTo: componentDictionary. 
					componentService ]).

	^ componentDictionary
%

category: 'other'
method: RowanProjectService
createProjectComponent: componentName symDict: defaultSymbolDictName convention: thePackageConvention format: packageFormat projectsHome: projectsHome type: repositoryType
	^ self
		createProjectComponent: componentName
		symDict: defaultSymbolDictName
		convention: thePackageConvention
		format: packageFormat
		projectsHome: projectsHome
		type: repositoryType
		shouldWrite: true
%

category: 'other'
method: RowanProjectService
createProjectComponent: componentName symDict: defaultSymbolDictName convention: thePackageConvention format: packageFormat projectsHome: projectsHome type: repositoryType shouldWrite: shouldWrite
	| definedProject resolvedProject |
	Rowan version < '3.0.0' asRwSemanticVersionNumber
		ifTrue: [ self error: 'This script needs to be run against a Rowan v3 solo extent' ].
	definedProject := (Rowan newProjectNamed: name)
		addLoadComponentNamed: componentName;
		packageConvention: thePackageConvention;
		gemstoneSetDefaultSymbolDictNameTo: defaultSymbolDictName;
		repoType: repositoryType asSymbol;
		packageFormat: packageFormat;
		projectsHome: projectsHome;
		yourself.
	resolvedProject := definedProject resolveProject.
	shouldWrite
		ifTrue: [ 
			(projectsHome asFileReference / name / 'rowan') ensureDeleteAll.
			resolvedProject write ].
	resolvedProject loadAsDefined.
	RowanBrowserService new updateProjects
%

category: 'examples'
method: RowanProjectService
createProjectNamed: projectName 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.
%

category: 'examples'
method: RowanProjectService
createProjectNamed: projectName in: symbolDictionaryName

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].
%

category: 'client commands'
method: RowanProjectService
defaultSymbolDictionaryFromLoadSpec
	| loadSpec |
	loadSpec := self rwProject loadSpecification.
	defaultSymbolDictionaryName := loadSpec platformProperties
		at: #'defaultSymbolDictName'
		ifAbsent: [ loadSpec _gemstoneDefaultSymbolDictName ]
%

category: 'replication'
method: RowanProjectService
excludedInstVars

	^ super excludedInstVars, #( #rwProject)
%

category: 'accessing'
method: RowanProjectService
existsOnDisk

	^existsOnDisk
%

category: 'client commands'
method: RowanProjectService
exportTopazFormatTo: filePath

	self rwProject exportTopazFormatTo: filePath
%

category: 'comparing'
method: RowanProjectService
hash
	^self name hash
%

category: 'initialization'
method: RowanProjectService
initialize

	super initialize. 
	packages := Array new.
	isDiskDirty := false.
%

category: 'other'
method: RowanProjectService
initializePackageGroups
	self
		packageGroups:
			(self rwProject packageGroupNames
				collect: [ :theName | 
					RowanPackageGroupService
						forPackageGroupNamed: theName
						loadedProject: self rwProject ]).
	wasUpdated := true.
%

category: 'client commands'
method: RowanProjectService
installProjectFromFile: path projectsHome: projectsHomePath componentNames: componentNames attributes: attributes resolveStrict: strict
	| spec browserService actualPath ws |
	ws := WriteStream on: String new.
	actualPath := path copyFrom: 6 to: path size. "`file:` is prepended to argument"
	self updateType: #'dontUpdate'.	"this service should not be updated on the client"
	(FileReference / actualPath) isDirectory
		ifTrue: [ 
			ws
				nextPutAll: 'Path is not a file. Project not loaded.';
				cr;
				nextPutAll: actualPath.
			self inform: ws contents.
			^ self ].	
	spec := [ RwSpecification fromUrl: path ]
		on: Error
		do: [ :ex | 
			ws
				nextPutAll: 'Error occurred during loading of file. Debug?';
				cr;
				nextPutAll: 'Path:';
				tab;
				nextPutAll: actualPath;
				cr;
				nextPutAll: ex printString.
			(self confirm: ws contents)
				ifTrue: [ ex pass ]
				ifFalse: [ ^ self ] ].
	spec
		projectsHome: projectsHomePath;
		componentNames: componentNames;
		customConditionalAttributes: attributes;
		yourself.
	strict
		ifTrue: [ spec resolveStrict ].
	[ spec resolve load ]
		on: Warning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description.
			ex resume ].
	browserService := RowanBrowserService new updateProjects
%

category: 'client commands'
method: RowanProjectService
installProjectFromURL: url
	| spec browserService |
	spec := RwSpecification fromUrl: url.
	[ spec resolve load ]
		on: Warning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description.
			ex resume ].
	browserService := RowanBrowserService new updateProjects
%

category: 'rowan'
method: RowanProjectService
isDirty
	name isNil
		ifTrue: [ ^ false ].
	^ self rwProject isDirty
%

category: 'accessing'
method: RowanProjectService
isDirty: aBoolean

	isDirty := aBoolean.
%

category: 'client commands'
method: RowanProjectService
isGitDirty
  "From https://github.com/GemTalk/Jadeite/issues/323#issuecomment-442545934"

  [ 
  ^ (Rowan gitTools gitstatusIn: self repositoryRootPath with: '--porcelain')
    isEmpty not ]
    on: Error
    do: [ :ignored | ^ false ]
%

category: 'rowan'
method: RowanProjectService
isSkew
	| repositorySha |
	name isNil ifTrue:[^false].
	self existsOnDisk ifFalse:[^false]. 
	repositorySha := [self repositorySha] on: Error do:[:ex | repositorySha := 'not on disk'].
	^self sha ~= repositorySha
%

category: 'client commands'
method: RowanProjectService
loadProjectFromFile: fileString
	self loadProjectFromFile: fileString projectsHome: nil
%

category: 'client commands'
method: RowanProjectService
loadProjectFromFile: fileString projectsHome: projectsHomePath
	self
		basicLoadProject: [ 
			((RwSpecification fromUrl: fileString)
				projectsHome: projectsHomePath;
				yourself) resolveStrict load ]
%

category: 'client commands'
method: RowanProjectService
loadProjectFromFile: fileString projectsHome: projectsHomePath customConditionalAttributes: attributes componentNames: componentNames
	self
		basicLoadProject: [ 
			((RwSpecification fromUrl: fileString)
				customConditionalAttributes: attributes;
				projectsHome: projectsHomePath;
				componentNames: componentNames;
				yourself) resolveStrict load ]
%

category: 'client commands'
method: RowanProjectService
loadProjectNamed: aName
	self basicLoadProject: [ Rowan projectTools load loadProjectNamed: aName ]
%

category: 'rowan'
method: RowanProjectService
log

	^Rowan projectTools log
		commitLogProjectNamed: name
		limit: 25
%

category: 'rsr'
method: RowanProjectService
massageServiceForRsrTransportWithVisitLog: visitLog

	rwProject := nil. 
	^super massageServiceForRsrTransportWithVisitLog: visitLog
%

category: 'accessing'
method: RowanProjectService
name

	^name
%

category: 'accessing'
method: RowanProjectService
name: anObject

	name := anObject
%

category: 'client commands'
method: RowanProjectService
newGitProject: url root: rootPath useSsh: useSsh
	"not valid in Rowan V2.0"
	"set useSsh to false to clone using https:"

	"Rowan projectTools clone
		cloneSpecUrl: url
		gitRootPath: rootPath
		useSsh: useSsh.
	(RowanBrowserService new organizer: organizer) updateProjects."
%

category: 'accessing'
method: RowanProjectService
packageGroups: object
	packageGroups := object
%

category: 'rowan'
method: RowanProjectService
packageNames
	"if no project is selected, return all package names"
	^name isNil ifTrue:[
		"list of visible packageNames for current user"
		Rowan image packageNames ]
	ifFalse:[
		"list of visible packageNames for current user and named project"
		Rowan image packageNamesForLoadedProjectNamed: name ]
%

category: 'rowan'
method: RowanProjectService
packageNameString
		"return a string showing the package names for a project"

	| ws packageNames |
	ws := WriteStream on: String new.
	ws
		nextPutAll: 'Packages for project: ' , name;
		cr.
	packageNames := Set new.
	packageNames addAll: (Rowan image loadedProjectNamed: name) packageNames.
	packageNames asSortedCollection do: 
			[:packageName |
			ws
				cr; 
				tab;
				nextPutAll: packageName
			].
	^ws contents
%

category: 'accessing'
method: RowanProjectService
packages
	^packages
%

category: 'accessing'
method: RowanProjectService
packages: object
	packages := object
%

category: 'accessing'
method: RowanProjectService
packageServices

	^self packageNames collect:[:packageName | RowanPackageService forPackageNamed: packageName]
%

category: 'client commands'
method: RowanProjectService
performGitCommand: gitCommand with: argsString
  | project |
  project := RwProject newNamed: name.
  Rowan gitTools
    performGitCommand: gitCommand
    in: project repositoryRootPath
    with: argsString.
  RowanCommandResult addResult: self
%

category: 'printing'
method: RowanProjectService
printOn: aStream

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
%

category: 'testing'
method: RowanProjectService
projectIsLoaded

	^(Rowan image
		loadedProjectNamed: name
		ifAbsent: []) notNil
%

category: 'accessing'
method: RowanProjectService
projectOop
	^projectOop
%

category: 'accessing'
method: RowanProjectService
projectOop: object
	projectOop := object
%

category: 'rowan'
method: RowanProjectService
projects

		^Rowan projectNames collect: 
			[:string |
			| service |
			service := self class new name: string. 
			service 
				sha: service rowanSha;
				branch: service rowanBranch;
				isDirty: service rowanDirty]
%

category: 'accessing'
method: RowanProjectService
projectUrl

	^projectUrl
%

category: 'accessing'
method: RowanProjectService
projectUrl: anObject

	projectUrl := anObject
%

category: 'client commands'
method: RowanProjectService
pullFromGit

	| project |
	project := self rwProject. 
	Rowan gitTools
		gitpullIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName
%

category: 'client commands'
method: RowanProjectService
pushToGit

	| project |
	project := self rwProject. 
	Rowan gitTools
		gitpushIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName
%

category: 'initialization'
method: RowanProjectService
refresh
	self basicRefresh. 
	isLoaded ifTrue:[
		packages := self packageServices].
%

category: 'client commands'
method: RowanProjectService
reloadProject
	self basicLoadProject: [ self rwProject reload ]
%

category: 'client commands'
method: RowanProjectService
removePackagesNamed: packageServices
	| loadedRwPackage |
	packageServices
		do: [ :packageService | 
			loadedRwPackage := Rowan image
				loadedPackageNamed: packageService name
				ifAbsent: [  ].
			loadedRwPackage
				ifNotNil: [ self rwProject removePackageNamed: packageService name ] ].
%

category: 'rowan'
method: RowanProjectService
removeProjectNamed: projectName
   "remove project"
 
    (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
      ifNotNil: [ :project | Rowan image _removeLoadedProject: project ]
%

category: 'rowan'
method: RowanProjectService
repositoryRoot
	^ self rwProject repositoryRoot
%

category: 'rowan'
method: RowanProjectService
repositoryRootPath

	^self repositoryRoot pathString
%

category: 'rowan'
method: RowanProjectService
repositorySha

	^ self rwProject commitId
%

category: 'rowan'
method: RowanProjectService
rowanBranch
	
	name isNil ifTrue:[^String new].
	^ [  self rwProject currentBranchName ] on: Error do: [:ex | ^'ERROR getting repository branch' ]
%

category: 'rowan'
method: RowanProjectService
rowanDirty

	^(RwProject newNamed: name) isDirty
%

category: 'rowan'
method: RowanProjectService
rowanDiskSha

	name isNil ifTrue:[^0].
	^self rwProject commitId
%

category: 'rowan'
method: RowanProjectService
rowanProjectName

	^name
%

category: 'rowan'
method: RowanProjectService
rowanProjectUrl

	^self rwProject projectUrl
%

category: 'rowan'
method: RowanProjectService
rowanSha

	name isNil ifTrue:[^0].
	^self rwProject loadedCommitId
%

category: 'rowan'
method: RowanProjectService
rowanSkew

	^self sha ~= self repositorySha
%

category: 'accessing'
method: RowanProjectService
rwProject
	^ rwProject
		ifNil: [ 
			rwProject := RwProject newNamed: name. 
			projectOop := rwProject asOop.
			rwProject ]
%

category: 'perform'
method: RowanProjectService
servicePerform: symbol withArguments: collection
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
%

category: 'rowan'
method: RowanProjectService
setExistsOnDisk
	"might be a better test than #repositorySha for
	determining if a project exists on disk."

	existsOnDisk := (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
		ifNil: [ false ]
		ifNotNil: [ self rwProject existsOnDisk ]
%

category: 'accessing'
method: RowanProjectService
sha

	name isNil ifTrue:[^0].
	^self rwProject loadedCommitId
%

category: 'accessing'
method: RowanProjectService
sha: anObject

	"because skew is intimately associated with sha
	set it here" 
	sha := anObject.
	isSkew := self rowanSkew
%

category: 'accessing'
method: RowanProjectService
specService
	^specService
%

category: 'accessing'
method: RowanProjectService
specService: object
	specService := object
%

category: 'rowan'
method: RowanProjectService
unload

	| loadedProject |
	loadedProject := Rowan image loadedProjects select:[:proj | proj name = name].
%

category: 'update'
method: RowanProjectService
update
	super update.
	self refresh.
	isLoaded
		ifFalse: [ ^ self ].
	name
		ifNotNil: [ 
			self initializePackageGroups.
			self defaultSymbolDictionaryFromLoadSpec ].
	shouldUpdate := false
%

category: 'update'
method: RowanProjectService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: packages size do:[:index |
		| packageService |
		packageService := packages at: index. 
		packageService = updatedService ifTrue:[
			packages at: index put: updatedService
		]].
%

category: 'other'
method: RowanProjectService
updateIsDirty

	isDirty := self rowanDirty
%

category: 'testing'
method: RowanProjectService
wasDeleted
  ^ self projectIsLoaded not
%

category: 'client commands'
method: RowanProjectService
write
	Rowan projectTools write writeProjectNamed: name
%

category: 'accessing'
method: RowanProjectService
_isSkew

	^isSkew
%

! Class implementation for 'RowanDefinedProjectService'

!		Instance methods for 'RowanDefinedProjectService'

category: 'testing'
method: RowanDefinedProjectService
isDefinedProject

	^true
%

category: 'client commands'
method: RowanDefinedProjectService
load
%

category: 'client commands'
method: RowanDefinedProjectService
resolve
%

! Class implementation for 'RowanQueryService'

!		Class methods for 'RowanQueryService'

category: 'rsr'
classmethod: RowanQueryService
templateClassName

	^#RowanQueryService
%

!		Instance methods for 'RowanQueryService'

category: 'private'
method: RowanQueryService
basicBreakpointMethods
	| bpMethods bpArray |
	bpMethods := Array new.
	bpArray := (GsNMethod _breakReport: true) at: 2.
	bpArray do: [ :array | bpMethods add: (array at: 5) ].
	bpMethods := bpMethods select: [ :bpMethod | bpMethod inClass notNil ].	"ignore anonymous method breakpoints"
	^ (self methodServicesFrom: bpMethods) asSet asArray
%

category: 'queries'
method: RowanQueryService
breakPointMethods
  queryResults := self basicBreakpointMethods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
browseClassReferences: className

	| methods |
	methods := self organizer referencesTo: className asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
browseReferencesTo: symbol

	| methods |
	methods := self organizer referencesTo: symbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
%

category: 'private'
method: RowanQueryService
defaultProjectLogSize

	^100
%

category: 'queries'
method: RowanQueryService
gitTagListUsing: projectService
  | answerString readStream |
  Rowan gitTools
    performGitCommand: 'fetch'
    in: projectService repositoryRootPath
    with: '--tags'.
  answerString := Rowan gitTools
    performGitCommand: 'tag'
    in: projectService repositoryRootPath
    with: '--sort=-taggerdate'.
  queryResults := Array new.
  readStream := ReadStream on: answerString.
  [ readStream atEnd ]
    whileFalse: [ queryResults add: (readStream upTo: Character lf) ].
  RowanCommandResult addResult: self
%

category: 'queries'
method: RowanQueryService
hierarchyImplementorsOf: selector inClass: className

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (self organizer allSubclassesOf: behavior). 
	methods := self organizer implementorsOf: selector in: classes.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
hierarchySendersOf: selector inClass: className

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (self organizer allSubclassesOf: behavior). 
	methods := self organizer sendersOf: selector in: classes.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
implementorsOf: selector

	| methods |
	methods := self organizer implementorsOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
implementorsOfAll: selectors

	| methods |
	methods := Array new. 
	selectors do:[:selector | 
	methods addAll: (self organizer implementorsOf: selector asSymbol)].
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
instVarReaders: instVarName in: className
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (self organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsRead includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
instVarWriters: instVarName in: className
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (self organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsWritten includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
%

category: 'queries'
method: RowanQueryService
literalReferences: string

	| methods compilationResult |
	compilationResult := string evaluate.  
	methods := self organizer referencesToLiteral: compilationResult.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods.
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
methodsContaining: string
  | methods sorted |
  methods := self organizer substringSearch: string.
  sorted := SortedCollection
    sortBlock: [ :x :y | 
      x className = y className
        ifTrue: [ x selector < y selector ]
        ifFalse: [ x className < y className ] ].
  1 to: methods first size do: [ :index | 
    | methodService |
    methodService := self methodServiceFrom: (methods first at: index).
    methodService
      firstReference: ((methods at: 2) at: index);
      searchString: string.
    sorted add: methodService ].
  queryResults := sorted asArray.
  self returnQueryToClient
%

category: 'query support'
method: RowanQueryService
methodServiceFrom: method
  ^ (self methodServicesFrom: (Array with: method)) first
%

category: 'query support'
method: RowanQueryService
methodServicesFrom: methods

	| sorted | 
	sorted := SortedCollection sortBlock: [:x :y | x className = y className ifTrue:[x selector < y selector] ifFalse:[x className < y className]].
	sorted addAll: (methods collect:[:gsNMethod | 
			RowanMethodService forSelector: gsNMethod selector class: gsNMethod inClass thisClass meta: gsNMethod inClass isMeta organizer: self organizer]).
	^sorted asArray
%

category: 'queries'
method: RowanQueryService
projectBranches: projectName

	| project  |
	project := (RwProject newNamed: projectName). 
	queryResults := Rowan gitTools gitbranchIn: project repositoryRootPath with: ''.
	RowanCommandResult addResult: self
%

category: 'queries'
method: RowanQueryService
projectLog: projectName

	queryResults := (RwProject newNamed: projectName) commitLog: self defaultProjectLogSize.
	RowanCommandResult addResult: self.
%

category: 'queryResults'
method: RowanQueryService
queryResults

	"for tests. So far, not needed on server." 

	^queryResults
%

category: 'private'
method: RowanQueryService
returnQueryToClient

	queryResults do:[:service |
		RowanCommandResult addResult: service].
	RowanCommandResult addResult: self.
%

category: 'queries'
method: RowanQueryService
sendersOf: selector

	| methods |
	methods := self organizer sendersOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
%

category: 'queries'
method: RowanQueryService
sendersOfAll: selectors

	| methods |
	methods := Array new. 
	selectors do:[:selector | 
	methods addAll: (self organizer sendersOf: selector asSymbol) first].
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
%

category: 'queryResults'
method: RowanQueryService
setFirstReferenceUsing: results and: methods
	results
		do: [ :methodService | 
			| gsMethod index |
			gsMethod := methods first
				detect: [ :meth | 
					meth selector = methodService selector
						and: [ meth inClass thisClass name asString = methodService className ] ]
				ifNone: [ 
					methodService firstReference: nil.
					nil ].
			gsMethod
				ifNotNil: [ 
					index := methods first indexOf: gsMethod.
					methodService
						firstReference:
							(methods last
								at: index
								ifAbsent: [ 
									"optimized selector perhaps"
									methodService source indexOfSubCollection: self commandArgs first asString ]) ] ]
%

category: 'ston'
method: RowanQueryService
stonOn: stonWriter   
	| instanceVariableNames |
	instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
	stonWriter writeObject: self
		streamMap: 
			[:dictionary |
			instanceVariableNames do: 
					[:each |
					(self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
						ifNotNil: [:value | dictionary at: each asSymbol put: value]
						ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
%

category: 'update'
method: RowanQueryService
updateInternalService: updatedService

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 
	1 to: queryResults size do:[:index |
		| service |
		service := queryResults at: index. 
		service = updatedService ifTrue:[ 
			queryResults at: index put: updatedService
		]].
%

! Class implementation for 'RowanTestService'

!		Class methods for 'RowanTestService'

category: 'rsr'
classmethod: RowanTestService
templateClassName

	^#RowanTestService
%

!		Instance methods for 'RowanTestService'

category: 'sample projects'
method: RowanTestService
addRowanSample1Class
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Core'.
	packageService
		createClassNamed: 'RowanSample1'
		superclass: 'Object'
		instVars: Array new
%

category: 'sample projects'
method: RowanTestService
addRowanSample1Methods
	| classService |
	classService := RowanClassService new
		name: 'RowanSample1';
		update.
	classService organizer: self organizer.
	classService saveMethodSource: 'foo ^''foo''' category: 'accessing'.
	classService meta: true.
	classService
		saveMethodSource: 'bar ^''bar'''
		category: '*rowansample1-extensions'
%

category: 'sample projects'
method: RowanTestService
addRowanSample1Test
	| packageService |
	packageService := RowanPackageService new name: 'RowanSample1-Tests'.
	packageService
		createClassNamed: 'RowanSample1Test'
		superclass: 'TestCase'
		instVars: Array new
%

category: 'sample projects'
method: RowanTestService
addRowanSample1TestMethods

	| classService |
	classService := RowanClassService new name: 'RowanSample1Test';
		update.
	classService organizer: self organizer. 
	classService saveMethodSource: 'test1
	
	self assert: RowanSample1 new foo = ''foo''' category: 'tests'.
		
	classService saveMethodSource: 'test2

	self assert: RowanSample1 bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testError

	self assert: RowanSample1 new bar = ''bar''' category: 'tests'.
	
	classService saveMethodSource: 'testFailure

	self assert: RowanSample1 new foo = ''bar''' category: 'tests'.
%

category: 'sample projects'
method: RowanTestService
createRowanSample1Project
	| projectService  |
	projectService := RowanProjectService new name: 'RowanSample1'.
	projectService
		createProjectComponent: 'Core'
		symDict: 'SampleSymbolDict'
		convention: 'RowanHybrid'
		format: 'tonel'
		projectsHome: '$ROWAN_PROJECTS_HOME'
		type: #'none'
		shouldWrite: false.
	projectService
		addPackagesNamed: self rowanSample1PackageNames.
	self addRowanSample1Class.
	self addRowanSample1Methods.  
	self addRowanSample1Test.
	self addRowanSample1TestMethods.
%

category: 'sample projects'
method: RowanTestService
rowanSample1PackageNames

	^ self rowanSample1PackageToClassDictionary keys
%

category: 'sample projects'
method: RowanTestService
rowanSample1PackageToClassDictionary
	| dictionary |
	dictionary := Dictionary new.
	dictionary at: 'RowanSample1-Core' put: (Array with:  'RowanSample1').
	dictionary at: 'RowanSample1-Extensions' put: (Array new). 
	dictionary at:  'RowanSample1-Tests'  put: (Array with: 'RowanSample1Test'). 
	^dictionary
%

! Class implementation for 'RowanTranscriptService'

!		Class methods for 'RowanTranscriptService'

category: 'rsr'
classmethod: RowanTranscriptService
templateClassName

	^#RowanTranscriptService
%

!		Instance methods for 'RowanTranscriptService'

category: 'actions'
method: RowanTranscriptService
installTranscript
	SessionTemps current at: #'TranscriptStream_SessionStream' put: nil.
	RowanAnsweringService new flipTranscript.
	self jadeiteServer transcriptService: self.
%

category: 'actions'
method: RowanTranscriptService
nextPutAll: aString
	| promise |
	string := aString. 
	promise := remoteSelf nextPutAll.
	promise wait.
%

! Class implementation for 'RowanVariableService'

!		Class methods for 'RowanVariableService'

category: 'other'
classmethod: RowanVariableService
oop: anInteger key: nameString value: valueString className: classNameString

	^self basicNew
		oop: anInteger key: nameString value: valueString className: classNameString;
		yourself
%

category: 'other'
classmethod: RowanVariableService
templateClassName 

	^#RowanVariableService
%

!		Instance methods for 'RowanVariableService'

category: 'other'
method: RowanVariableService
oop: anInteger key: nameString value: valueString className: classNameString

	oop := anInteger.
	key := nameString.
	value := valueString.
	className := classNameString.
%

! Class implementation for 'RowanVariableServiceServer'

!		Class methods for 'RowanVariableServiceServer'

category: 'ston'
classmethod: RowanVariableServiceServer
stonName
	"JfD does not know about this class yet"
	^ 'RowanVariableService'
%

! Class implementation for 'RowanClientServiceTemplateResolver'

!		Instance methods for 'RowanClientServiceTemplateResolver'

category: 'resolving'
method: RowanClientServiceTemplateResolver
clientClassForTemplate: aTemplate ifAbsent: absentBlock
	^ Rowan platform
		serviceClassFor: aTemplate clientClassName
		ifAbsent: [ super clientClassForTemplate: aTemplate ifAbsent: absentBlock ]
%

category: 'resolving'
method: RowanClientServiceTemplateResolver
serverClassForTemplate: aTemplate ifAbsent: absentBlock
	^ Rowan platform
		serviceClassFor: aTemplate serverClassName
		ifAbsent: [ super serverClassForTemplate: aTemplate ifAbsent: absentBlock ]
%

category: 'resolving'
method: RowanClientServiceTemplateResolver
templateNamed: aTemplateName ifAbsent: aBlock
	^ Rowan platform
		serviceClassFor: aTemplateName
		ifAbsent: [ super templateNamed: aTemplateName ifAbsent: aBlock ]
%

! Class extensions for 'RowanMethodService'

!		Instance methods for 'RowanMethodService'

category: '*rowan-services-core-37x'
method: RowanMethodService
_initializeBreakPointsFor: theMethod
  "Answers an Array stepPoints - _allBreakpoints array size changed in 3.7.0"
  | list |
  list := OrderedCollection new.
  theMethod _allBreakpoints
    ifNil: [ ^ OrderedCollection new ]
    ifNotNil: [ :anArray | 
      1 to: anArray size by: 4 do: [ :i | 
        list
          add:
            (theMethod _stepPointForMeth: (anArray at: i + 1) ip: (anArray at: i + 2)) ] ].
  ^ list asOrderedCollection
%

! Class extensions for 'RowanProjectService'

!		Instance methods for 'RowanProjectService'

category: '*rowan-services-corev2'
method: RowanProjectService
addPackageNamed: packageName toComponentNamed: componentName

	Rowan image loadedPackageNamed: packageName ifAbsent: [
		self browserTool addPackageNamed: packageName toComponentNamed: componentName andProjectNamed: name. 
		self update.
		^self answer: #added.].
	self answer: #duplicatePackage
%

! Class extensions for 'RwGsPlatform'

!		Instance methods for 'RwGsPlatform'

category: '*rowan-services-extensions'
method: RwGsPlatform
answeringServiceClass
  ^ RowanAnsweringService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
browserServiceClass

	^ RowanBrowserService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
classServiceClass

	^ RowanClassService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
commandResultClass

	^ RowanCommandResult
%

category: '*rowan-services-extensions'
method: RwGsPlatform
definedProjectServiceClass
	^ RowanDefinedProjectService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
jadeServerClassNamed: className

	| jadeClasses |
	jadeClasses := Array with: (UserGlobals at: #JadeServer). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit32). 
	jadeClasses add: (UserGlobals at: #JadeServer64bit35). 
	^jadeClasses detect:[:cls | cls name == className] ifNone:[self error: 'Could not look up a JadeServer class: ', className]
%

category: '*rowan-services-extensions'
method: RwGsPlatform
loggingServiceClass

	^ RowanLoggingService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
methodServiceClass

	^ RowanMethodService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
packageServiceClass

	^ RowanPackageService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
projectServiceClass

	^ RowanProjectService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClass

	^ RowanService
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClasses
	"Explicitly add each class rather than sending #allSubclasses so
	that users other than SystemUser have visibility. Visibility in Rowan
	is determined at compile time. See STONReader>>lookupClass:"

	| array |
	array := Array with: RowanService.
	array
		add: RowanAnsweringService;
		add: RowanAutoCommitService;
		add: RowanBrowserService;
		add: RowanClassService;
		add: RowanClassDefinitionService;
		add: RowanCompileErrorService;
		add: RowanComponentService;
		add: RowanComponentDefinitionService;
		add: RowanDebuggerService;
		add: RowanDefinedProjectService;
		add: RowanDefinitionService;
		add: RowanDictionaryService;
		add: RowanFileService;
		add: RowanFrameService;
		add: RowanInspectorService;
		add: RowanLoadSpecService;
		add: RowanLoggingService;
		add: RowanMethodService;
		add: RowanMethodDefinitionService; 
		add: RowanPackageService;
		add: RowanPackageDefinitionService;
		add: RowanPackageGroupService;
		add: RowanPackageGroupDefinitionService;
		add: RowanPlatformSubcomponentDefinitionService; 
		add: RowanProcessService;
		add: RowanProjectService;
		add: RowanProjectDefinitionService;
		add: RowanQueryService;
		add: RowanSubcomponentDefinitionService; 
		add: RowanTranscriptService;
		add: RowanTestService;
		add: RowanVariableService;
		
		add: RowanAnsweringServiceServer;
		add: RowanAutoCommitServiceServer;
		add: RowanBrowserServiceServer;
		add: RowanClassServiceServer;
		"add:  RowanClassDefinitionServiceServer;"
		add: RowanCompileErrorServiceServer;
		add: RowanComponentServiceServer;
		add: RowanComponentDefinitionServiceServer;
		"add: RowanDebuggerServiceServer;"
		"add: RowanDefinedProjectServiceServer;"
		"add: RowanDefinitionServiceServer;"
		add: RowanDictionaryServiceServer;
		add: RowanFileServiceServer;
		add: RowanFrameServiceServer;
		add: RowanInspectorServiceServer;
		add: RowanLoadSpecServiceServer;
		"add: RowanLoggingServiceServer;"
		add: RowanMethodServiceServer;
		"add: RowanMethodDefinitionServiceServer; "
		add: RowanPackageServiceServer;
		add: RowanPackageDefinitionServiceServer;
		"add: RowanPackageGroupServiceServer;"
		"add: RowanPackageGroupDefinitionServiceServer;"
		"add: RowanPlatformSubcomponentDefinitionServiceServer; "
		add: RowanProcessServiceServer;
		add: RowanProjectServiceServer;
		add: RowanProjectDefinitionServiceServer;
		add: RowanQueryServiceServer;
		"add: RowanSubcomponentDefinitionServiceServer; "
		add: RowanTranscriptServiceServer;
		add: RowanTestServiceServer;
		add: RowanVariableServiceServer.
	^ array
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClassFor: className

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone:[]
%

category: '*rowan-services-extensions'
method: RwGsPlatform
serviceClassFor: className ifAbsent: absentBlock

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone: absentBlock
%

category: '*rowan-services-extensions'
method: RwGsPlatform
specificationServiceClass
  ^ RowanLoadSpecService
%

