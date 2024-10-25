Extension { #name : 'Rowan3PlatformStub' }

{ #category : 'accessing' }
Rowan3PlatformStub >> commandResultClass [
	^ RowanCommandResult
]

{ #category : 'accessing' }
Rowan3PlatformStub >> serviceClasses [
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
]
