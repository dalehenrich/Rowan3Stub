! Class Declarations
! Generated file, do not Edit

doit
(RowanService
	subclass: 'RowanMonticelloService'
	instVarNames: #(packages changes)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Services';
		immediateInvariant.
true.
%

doit
(RowanMonticelloService
	subclass: 'RowanMonticelloServiceServer'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Services';
		immediateInvariant.
true.
%

! Class implementation for 'RowanMonticelloService'

!		Class methods for 'RowanMonticelloService'

category: 'accesing'
classmethod: RowanMonticelloService
templateClassName

	^#RowanMonticelloService
%

!		Instance methods for 'RowanMonticelloService'

category: 'client commands'
method: RowanMonticelloService
changes
	| jadeServer modifiedMCPackages |
	jadeServer := Rowan jadeServerClassNamed: #'JadeServer'.
	modifiedMCPackages := (Rowan globalNamed: 'MCWorkingCopy') allManagers
		select: [ :wc | wc modified ].
	changes := Array new.
	modifiedMCPackages
		collect: [ :wc | 
			| patch packageName |
			patch := wc
				changesRelativeToRepository: wc repositoryGroup repositories first.
			packageName := wc packageName.
			changes
				add:
					(jadeServer new
						_mcDescriptionOfPatch: patch
						baseName: 'closest ancestor'
						alternateName: nil) ].
	self refresh.
	RowanCommandResult addResult: self
%

! Class extensions for 'Rowan3PlatformStub'

!		Instance methods for 'Rowan3PlatformStub'

category: 'accessing'
method: Rowan3PlatformStub
commandResultClass
	^ RowanCommandResult
%

category: 'accessing'
method: Rowan3PlatformStub
loggingServiceClass

	^ RowanLoggingService
%

category: 'accessing'
method: Rowan3PlatformStub
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
		add: RowanMonticelloService;
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
		add: RowanMonticelloServiceServer;
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
		add: RowanVariableServiceServer;
		add: RowanCommandResult.	"temporary measure"
	^ array
%

