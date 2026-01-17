! Class Declarations
! Generated file, do not Edit

doit
(Rowan3LoadedPackageStub
	subclass: 'Rowan3MetacelloLoadedPackageStub'
	instVarNames: #(workingCopy loadedProject)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

removeallmethods Rowan3MetacelloLoadedPackageStub
removeallclassmethods Rowan3MetacelloLoadedPackageStub

doit
(Rowan3LoadedProjectStub
	subclass: 'Rowan3MetacelloLoadedProjectStub'
	instVarNames: #(projectRegistration)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Core';
		immediateInvariant.
true.
%

removeallmethods Rowan3MetacelloLoadedProjectStub
removeallclassmethods Rowan3MetacelloLoadedProjectStub

! Class implementation for 'Rowan3MetacelloLoadedPackageStub'

!		Instance methods for 'Rowan3MetacelloLoadedPackageStub'

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
gs_symbolDictionary
	^ #'UserGlobals'
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
loadedClasses
	| theLoadedClasses classes organizer |
	theLoadedClasses := KeyValueDictionary new.
	classes := IdentitySet new.
	organizer := ClassOrganizer new.
	((GsSession currentSession objectNamed: #'MCPackage') named: self name)
		packageInfo systemCategories
		do: [ :cat | 
			(organizer categories at: cat)
				do: [ :aClass | 
					theLoadedClasses
						at: aClass name
						put: (Rowan3LoadedClassStub new theClass: aClass) ] ].
	^ theLoadedClasses
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
loadedClassExtensions
	| theExtendedClasses packageInfo extensionClasses |
	theExtendedClasses := KeyValueDictionary new.
	extensionClasses := IdentitySet new.
	packageInfo := (Rowan globalNamed: 'PackageInfo') named: self name.
	packageInfo extensionClasses
		do: [ :aBehavior | extensionClasses add: aBehavior theNonMetaClass ].
	extensionClasses
		do: [ :aClass | 
			theExtendedClasses
				at: aClass name
				put: (Rowan3LoadedClassExtensionStub new theClass: aClass) ].
	^ theExtendedClasses
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
loadedProject
	^loadedProject
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
loadedProject: object
	loadedProject := object
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
projectName
	^ self loadedProject name
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
workingCopy
	^ workingCopy
%

category: 'accessing'
method: Rowan3MetacelloLoadedPackageStub
workingCopy: aMCWorkingCopy
	workingCopy := aMCWorkingCopy
%

! Class implementation for 'Rowan3MetacelloLoadedProjectStub'

!		Class methods for 'Rowan3MetacelloLoadedProjectStub'

category: 'accessing'
classmethod: Rowan3MetacelloLoadedProjectStub
metacelloProjectRegistrations

	(Rowan globalNamed: 'MetacelloProjectRegistration')
		ifNil: [ ^ #() ]
		ifNotNil: [:theMetacelloProjectRegistrationClass |
				"if logging in as SystemUser, then these classes aren't present"
				^(theMetacelloProjectRegistrationClass registry projectSpecs
    			collect: [ :projectSpec | 
      			(Rowan globalNamed: 'MetacelloProjectRegistration')
        			registrationForProjectSpec: projectSpec
        			ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        			ifPresent: [ :registration :ignored |  registration ] ]) ]
%

!		Instance methods for 'Rowan3MetacelloLoadedProjectStub'

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
loadedPackageNamed: aPackageName ifAbsent: absentBlock
	| pr mcVersion wc |
	pr := self projectRegistration.
	mcVersion := pr configurationProjectSpec project map at: pr configurationProjectSpec versionString.
	wc := mcVersion packages detect: [:each | each name = aPackageName ]
		ifNone: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aPackageName;
		loadedProject: self;
		workingCopy: wc;
		yourself
%

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
packageConvention
	^ 'Monticello'
%

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
packageGroupNames
	^ #()
%

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
packageNames
	| pr mcVersion |
	pr := self projectRegistration.
	mcVersion := pr configurationProjectSpec project map at: pr configurationProjectSpec versionString.
	^ mcVersion packages collect: [:each | each name ]
%

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
projectRegistration
	^ projectRegistration
%

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
projectRegistration: aTDMetacelloRegistrationDefinition
	projectRegistration := aTDMetacelloRegistrationDefinition
%

! Class extensions for 'Rowan3ImageStub'

!		Instance methods for 'Rowan3ImageStub'

category: 'accessing'
method: Rowan3ImageStub
loadedProjects
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			| loadedProjects |
			loadedProjects := IdentitySet new.
			Rowan3MetacelloLoadedProjectStub metacelloProjectRegistrations do: [:projectRegistration |
				loadedProjects add: (Rowan3MetacelloLoadedProjectStub new
								name: projectRegistration projectName;
								projectRegistration: projectRegistration; 
								yourself) ].
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:loadedProjects ]
%

category: 'querying'
method: Rowan3ImageStub
packageNamesForLoadedProjectNamed: projectName
	^ (self loadedProjectNamed: projectName) packageNames
%

category: 'accessing'
method: Rowan3ImageStub
projectNamed: projectName

	^ self loadedProjectNamed: projectName
%

! Class extensions for 'RowanStubForJadeite'

!		Instance methods for 'RowanStubForJadeite'

category: 'accessing'
method: RowanStubForJadeite
extentType
	"Create a new initialized instance of the receiver."

	^ 'metacello'
%

category: 'accessing'
method: RowanStubForJadeite
projectNamed: projectName
	^ self image loadedProjectNamed: projectName
%

category: 'accessing'
method: RowanStubForJadeite
projectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock
 
    ^self image loadedProjectNamed: projectName ifPresent: ifPresentBlock ifAbsent: ifAbsentBlock.
%

