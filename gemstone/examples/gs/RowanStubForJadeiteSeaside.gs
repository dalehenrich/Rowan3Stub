! Class Declarations
! Generated file, do not Edit

doit
(Rowan3LoadedPackageStub
	subclass: 'Rowan3SeasideLoadedPackageStub'
	instVarNames: #()
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

removeallmethods Rowan3SeasideLoadedPackageStub
removeallclassmethods Rowan3SeasideLoadedPackageStub

doit
(Rowan3LoadedProjectStub
	subclass: 'Rowan3SeasideLoadedProjectStub'
	instVarNames: #()
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

removeallmethods Rowan3SeasideLoadedProjectStub
removeallclassmethods Rowan3SeasideLoadedProjectStub

! Class implementation for 'Rowan3SeasideLoadedPackageStub'

!		Instance methods for 'Rowan3SeasideLoadedPackageStub'

category: 'accessing'
method: Rowan3SeasideLoadedPackageStub
gs_symbolDictionary
	^ #'UserGlobals'
%

category: 'accessing'
method: Rowan3SeasideLoadedPackageStub
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
method: Rowan3SeasideLoadedPackageStub
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
method: Rowan3SeasideLoadedPackageStub
projectName
	^ Rowan3SeasideLoadedProjectStub monticelloProjectName
%

! Class implementation for 'Rowan3SeasideLoadedProjectStub'

!		Class methods for 'Rowan3SeasideLoadedProjectStub'

category: 'accessing'
classmethod: Rowan3SeasideLoadedProjectStub
monticelloProjectName
	^ 'Seaside'
%

!		Instance methods for 'Rowan3SeasideLoadedProjectStub'

category: 'accessing'
method: Rowan3SeasideLoadedProjectStub
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3SeasideLoadedPackageStub new
		name: aName;
		yourself
%

category: 'accessing'
method: Rowan3SeasideLoadedProjectStub
packageConvention
	^ 'Monticello'
%

category: 'accessing'
method: Rowan3SeasideLoadedProjectStub
packageGroupNames
	^ #()
%

category: 'accessing'
method: Rowan3SeasideLoadedProjectStub
packageNames
	self name = self class monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' self name ].
	^ (Rowan globalNamed: 'MCWorkingCopy')
			ifNil: [ #() ]
			ifNotNil: [:wc |
				(wc allManagers collect: [ :wc | wc packageName ]) sort ]
%

! Class extensions for 'Rowan3ImageStub'

!		Instance methods for 'Rowan3ImageStub'

category: 'accessing'
method: Rowan3ImageStub
loadedProjects
	^ UserGlobals
		at: #'Rowan3StubUserLoadedProjects'
		ifAbsent: [ 
			UserGlobals
				at: #'Rowan3StubUserLoadedProjects'
				put:
					(IdentitySet
						with:
							(Rowan3SeasideLoadedProjectStub new
								name: Rowan3SeasideLoadedProjectStub monticelloProjectName;
								yourself)) ]
%

category: 'querying'
method: Rowan3ImageStub
packageNamesForLoadedProjectNamed: projectName
		projectName = Rowan3SeasideLoadedProjectStub monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ (Rowan globalNamed: 'MCWorkingCopy') 
			ifNil: [ #() ]
			ifNotNil: 
				[:wc | 
					(wc allManagers collect: [ :mgr | mgr packageName ]) sort ]
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
 
    ^ 'seaside'
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

