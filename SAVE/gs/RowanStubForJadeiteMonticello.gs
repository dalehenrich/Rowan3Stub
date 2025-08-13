! Class Declarations
! Generated file, do not Edit

doit
(Rowan3LoadedPackageStub
	subclass: 'Rowan3MonticelloLoadedPackageStub'
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

doit
(Rowan3LoadedProjectStub
	subclass: 'Rowan3MonticelloLoadedProjectStub'
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

! Class implementation for 'Rowan3MonticelloLoadedPackageStub'

!		Instance methods for 'Rowan3MonticelloLoadedPackageStub'

category: 'accessing'
method: Rowan3MonticelloLoadedPackageStub
gs_symbolDictionary
	^ #'UserGlobals'
%

category: 'accessing'
method: Rowan3MonticelloLoadedPackageStub
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
method: Rowan3MonticelloLoadedPackageStub
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
method: Rowan3MonticelloLoadedPackageStub
projectName
	^ Rowan3MonticelloLoadedProjectStub monticelloProjectName
%

! Class implementation for 'Rowan3MonticelloLoadedProjectStub'

!		Class methods for 'Rowan3MonticelloLoadedProjectStub'

category: 'accessing'
classmethod: Rowan3MonticelloLoadedProjectStub
monticelloProjectName
	^ 'Monticello'
%

!		Instance methods for 'Rowan3MonticelloLoadedProjectStub'

category: 'accessing'
method: Rowan3MonticelloLoadedProjectStub
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3MonticelloLoadedPackageStub new
		name: aName;
		yourself
%

category: 'accessing'
method: Rowan3MonticelloLoadedProjectStub
packageConvention
	^ 'Monticello'
%

category: 'accessing'
method: Rowan3MonticelloLoadedProjectStub
packageGroupNames
	^ #()
%

category: 'accessing'
method: Rowan3MonticelloLoadedProjectStub
packageNames
	self name = self class monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' self name ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
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
							(Rowan3MonticelloLoadedProjectStub new
								name: Rowan3MonticelloLoadedProjectStub monticelloProjectName;
								yourself)) ]
%

category: 'querying'
method: Rowan3ImageStub
packageNamesForLoadedProjectNamed: projectName
	projectName = Rowan3MonticelloLoadedProjectStub monticelloProjectName
		ifFalse: [ self error: 'unexpected projectName: ' projectName ].
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
%

