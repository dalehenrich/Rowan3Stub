! Class Declarations
! Generated file, do not Edit

doit
(Rowan3LoadedPackageStub
	subclass: 'Rowan3BaseLoadedPackageStub'
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
	subclass: 'Rowan3BaseLoadedProjectStub'
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

! Class implementation for 'Rowan3BaseLoadedPackageStub'

!		Instance methods for 'Rowan3BaseLoadedPackageStub'

category: 'accessing'
method: Rowan3BaseLoadedPackageStub
gs_symbolDictionary
	^ #'UserGlobals'
%

category: 'accessing'
method: Rowan3BaseLoadedPackageStub
loadedClasses
	| theLoadedClasses |
	theLoadedClasses := KeyValueDictionary new.
	(ClassOrganizer new categories at: self name ifAbsent: [ ^ theLoadedClasses ])
		do: [ :aClass | 
			theLoadedClasses
				at: aClass name
				put: (Rowan3LoadedClassStub new theClass: aClass) ].
	^ theLoadedClasses
%

category: 'accessing'
method: Rowan3BaseLoadedPackageStub
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
method: Rowan3BaseLoadedPackageStub
projectName
	^ Rowan3BaseLoadedProjectStub baseProjectName
%

! Class implementation for 'Rowan3BaseLoadedProjectStub'

!		Class methods for 'Rowan3BaseLoadedProjectStub'

category: 'accessing'
classmethod: Rowan3BaseLoadedProjectStub
baseProjectName
	^ 'Image'
%

!		Instance methods for 'Rowan3BaseLoadedProjectStub'

category: 'accessing'
method: Rowan3BaseLoadedProjectStub
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3BaseLoadedPackageStub new
		name: aName;
		yourself
%

category: 'accessing'
method: Rowan3BaseLoadedProjectStub
packageConvention
	^ 'Rowan'
%

category: 'accessing'
method: Rowan3BaseLoadedProjectStub
packageGroupNames
	^ #()
%

category: 'accessing'
method: Rowan3BaseLoadedProjectStub
packageNames
	^#()
%

! Class extensions for 'Rowan3ImageStub'

!		Instance methods for 'Rowan3ImageStub'

category: 'accessing'
method: Rowan3ImageStub
loadedProjects
	^ #()
%

category: 'querying'
method: Rowan3ImageStub
packageNamesForLoadedProjectNamed: projectName
	^#()
%

