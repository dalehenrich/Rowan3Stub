! Class Declarations
! Generated file, do not Edit

doit
(Rowan3LoadedPackageStub
	subclass: 'Rowan3MetacelloLoadedPackageStub'
	instVarNames: #(workingCopy)
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
projectName
	self halt: 'will need an IV for this methinks'
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

	| projectRegistration |
	^((Rowan globalNamed: 'MetacelloProjectRegistration') registry projectSpecs
    collect: [ :projectSpec | 
      (Rowan globalNamed: 'MetacelloProjectRegistration')
        registrationForProjectSpec: projectSpec
        ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        ifPresent: [ :registration :ignored | (Rowan globalNamed: 'TDMetacelloRegistrationDefinition') registration: registration ] ])
%

!		Instance methods for 'Rowan3MetacelloLoadedProjectStub'

category: 'accessing'
method: Rowan3MetacelloLoadedProjectStub
loadedPackageNamed: aName ifAbsent: absentBlock
	(self packageNames includes: aName)
		ifFalse: absentBlock.
	^ Rowan3MetacelloLoadedPackageStub new
		name: aName;
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
	^ self projectRegistration workingCopies collect: [:each | each packageName ]
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
	^ ((Rowan globalNamed: 'MCWorkingCopy') allManagers
		collect: [ :wc | wc packageName ]) sort
%

