Class {
	#name : 'Rowan3MCPackageBrowser',
	#superclass : 'Object',
	#instVars : [
		'changes',
		'packages',
		'repositories',
		'modified',
		'noCommit'
	],
	#category : 'Rowan3Stub-Monticello-PackageBrowser'
}

{ #category : 'private' }
Rowan3MCPackageBrowser class >> _allWorkingCopies [
	"if MCWorkingCopy is not available, we are running in an evironment where Monticello has not been installed"

	^ (Rowan globalNamed: 'MCWorkingCopy')
		ifNil: [ #() ]
		ifNotNil: [ :mcWorkingCopy | mcWorkingCopy allManagers ]
]

{ #category : 'private' }
Rowan3MCPackageBrowser class >> _repositoryDescriptions [
	"MCRepositoryGroup holds onto all registered repositores"

	| descriptions |
	descriptions := Set new.
	^ (Rowan globalNamed: 'MCRepositoryGroup')
		ifNil: [ descriptions ]
		ifNotNil: [ :mcRepositoryGroup | 
			mcRepositoryGroup default
				repositoriesDo: [ :repo | descriptions add: repo description ].
			descriptions ]
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> changes [
	^changes
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> changes: object [
	changes := object
]

{ #category : 'operations' }
Rowan3MCPackageBrowser >> listPackageNamesOn:  stream [
	self packageNames sort
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ]
]

{ #category : 'operations' }
Rowan3MCPackageBrowser >> listRepositoriesOn: stream [
	self class _repositoryDescriptions sort
		do: [ :each | 
			stream
				nextPutAll: each;
				lf ]
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> modified [
	^modified
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> modified: object [
	modified := object
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> modifiedPackageNames [
	"return a list of modified Monticello package names in the image"

	^ self modifiedPackages collect: [ :wc | wc packageName ]
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> modifiedPackages [
	"return a list of modified Monticello packages (instances of MCWorkingCopy) in the image"

	^ self class _allWorkingCopies select: [ :wc | wc modified ]
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> noCommit [
	^noCommit
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> noCommit: object [
	noCommit := object
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> packageNames [
	"return a list of Monticello package names in the image"

	^ self class _allWorkingCopies collect: [ :wc | wc packageName ]
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> packages [
	^packages
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> packages: object [
	packages := object
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> repositories [
	^repositories
]

{ #category : 'accessing' }
Rowan3MCPackageBrowser >> repositories: object [
	repositories := object
]
