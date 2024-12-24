Class {
	#name : 'RowanMonticelloService',
	#superclass : 'RowanService',
	#instVars : [
		'changes'
	],
	#category : 'Rowan3Stub-Services'
}

{ #category : 'accesing' }
RowanMonticelloService class >> templateClassName [

	^#RowanMonticelloService
]

{ #category : 'client commands' }
RowanMonticelloService >> changes [
	| jadeServer modifiedMCPackages |
	jadeServer := Rowan jadeServerClassNamed: #'JadeServer'.
	modifiedMCPackages := (Rowan globalNamed: 'MCWorkingCopy') allManagers
		select: [ :wc | wc modified ].
	changes := Array new.
	modifiedMCPackages
		collect: [ :wc | 
			| patch packageName |
			patch := wc
				changesRelativeToRepository: wc repositoryGroup repositories last.
			packageName := wc packageName.
			changes
				add:
					(jadeServer new
						_mcDescriptionOfPatch: patch
						baseName: 'closest ancestor'
						alternateName: nil
						packageName: packageName) ].
	RowanCommandResult addResult: self
]

{ #category : 'client commands' }
RowanMonticelloService >> write: commitComment [
	|  modifiedMCPackages browser |
	modifiedMCPackages := (Rowan globalNamed: 'MCWorkingCopy') allManagers
		select: [ :wc | wc modified ].
	browser := (Rowan globalNamed: 'Rowan3MCPackageBrowser') new.
	modifiedMCPackages
		do: [ :wc | 
			| repository |
			repository := wc repositoryGroup repositories last.
			browser 
				writePackageNamed: wc  packageName 
				toRepository: repository 
				commitComment: commitComment
].
	RowanCommandResult addResult: self
]
