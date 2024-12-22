Class {
	#name : 'RowanMonticelloService',
	#superclass : 'RowanService',
	#instVars : [
		'packages',
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
]
