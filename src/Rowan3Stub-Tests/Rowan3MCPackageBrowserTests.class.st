Class {
	#name : 'Rowan3MCPackageBrowserTests',
	#superclass : 'TestCase',
	#category : 'Rowan3Stub-Monticello-PackageBrowser'
}

{ #category : 'private' }
Rowan3MCPackageBrowserTests >> _assertListMatchesExpectedRepositoryList: repositoryList equals: expectedRepositoryList [
	"ignore non http: repositories ... error free functionality is the goal"

	| filteredStream repositoryListStream |
	filteredStream := String new writeStream.
	repositoryListStream := repositoryList readStream.
	[ repositoryListStream atEnd ]
		whileFalse: [ 
			| line |
			line := repositoryListStream nextLine.
			(line beginsWith: 'http:')
				ifTrue: [ 
					filteredStream
						nextPutAll: line;
						lf ] ].
	self assert: expectedRepositoryList equals: filteredStream contents
]

{ #category : 'private' }
Rowan3MCPackageBrowserTests >> _expectedHttpRepositoryList [
	^ 'http://seaside.gemtalksystems.com/ss/announcements
http://seaside.gemtalksystems.com/ss/GemStone
http://seaside.gemtalksystems.com/ss/GLASSClient
http://seaside.gemtalksystems.com/ss/GLASSproject
http://seaside.gemtalksystems.com/ss/hyper
http://seaside.gemtalksystems.com/ss/metacello
http://seaside.gemtalksystems.com/ss/MetacelloRepository
http://seaside.gemtalksystems.com/ss/monticello
http://seaside.gemtalksystems.com/ss/obsunit
http://seaside.gemtalksystems.com/ss/PharoCompat
http://seaside.gemtalksystems.com/ss/QueryExtensions
http://seaside.gemtalksystems.com/ss/Seaside30
http://seaside.gemtalksystems.com/ss/smtp
http://seaside.gemtalksystems.com/ss/VBRegex
http://ss3.gemtalksystems.com/ss/FileTree
http://www.squeaksource.com/Loader
http://www.squeaksource.com/metacello
http://www.squeaksource.com/MetacelloRepository
http://www.squeaksource.com/Seaside30
'
]

{ #category : 'tests' }
Rowan3MCPackageBrowserTests >> test [
	| packageBrowser stream |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listRepositoriesOn: stream.
	self _assertListMatchesExpectedRepositoryList: stream contents equals: self _expectedHttpRepositoryList
]
