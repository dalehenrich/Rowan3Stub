! Class Declarations
! Generated file, do not Edit

doit
(TestCase
	subclass: 'Rowan3MCPackageBrowserTests'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Rowan3Stub-Monticello-PackageBrowser';
		immediateInvariant.
true.
%

! Class implementation for 'Rowan3MCPackageBrowserTests'

!		Instance methods for 'Rowan3MCPackageBrowserTests'

category: 'tests'
method: Rowan3MCPackageBrowserTests
test
	| packageBrowser stream |
	packageBrowser := Rowan3MCPackageBrowser new
		noCommit: true;
		yourself.
	stream := WriteStream on: String new.
	packageBrowser listRepositoriesOn: stream.
	self _assertListMatchesExpectedRepositoryList: stream contents equals: self _expectedHttpRepositoryList
%

category: 'private'
method: Rowan3MCPackageBrowserTests
_assertListMatchesExpectedRepositoryList: repositoryList equals: expectedRepositoryList
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
%

category: 'private'
method: Rowan3MCPackageBrowserTests
_expectedHttpRepositoryList
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
%

