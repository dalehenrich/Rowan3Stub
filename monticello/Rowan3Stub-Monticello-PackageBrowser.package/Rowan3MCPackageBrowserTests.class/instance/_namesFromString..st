private
_namesFromString: aString
	| stream list | 
	stream := aString readStream .
	list := {}.
	[stream atEnd] whileFalse: [
		list add: stream nextLine ].
	^list sorted