accessing
removeMethod: selector forClassNamed: name  isMeta: meta
	| beh |
	beh := Rowan globalNamed: name.
	meta ifTrue: [ beh := beh class ].
	beh removeSelector: selector