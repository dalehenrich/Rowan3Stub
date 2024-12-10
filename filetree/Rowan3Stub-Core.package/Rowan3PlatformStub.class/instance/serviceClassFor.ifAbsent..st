accessing
serviceClassFor: className ifAbsent: absentBlock

	^self serviceClasses detect:[:cls | cls name asString = className asString] ifNone: absentBlock