Running
tearDown

self assert: (Rowan3MCPackageBrowser new packageNamed: 'ATEST-PACKAGE' ifAbsent: []) equals: nil.	
self assert: (Rowan3MCPackageBrowser new packageNamed: 'ATest-Core' ifAbsent: []) equals: nil.