accessing
metacelloProjectRegistrations

	^((Rowan globalNamed: 'MetacelloProjectRegistration') registry projectSpecs
    collect: [ :projectSpec | 
      (Rowan globalNamed: 'MetacelloProjectRegistration')
        registrationForProjectSpec: projectSpec
        ifAbsent: [ self error: 'registration for projectSpec: ' projectSpec name , ' not found' ]
        ifPresent: [ :registration :ignored | (Rowan globalNamed: 'TDMetacelloRegistrationDefinition') registration: registration ] ])