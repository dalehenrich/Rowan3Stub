
# install JadeiteForPharo support in a non-Rowan stone

input $ROWAN_PROJECTS_HOME/RemoteServiceReplication/src-gs/bootstrapRSR.gs
input $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs

run

(Published at: #Rowan ifAbsent: [])
	ifNotNil: [ self error: 'Rowan is already installed!!' ].

Published at: #Rowan put: Rowan3Stub new.


%
