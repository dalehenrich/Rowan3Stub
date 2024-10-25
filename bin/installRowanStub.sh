
set -ex

export ROWAN_PROJECTS_HOME=/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st/

#newExtent.solo -r 37x -e product/bin/extent0.dbf battery_j
newExtent.solo -r 37x -e product/bin/extent0.seaside.dbf battery_j

startNetldi.solo -r

# refresh the .gs files

if [ "x" = "regenerate" ]; then
	product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs Rowan3Stub-Core

	product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubServices.gs Rowan3Stub-Services

	product/rowan3/bin/exportRowanProjectAsTopaz.solo file: --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs

	product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RowanClientServicesV3/rowan/specs/RowanClientServices.ston --projectAlias=RowanClientServicesV3 --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RowanClientServicesV3.gs

	product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/Announcements/rowan/specs/Announcements.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Announcements.gs

	product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RemoteServiceReplication.gs

	product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanV3/rowan/specs/Rowan.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/GemStoneInteractions.gs GemStone-Interactions-Core GemStone-Interactions-Kernel
fi

source customenv # set $GEMSTONE

$ROWAN_PROJECTS_HOME/Rowan3Stub/bin/installRowanStub.gs -I .topazini -L

