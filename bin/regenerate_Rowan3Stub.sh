set -ex

export ROWAN_PROJECTS_HOME=/bosch1/users/dhenrich/_stones/37x/h_37x_externals_st/

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
 	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeite.gs RowanStubForJadeite-Core $*

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteServices.gs RowanStubForJadeite-Services $*

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_monticello.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteMonticello.gs RowanStubForJadeite-Monticello $*

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_metacello.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteMetacello.gs RowanStubForJadeite-Monticello RowanStubForJadeite-Metacello $*

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
		--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteBase.gs RowanStubForJadeite-Base

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RemoteServiceReplication.gs $*

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RowanClientServicesV3/rowan/specs/RowanClientServices.ston \
	--projectAlias=RowanClientServicesV3 --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanClientServicesV3.gs $*

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/Announcements/rowan/specs/Announcements.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/Announcements.gs $*


product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanV3/rowan/specs/Rowan.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/GemStoneInteractions.gs GemStone-Interactions-Core \
	GemStone-Interactions-Kernel $*

