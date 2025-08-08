#
#	The following projects should be present in ROWAN_PROJECTS_HOME:
#		RowanStubForJadeite
#		RemoteServiceReplication
#		RowanClientServicesV3
#		Announcements
#		RowanV3

set -ex

if [ "$ROWAN_PROJECTS_HOME" = "" ]; then
	echo "ERROR -- \$ROWAN_PROJECTS_HOME must be defined"
	exit 1
fi

if [ "$GEMSTONE" = "" ]; then
	echo "ERROR -- \$GEMSTONE must be defined"
	exit 1
fi

export PATH=$GEMSTONE/bin:$PATH

$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
 	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeite.gs RowanStubForJadeite-Core $*

$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteServices.gs RowanStubForJadeite-Services $*

$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_monticello.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteMonticello.gs RowanStubForJadeite-Monticello $*

$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_metacello.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteMetacello.gs RowanStubForJadeite-Monticello RowanStubForJadeite-Metacello $*

$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
		--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanStubForJadeiteBase.gs RowanStubForJadeite-Base

$GEMSTONE/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RemoteServiceReplication.gs $*

$GEMSTONE/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RowanClientServicesV3/rowan/specs/RowanClientServices.ston \
	--projectAlias=RowanClientServicesV3 --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/RowanClientServicesV3.gs $*

$GEMSTONE/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/Announcements/rowan/specs/Announcements.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/Announcements.gs $*


$GEMSTONE/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanV3/rowan/specs/Rowan.ston \
	--projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/RowanStubForJadeite/gs/GemStoneInteractions.gs GemStone-Interactions-Core \
	GemStone-Interactions-Kernel $*

