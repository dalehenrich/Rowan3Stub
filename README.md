### Install Scripts
```
# Installation shell script for extent0.dbf
#		Uses GsDevKit_stones and superdoit script; hardwired paths in the script, so edit before running in your environment

bin/instalRowanStub.sh
```
### Dev Scripts -- should be run using GemStone 3.7.2 or later for solo scripts
```
#
## Create project and package -- these scripts must be run using 3.7.2 or later 
#
product/rowan3/bin/createRowanProject.solo --projectName=Rowan3Stub --projectsHome=/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st --packageConvention=Rowan --repositoryType=git --packageFormat=tonel --defaultSymbolDictName=Globals
/bosch1/users/dhenrich/_stones/37x/stones/rowan3_dev_i/product/rowan3/bin/addRowanProjectPackages.solo file:/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston  --projectsHome=/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st Rowan3Stub-Core
#
## install Rowan3Stub project in Rowan3 image for development
#
installProject.stone file:/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st
#
## export projects (Rowan3Stub, RowanClientServices, Announcements, RemoteServiceReplication to Rowan3Stub/gs
#
product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_core.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3Stub.gs Rowan3Stub-Core

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_core.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubServices.gs Rowan3Stub-Services

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_monticello.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubMonticello.gs Rowan3Stub-Monticello

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_base.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubBase.gs Rowan3Stub-Base

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RemoteServiceReplication.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RowanClientServicesV3/rowan/specs/RowanClientServices.ston --projectAlias=RowanClientServicesV3 --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RowanClientServicesV3.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/Announcements/rowan/specs/Announcements.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Announcements.gs

#
# export GemStone-Interactions packages from Rowan 3 using GsDevKit_stones/bin/exportRowanPackagesAsTopaz.solo
#
product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanV3/rowan/specs/Rowan.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/GemStoneInteractions.gs GemStone-Interactions-Core GemStone-Interactions-Kernel

```
