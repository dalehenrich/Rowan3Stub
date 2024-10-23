### Install Scripts
```
# Installation shell script for extent0.dbf
#		Uses GsDevKit_stones and superdoit script; hardwired paths in the script, so edit before running in your environment

bin/instalRowanStub.sh
```
### Dev Scripts
```
#
## Create project and package
#
product/rowan3/bin/createRowanProject.solo --projectName=Rowan3Stub --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st --packageConvention=Rowan --repositoryType=git --packageFormat=tonel --defaultSymbolDictName=Globals
/bosch1/users/dhenrich/_stones/37x/stones/rowan3_dev_i/product/rowan3/bin/addRowanProjectPackages.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston  --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st Rowan3Stub-Core
#
## install Rowan3Stub project in Rowan3 image for development
#
installProject.stone file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st
#
## export projects (Rowan3Stub, RowanClientServices, Announcements to Rowan3Stub/gs
#
product/rowan3/bin/exportRowanProjectAsTopaz.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st /bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/gs/Rowan3Stub.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/RowanClientServicesV3/rowan/specs/RowanClientServices.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st --projectAlias=RowanClientServicesV3 /bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/gs/RowanClientServicesV3.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Announcements/rowan/specs/Announcements.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st /bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/gs/Announcements.gs

#
# export GemStone-Interactions packages from Rowan 3 using GsDevKit_stones/bin/exportRowanPackagesAsTopaz.solo
#
exportRowanPackagesAsTopaz.solo --loadSpec=file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/RowanV3/rowan/specs/Rowan.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st --fileName=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/gs/GemStoneInteractions GemStone-Interactions-Core GemStone-Interactions-Kernel -D

```
