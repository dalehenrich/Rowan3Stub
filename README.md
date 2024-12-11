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

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_metacello.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubMetacello.gs Rowan3Stub-Monticello Rowan3Stub-Metacello

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_base.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubBase.gs Rowan3Stub-Base

product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/Rowan3Stub/rowan/specs/Rowan3Stub_packageBrowser.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Rowan3StubPackageBrowser.gs Rowan3Stub-Monticello-PackageBrowser

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RemoteServiceReplication.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/RowanClientServicesV3/rowan/specs/RowanClientServices.ston --projectAlias=RowanClientServicesV3 --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/RowanClientServicesV3.gs

product/rowan3/bin/exportRowanProjectAsTopaz.solo file:$ROWAN_PROJECTS_HOME/Announcements/rowan/specs/Announcements.ston --projectsHome=$ROWAN_PROJECTS_HOME $ROWAN_PROJECTS_HOME/Rowan3Stub/gs/Announcements.gs

#
# export GemStone-Interactions packages from Rowan 3 using GsDevKit_stones/bin/exportRowanPackagesAsTopaz.solo
#
product/rowan3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$ROWAN_PROJECTS_HOME/RowanV3/rowan/specs/Rowan.ston --projectsHome=$ROWAN_PROJECTS_HOME --fileName=$ROWAN_PROJECTS_HOME/Rowan3Stub/gs/GemStoneInteractions.gs GemStone-Interactions-Core GemStone-Interactions-Kernel

```
### MCPackageBrowser.stone examples
```
# list repositories and packages
MCPackageBrowser.stone --repositories
MCPackageBrowser.stone --repositories --packages
MCPackageBrowser.stone --repositories --modified

# expect an HTTP Error, since http://seaside.gemtalksystems.com is read only
MCPackageBrowser.stone --repository=http://seaside.gemtalksystems.com/ss/MetacelloRepository --write=ConfigurationOfGsOB \
    --commitMessage='testing the MCPackageBrowser.stone --write option'

# list packages
MCPackageBrowser.stone --packages

# describe changes to packages 
MCPackageBrowser.stone --changes

# load package GemStone-Compression-dkh.3 from $GEMSTONE/seaside/monticello/repository
MCPackageBrowser.stone --repository=server://$GEMSTONE/seaside/monticello/repository --load=GemStone-Compression-dkh.3

# create a filetree repository
mkdir //bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/filetree
MCPackageBrowser.stone --createRepository=filetree:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/filetree
MCPackageBrowser.stone --repositories | grep filetree

# create a monticello repository
mkdir //bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz
MCPackageBrowser.stone --createRepository=server:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz
MCPackageBrowser.stone --repositories | grep mcz

# list versions of a package in a repository
MCPackageBrowser.stone --versions=ConfigurationOfGLASS --repository=http://seaside.gemtalksystems.com/ss/MetacelloRepository
MCPackageBrowser.stone --versions=GemStone-Compression --repository=server://$GEMSTONE/seaside/monticello/repository

# create a package and save to filetree repository
MCPackageBrowser.stone --createPackage=XXX-Core
MCPackageBrowser.stone --write=XXX-Core --repository=filetree:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/filetree --commitMessage='testing'
cat /bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/filetree/XXX-Core.package/monticello.meta/version
   
# save package to monticello repository
MCPackageBrowser.stone --write=XXX-Core  --commitMessage='testing' --repository=bosch:/bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz
ls /bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz

# save package to monticello repository with custom version (--version)
MCPackageBrowser.stone --write=XXX-Core --version=XXX-Core-DataCurator.44 --commitMessage='testing' --repository=bosch:/bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz
ls /bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz

# load package XXX-Core-dkh.3 from $GEMSTONE/seaside/monticello/repository
MCPackageBrowser.stone --repository=server:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz --load=XXX-Core-DataCurator.3

# copy a package version (from above) from one repository to another
MCPackageBrowser.stone --copyVersion=XXX-Core-DataCurator.3 --to=filetree:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/filetree --from=server:///bosch1/users/dhenrich/_stones/tode/stones/tode_3.7.2_j/mcz

# unload XXX-Core from image
MCPackageBrowser.stone --unload=XXX-Core
```
