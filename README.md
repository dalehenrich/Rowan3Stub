### GsDevKit_stones Install Script for installing JfPwoR support in a stone
.topazini should be setup to login as SystemUser. 
```
# Installation shell script for extent0.dbf using GsDevKit_stones
export ROWAN_PROJECTS_HOME=/bosch1/users/dhenrich/_stones/37x/h_37x_externals_st
#
# specify the <stone-name> and <registry-name> on the command line ... 
# if 'base' or 'seaside' is specified, then the stone will be restarted with a fresh 
# extent0.dbf or extent0.seaside.dbf. Otherwise the current running stone will used
#
$ROWAN_PROJECTS_HOME/RowanStubForJadeite/bin/instalRowanStub.sh <stone-name> <registry-name> [base | seaside ]
```
### topaz script for installing JfPwoR support in an extent0.dbf stone.
```
set u SystemUser p swordfish
login
# Installation shell script using topaz
run
System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME' put: '/bosch1/users/dhenrich/_stones/37x/h_37x_externals_st'.
System gemEnvironmentVariable: 'ROWAN_STUB_EXTENT_TYPE' put: 'base'.
%
input $ROWAN_PROJECTS_HOME/RowanStubForJadeite/bin/installRowanStub.gs
input $ROWAN_PROJECTS_HOME/RowanStubForJadeite/bin/RowanClassService_base.gs
```

