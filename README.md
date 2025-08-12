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
$GEMSTONE/jadeite/bin/installRowanStub_stones.sh <stone-name> <registry-name> base
```
### topaz-based script for installing JfPwoR support in an existing extent0.dbf stone.

```
export ROWAN_STUB_EXTENT_TYPE=base

# start topaz
set u SystemUser p swordfish
login

input $GEMSTONE/jadeite/bin/installRowanStub.gs
input $GEMSTONE/jadeite/gs/RowanClassService_base.gs
```

