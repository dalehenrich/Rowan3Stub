# Install instructions for installing JfPwoR support in a stone
The env var GEMSTONE needs to be defined to point at GemStone 3.7.5 and $GEMSTONE/bin should be in your path.

The .topazini file (in the current directory) should be setup to login as SystemUser in your target stone. 

### Installation shell script for extent0.dbf for GsDevKit_stones.
```
#
# Specify the <stone-name> and <registry-name> on the command line ... 
# if 'base' is specified, then the stone will be restarted with a fresh 
# extent0.dbf, otherwise the currently running stone will used
#
$GEMSTONE/jadeite/bin/installRowanStub_stones.sh <stone-name> <registry-name> base
```
### topaz-based script for installing JfPwoR support in an existing extent0.dbf stone.
```
$GEMSTONE/jadeite/bin/installRowanStub.sh
```

