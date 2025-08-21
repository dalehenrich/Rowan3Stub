# Install instructions for installing JfPwoR support in a stone
The env var GEMSTONE needs to be defined to point at GemStone 3.7.5 and $GEMSTONE/bin should be in your path.

The .topazini file (in the current directory) should be setup to login as SystemUser in your target stone. 

## Installation shell script for extent0.dbf
### topaz-based script for installing JfPwoR support in an existing extent0.dbf stone.
```
#
# A running stone based on extent0.dbf must exist. If a .topazini file for the stone 
# is not present in the current directory, then specify the path to the stone's 
# .topazini file on the command line as a positional argument to the script.
#
$GEMSTONE/examples/jadeite/bin/installRowanStub_topaz.sh [<.topazini-path>]
```
### topaz-based script for installing JfPwoR support in an existing extent0.dbf stone using GsDevKit_stones.
```
#
# Specify the <stone-name> and <registry-name> on the command line ... 
# if 'base' is specified, then the stone will be restarted with a fresh 
# extent0.dbf, otherwise the currently running stone will used
#
$GEMSTONE/examples/jadeite/bin/installRowanStub_stones.sh <stone-name> <registry-name> base
```
### Updating or Upgrading a stone after running installRowanStub_stones.sh or installRowanStub_topaz.sh
If any changes need to be made to the .gs files in $GEMSTONE/examples/jadeite/gs files
You can define the environment variable ROWAN_STUB_GS_DIRECTORY before running installRowanStub_*.sh 
and the referenced directory will be used instead of $GEMSTONE/examples/jadeite/gs and then either 
$GEMSTONE/examples/jadeite/bin/installRowanStub_stones.sh or 
$GEMSTONE/examples/jadeite/bin/installRowanStub_topaz.sh with the appropriate arguments and the files in
the alternate directory will be loaded.

To populate the alternate .gs directory, you can use the script $GEMSTONE/examples/jadeite/bin/generate_Rowan3Stub.sh.


