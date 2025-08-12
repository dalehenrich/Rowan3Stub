# Install instructions for installing JfPwoR support in a stone
The env var GEMSTONE needs to be defined to point at GemStone 3.7.5 and $GEMSTONE/bin should be in your path.

### GsDevKit_stones-based script for installing JfPwoR support into a stone
```
#
# Specify the <stone-name> and <registry-name> on the command line ... 
# 	if 'base' is specified, then the stone will be restarted with a fresh 
# 	extent0.dbf, if 'existing' is used then no new stone will be created 
#		and JfPwoR sypport will be installed in the currently running stone.
#
# You may specify custom topaz command line options that will be used
#		to log into your stone, if no topaz options are supplied, `-L` will
#		be used and it will be assumed that the .topazini file for the stone
#		is located in the current directory
#
$GEMSTONE/jadeite/bin/installRowanStub_stones.sh <stone-name> <registry-name> \
                                                 [ base | existing ] \
                                                 [ -L -I <path-to-.topazini-file> ]
```
### topaz-based script for installing JfPwoR support in an existing extent0.dbf stone.
```
#
# You may specify custom topaz command line options that will be used
#		to log into your stone, if no topaz options are supplied, `-L` will
#		be used and it will be assumed that the .topazini file for the stone
#		is located in the current directory
#
$GEMSTONE/jadeite/bin/installRowanStub_topaz.sh [ -L -I <path-to-.topazini-file> ]
```

