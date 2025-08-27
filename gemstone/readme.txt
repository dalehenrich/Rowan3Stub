Install instructions for setup of a Jadeite For Pharo without Rowan (JfPwoR) environment.

This readme describes the scripts used to install server support components (a Rowan stub)
into a non-Rowan environment. This allow Jadeite for Pharo to login and do development in
a standard GemStone server. 

The Rowan Stub and Jadeite for Pharo are unreleased pre-alpha level open source projects, and
should be used with caution. Backup your repository prior to installing JfRwoR support.
Note that at this time, the JfPwoR environment does not directly support any code management
tools. You may file in and file out code. 

  https://github.com/GemTalk/JadeiteForPharo
  https://github.com/GemTalk/RowanStubForJadeite

-----------------------------------------------------------
To install JfPwoR into a standard GemStone extent, extent0.dbf
-----------------------------------------------------------

  The env var GEMSTONE needs to be defined to point at the GemStone installation directory, 
  and $GEMSTONE/bin should be in your path.

  A Stone must be running, based on extent0.dbf.  

  There must be a .topazini file available that is setup to login to the target stone as SystemUser.
  This can be in the directory from which the install script is executed, or specified on the install
  command line.

$GEMSTONE/examples/jadeite/bin/installRowanStub_topaz.sh [<.topazini-path>]

-----------------------------------------------------------
To install JfPwoR into an standard GemStone extent using GsDevKit_stones
-----------------------------------------------------------

  The env var GEMSTONE needs to be defined to point at the GemStone installation directory, 
  and $GEMSTONE/bin should be in your path.

  There must be a .topazini file in the current working directory that is setup to login to the target 
  stone as SystemUser.

  There are two command line arguments, <stone-name> and <registry-name>.  
  If 'base' is specified, then the stone will be restarted with a fresh extent0.dbf, otherwise the 
  currently running stone will used

$GEMSTONE/examples/jadeite/bin/installRowanStub_stones.sh <stone-name> <registry-name> base

-----------------------------------------------------------
To reinstall JfPwoR support  
-----------------------------------------------------------

  The above scripts normally do not allow reinstallation. You can enable reinstalltaion by
  setting the environment variable ROWAN_STUB_GS_DIRECTORY to a directory containing the
  JfPwoR support files, as are provided in $GEMSTONE/examples/jadeite/gs. 

  This is designed to allow distribution of updates for JfPwoR support without the need to 
  modify any files in the GemStone distribution. 

