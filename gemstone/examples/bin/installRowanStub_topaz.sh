set -ex
#
# To run:
#
# installRowanStub_topaz.sh [ <topaz-command-line-args> ]
#
# If no arguments specified on the script command line, then .topazini is assumed to 
#		exists in the current directory and the default topaz command line will be:
#
#		-L
#
# A minimal command line would be the following
#
#		 -L -I ./.topazini
#
# Additional topaz command line arguments are not required, but may be optionally specified
#

echo "installRowanStub_topaz.sh"
if [ "$GEMSTONE" = "" ]; then
	echo "ERROR: \$GEMSTONE env var expected to be set before running this script" 
	exit 1
fi

if [ "$#" -eq 0 ]; then
	if [ ! -f .topazini ]; then
		echo ".topazini for SystemUser was expected to be in the current directory"
		exit 1
	fi
	topazCommandLine="-L"
else
	topazCommandLine="$*"
fi

if [ "$ROWAN_STUB_BIN_DIRECTORY"x = "x" ]; then
	# if script overrides for $GEMSTONE/examples/jadeite/bin are needed, define ROWAN_STUB_BIN_DIRECTORY 
	# to point to alternate directory
	export ROWAN_STUB_BIN_DIRECTORY=$GEMSTONE/examples/jadeite/bin
fi
export ROWAN_STUB_EXTENT_TYPE=base
$ROWAN_STUB_BIN_DIRECTORY/installRowanStub.topaz $topazCommandLine
