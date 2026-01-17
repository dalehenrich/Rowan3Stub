set -ex
#
# To run:
#
# installRowanStub_topaz.sh [ <topaz-command-line-args> ]
#
# Define the environment varianble ROWAN_STUB_EXTENT_TYPE to one of base, metacello,
# seaside, or tode. If the environemnt variable is not defined, then base will be 
# used.
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

if [ "$ROWAN_STUB_EXTENT_TYPE" = "" ]; then
	export ROWAN_STUB_EXTENT_TYPE=base
fi

if [ "$ROWAN_STUB_BIN_DIRECTORY" = "" ]; then
	export ROWAN_STUB_BIN_DIRECTORY=$GEMSTONE/examples/jadeite/bin
	echo "using default ROWAN_STUB_BIN_DIRECTORY=$ROWAN_STUB_BIN_DIRECTORY"
fi

$ROWAN_STUB_BIN_DIRECTORY/installRowanStub.topaz $topazCommandLine
