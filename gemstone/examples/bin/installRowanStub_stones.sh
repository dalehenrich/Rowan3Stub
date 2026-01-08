set -ex
#
# To run:
#
# installRowanStub.sh <stone-name> <registry-name> base [ <topaz command line args> ]
#
# If no <topaz command line args> specified on the script command line, then .topazini 
#	is assumed to exists in the current directory and the default command line will be:
#
#		installRowanStub_stones.sh <stone-name> <registry-name> base -L
#
#  a minimal command line would be the following
#
#		installRowanStub_stones.sh base -L -I <path-to-.topazini-file>
#

if [ "$GEMSTONE" = "" ]; then
	echo "ERROR: \$GEMSTONE env var expected to be set" 
	exit 1
fi

stoneName=$1
shift
registryName=$1;
shift
# extentType
#		base			- extent0.dbf
#		metacello	- extent0.seaside.dbf with monticello and metacello installed				[not supported - yet]
#		seaside 	- extent0.seaside.dbf with monticello installed											[not supported - yet]
#		tode			-	extent0.seaside.dbf with monticello ,metacello and tODE installed	[not supported - yet]
extentType=$1
shift

if [ "$#" -eq 0 ]; then
	topazCommandLine="-L"
else
	topazCommandLine="$*"
fi

if [ "$stoneName" = "" ]; then
	echo "missing stone name (argument 1)"
	exit 1
fi
if [ "$registryName" = "" ]; then
	echo "missing registry name (argument 2)"
	exit 1
fi

echo "registryName=$registryName stoneName=$stoneName extentType=$extentType"

if [ $extentType = "base" ]; then
	newExtent.solo -r $registryName -e product/bin/extent0.dbf $stoneName
else
	echo "existing extent in $stoneName will be updated with JfPwoR support"
fi

export ROWAN_STUB_EXTENT_TYPE=base

$GEMSTONE/examples/jadeite/bin/installRowanStub.topaz $topazCommandLine
