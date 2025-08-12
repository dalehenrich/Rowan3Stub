set -ex
#
# To run:
#
# installRowanStub.sh base <topaz command line args>
#
# A minimal command line (where .topazini exists in the current directory) would like the following:
#
# installRowanStub.sh base -L
#
#  OR
#
# installRowanStub.sh base -L -I <path-to-.topazini-file>
# 
if [ "$iGEMSTONE" = "" ]; then
	echo "ERROR -- \$GEMSTONEmust be defined"
	exit 1
fi

if [ "$ROWAN_PROJECTS_HOME" = "" ]; then
	echo "ERROR -- \$ROWAN_PROJECTS_HOME must be defined"
	exit 1
fi

# extentType
#		base			- extent0.dbf
#		metacello	- extent0.seaside.dbf with monticello and metacello installed [NOT YET SUPPORTED]
#		seaside 	- extent0.seaside.dbf with monticello installed [NOT YET SUPPORTED]
#		tode			-	extent0.seaside.dbf with monticello ,metacello and tODE installed [NOT YET SUPPORTED]
#
extentType=$1
shift

if [ $extentType != "base" ]; then
	echo "ERROR -- extentType '$extentType' is not recognized"
fi

export ROWAN_STUB_EXTENT_TYPE=$extentType
$GEMSTONE/examples/jadeite/bin/installRowanStub.gs $*
$GEMSTONE/examples/jadeite/gs/RowanClassService_base.gs $*
