
set -ex

export ROWAN_PROJECTS_HOME=/bosch1/users/dhenrich/_stones/37x/j_37x_externals_st/

extentType=$1
stoneName=$2
if [ "$extentType" = "" ]; then
  extentType="tode"       #                     - Monticello packages; tODE "projects" present
	#  extentType="metacello"  #                    - cannot use naked Metacello for managing projects (at the moment)
  extentType="seaside"    #	extent0.seaside.dbf - Monticello packages
  extentType="base"       #	extent0.dbf
fi
if [ "$stoneName" = "" ]; then
	echo "missing stone name (argument 2"
	exit 1
fi

echo "extentType=$extentType stoneName=$stoneName"

topazini_systemuser=".topazini_SU"
topazini_seaside=".topazini_DC"

if [ $extentType = "tode" ]; then
	newExtent.solo -r tode -e snapshots/extent0.10-28-2024_14:21:33_tode_virgin.dbf $stoneName
elif [ $extentType = "metacello" ]; then
	# need to run `GsUpgrade upgradeGrease` (see $l2tests/ernie/glass1tst.tpz for full details)
	echo "metacello extent type not currently supported"
	exit 1
elif [ $extentType = "seaside" ]; then
	# newExtent.solo -r tode -e snapshots/extent0.10-28-2024_14:21:33_tode_virgin.dbf $stoneName
	newExtent.solo -r 37x -e product/bin/extent0.seaside.dbf $stoneName
else
	newExtent.solo -r 37x -e product/bin/extent0.dbf $stoneName
fi

startNetldi.solo -r

source customenv # set $GEMSTONE

export ROWAN_STUB_EXTENT_TYPE=$extentType
$ROWAN_PROJECTS_HOME/Rowan3Stub/bin/installRowanStub.gs -I $topazini_systemuser -L

if [ $extentType = "seaside" ] || [ $extentType = "tode" ]; then
	$ROWAN_PROJECTS_HOME/Rowan3Stub/bin/RowanClassService_seaside.gs -I $topazini_seaside -L
	if [ $extentType = "tode" ]; then
		$ROWAN_PROJECTS_HOME/Rowan3Stub/bin/RowanClassService_tode.gs -I $topazini_seaside -L
	fi
else
	$ROWAN_PROJECTS_HOME/Rowan3Stub/bin/RowanClassService_base.gs -I $topazini_systemuser -L
fi
