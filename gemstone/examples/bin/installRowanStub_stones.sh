set -ex

source customenv # set $GEMSTONE

stoneName=$1
registryName=$2;
# extentType
#		base			- extent0.dbf
#		metacello	- extent0.seaside.dbf with monticello and metacello installed
#		seaside 	- extent0.seaside.dbf with monticello installed
#		tode			-	extent0.seaside.dbf with monticello ,metacello and tODE installed
extentType=$3
if [ "$stoneName" = "" ]; then
	echo "missing stone name (argument 1)"
	exit 1
fi
if [ "$registryName" = "" ]; then
	echo "missing registry name (argument 2)"
	exit 1
fi

echo "registryName=$registryName stoneName=$stoneName extentType=$extentType"
stones_root=`registryQuery.solo -r $registryName --stonesDirectory`
cd $stones_root/$stoneName
if [ $extentType = "seaside" ]; then
	newExtent.solo -r $registryName -e product/bin/extent0.seaside.dbf $stoneName
elif [ $extentType = "base" ]; then
	newExtent.solo -r $registryName -e product/bin/extent0.dbf $stoneName
else
	echo "existing extent in $stoneName will be updated with JfPwoR support"
fi

startNetldi.solo -r


export ROWAN_STUB_EXTENT_TYPE=$extentType
$GEMSTONE/examples/jadeite/bin/installRowanStub.gs -L
$GEMSTONE/examples/jadeite/gs/RowanClassService_base.gs -L
