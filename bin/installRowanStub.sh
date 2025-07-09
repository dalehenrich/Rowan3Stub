set -ex

if [ $ROWAN_PROJECTS_HOME = "" ]; then
	echo "ERROR -- \$ROWAN_PROJECTS_HOME must be defined"
	exit 1
fi

stoneName=$1
registryName=$2;
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
stones_root=`registryQuery.solo -r $registryName --GsDevKit_stones_root`
cd $stones_root/$stoneName
if [ $extentType = "seaside" ]; then
	newExtent.solo -r $registryName -e product/bin/extent0.seaside.dbf $stoneName
elseif [ $extentType = "base" ]; then
	newExtent.solo -r $registryName -e product/bin/extent0.dbf $stoneName
else
	echo "existing extent in $stoneName will be updated with JfPwoR support"
fi

startNetldi.solo -r

source customenv # set $GEMSTONE

export ROWAN_STUB_EXTENT_TYPE=$extentType
$ROWAN_PROJECTS_HOME/RowanStubForJadeite/bin/installRowanStub.gs -I $topazini_systemuser -L
$ROWAN_PROJECTS_HOME/RowanStubForJadeite/bin/RowanClassService_base.gs -I $topazini_systemuser -L
