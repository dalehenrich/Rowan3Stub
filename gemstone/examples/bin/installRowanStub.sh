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
# extentType
#		base			- extent0.dbf
#		metacello	- extent0.seaside.dbf with monticello and metacello installed       [NOT YET SUPPORTED]
#		seaside 	- extent0.seaside.dbf with monticello installed                     [NOT YET SUPPORTED]
#		tode			-	extent0.seaside.dbf with monticello ,metacello and tODE installed [NOT YET SUPPORTED]
#

export ROWAN_STUB_EXTENT_TYPE=base
$GEMSTONE/examples/jadeite/bin/installRowanStub.gs -L 
$GEMSTONE/examples/jadeite/gs/RowanClassService_base.gs -L 
