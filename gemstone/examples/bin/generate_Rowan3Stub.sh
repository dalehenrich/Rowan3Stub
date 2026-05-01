# Copyright (C) GemTalk Systems 1986-2026.  All Rights Reserved.
#
# generate_Rowan3Stub.sh <target-directory> <project-directory> [ -D ]
#
#	The following git clones should be present in the directory specified as second argument:
#		RowanStubForJadeite
#		RemoteServiceReplication
#		RowanClientServicesV3
#		Announcements
#		RowanV3

set -ex

targetDir="$1"
shift

if [ "$targetDir" = "" ]; then
	echo "ERROR -- required positional argument specifying target directory for .gs files is missing"
	exit 1
fi

if [ ! -d "$targetDir" ]; then
	mkdir "$targetDir"
fi

projectDir="$1"
shift

if [ "$projectDir" = "" ]; then
	echo "ERROR -- \$projectDir must be defined"
	exit 1
fi

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
 	--projectsHome=$projectDir --fileName=$targetDir/RowanStubForJadeite.gs RowanStubForJadeite-Core --includeClassInitializers --includeRemoveAllMethods  $*

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
	--projectsHome=$projectDir --fileName=$targetDir/RowanStubForJadeiteServices.gs RowanStubForJadeite-Services --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_monticello.ston \
	--projectsHome=$projectDir --fileName=$targetDir/RowanStubForJadeiteMonticello.gs RowanStubForJadeite-Monticello --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_metacello.ston \
	--projectsHome=$projectDir --fileName=$targetDir/RowanStubForJadeiteMetacello.gs RowanStubForJadeite-Monticello RowanStubForJadeite-Metacello --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanStubForJadeite/rowan/specs/RowanStubForJadeite_base.ston \
		--projectsHome=$projectDir --fileName=$targetDir/RowanStubForJadeiteBase.gs RowanStubForJadeite-Base --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanPackagesAsTopaz.solo --loadSpec=file:$projectDir/RowanV3/rowan/specs/Rowan.ston \
	--projectsHome=$projectDir --fileName=$targetDir/GemStoneInteractions.gs GemStone-Interactions-Core GemStone-Interactions-Kernel --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanProjectAsTopaz.solo file:$projectDir/RemoteServiceReplication/rowan/specs/RemoteServiceReplication.ston \
	--projectsHome=$projectDir $targetDir/RemoteServiceReplication.gs --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanProjectAsTopaz.solo file:$projectDir/RowanClientServicesV3/rowan/specs/RowanClientServices.ston \
	--projectAlias=RowanClientServicesV3 --projectsHome=$projectDir $targetDir/RowanClientServicesV3.gs --includeClassInitializers --includeRemoveAllMethods $*

$projectDir/RowanV3/bin/exportRowanProjectAsTopaz.solo file:$projectDir/Announcements/rowan/specs/Announcements.ston \
	--projectsHome=$projectDir $targetDir/Announcements.gs --includeClassInitializers --includeRemoveAllMethods $*

# record the SHAs for each of the projects in a .gs file so that the correct SHAs can be displayed in Jadeite
/$projectDir/RowanStubForJadeite/bin/generateProjectSHAs.rw3_solo RowanStubForJadeite RowanV3 RemoteServiceReplication RowanClientServicesV3 Announcements \
	--projectsHome=$projectDir --targetDir=$targetDir

