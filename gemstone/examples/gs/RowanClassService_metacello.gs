#!/usr/bin/env superdoit_topaz
#
# to run as superdoit script with $GEMSTONE set :
#		./installRowanClassService_tode.gs -I <path-to-topazini> -L
# 
# as SystemUser
login

set INPUTPAUSEONERROR on

#
# overwrite of RowanProjectService method that will need to change for JfPwoR
# when run against Metacello image
#

category: 'Rowan3 stub'
method: RowanProjectService
changes
    | jadeServer projectNames loadedProject |
    jadeServer := Rowan jadeServerClassNamed: #'JadeServer'.
    changes := Array new.
    loadedProject := Rowan image loadedProjectNamed: self name.
    loadedProject packageNames do: [:packageName |
    	| loadedPackage wc |
      loadedPackage := loadedProject loadedPackageNamed: packageName.
      wc := loadedPackage workingCopy.
      wc modified
      	ifTrue: [
        	| patch |
          patch := wc changesRelativeToRepository: wc repositoryGroup repositories first.
          changes add:
          	(jadeServer new
            	_mcDescriptionOfPatch: patch
              baseName: 'closest ancestor'
              alternateName: nil
              packageName: packageName) ] ]
%
commit

## end of RowanClassService_metacello.gs
