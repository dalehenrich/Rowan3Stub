### Scripts
```
#
## Create project and package
#
product/rowan3/bin/createRowanProject.solo --projectName=Rowan3Stub --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st --packageConvention=Rowan --repositoryType=git --packageFormat=tonel --defaultSymbolDictName=Globals
/bosch1/users/dhenrich/_stones/37x/stones/rowan3_dev_i/product/rowan3/bin/addRowanProjectPackages.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston  --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st Rowan3Stub-Core
#
## install Rowan3Stub project in Rowan3 image for development
#
installProject.stone file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st
#
## export project to .gs file
#
product/rowan3/bin/exportRowanProjectAsTopaz.solo file:/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/rowan/specs/Rowan3Stub.ston --projectsHome=/bosch1/users/dhenrich/_stones/37x/i_37x_externals_st /bosch1/users/dhenrich/_stones/37x/i_37x_externals_st/Rowan3Stub/gs/Rowan3Stub.gs

```
