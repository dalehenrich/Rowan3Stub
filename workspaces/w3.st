  | project repositorySpec repository |
  project := MetacelloMCProject new.
  repositorySpec := project repositorySpec.
  repositorySpec description: 'filetree:///bosch1/users/dhenrich/_stones/37x/j_37x_externals_st/Rowan3Stub/monticello'.
  repository := repositorySpec createRepository.
        repository isValid
                ifFalse: [ repository directory assureExistence ].
MCRepositoryGroup default addRepository: repository.
